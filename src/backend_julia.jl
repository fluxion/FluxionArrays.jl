using FunctionalCollections


struct JuliaBackend <: ArrayBackend
end


const ObjectID = UInt64


function reify_value(val::Complex)
    if real(val) == 0
        :( $(imag(val)) * im ), Nullable{Symbol}()
    elseif imag(val) == 0
        real(val), Nullable{Symbol}()
    else
        :( $(real(val)) + $(imag(val)) * im ), Nullable{Symbol}()
    end
end


function reify_value(val::AbstractArray)
    if ndims(val) == 0
        return val[1], Nullable{Symbol}()
    else
        name = gensym()
        ast_node = Symbol(name)
        return ast_node, Nullable{Symbol}(ast_node)
    end
end


function reify_value(val::Number)
    val, Nullable{Symbol}()
end


function reify_value(val)
    name = gensym()
    ast_node = Symbol(name)
    return ast_node, Nullable{Symbol}(ast_node)
end


function reify_leaf(identifiers, val)

    val_id = object_id(val)
    if haskey(identifiers, val_id)
        ast_node, _ = identifiers[val_id]
    else
        ast_node, name = reify_value(val)
        if !isnull(name)
            identifiers = assoc(identifiers, val_id, get(name) => val)
        end
    end

    identifiers, ast_node
end


function reify(identifiers, aexpr::LazyArray)
    reify_leaf(identifiers, aexpr.value)
end


function reify(identifiers, aexpr::BackendArray)
    reify_leaf(identifiers, from_backend(aexpr))
end


function reify(identifiers, aexpr::Unknown)
    reify_leaf(identifiers, aexpr)
end


function unpack(x, i)
    x[i]
end


function repeat_outer(x, outer)
    repeat(x, outer=outer)
end

# Julia's reshape only accepts tuples
function reshape(x::AbstractArray, dims::AbstractArray)
    reshape(x, dims...)
end

# Zero-dimensional arrays are reified as scalars, and Julia doesn't have a reshape() method
# for scalars, so we have to provide our own.
function reshape(x::Number, dims...)
    reshape([x], dims...)
end


function random_uniform(state, dims...)
    state_copy = copy(state)
    res = rand(state_copy, Float64, dims...)
    res, state_copy
end


function random_normal(state, dims...)
    state_copy = copy(state)
    res = randn(state_copy, dims...)
    res, state_copy
end


function reify(identifiers, aexpr::CallNode)
    head = aexpr.func
    args = aexpr.args
    identifiers, arg_asts = reify(identifiers, args)
    identifiers, :( $head($(arg_asts...)) )
end


function reify{T}(identifiers, arr::Array{T, 1})
    result = Array{Any, 1}(length(arr))
    for (i, elem) in enumerate(arr)
        identifiers, elem_ast = reify(identifiers, elem)
        result[i] = elem_ast
    end
    identifiers, result
end


function reify{K, V}(identifiers, pair::Pair{K, V})
    fst, snd = pair
    identifiers, fst = reify(identifiers, fst)
    identifiers, snd = reify(identifiers, snd)
    identifiers, (fst => snd)
end


function reify{K, V}(identifiers, dict::Dict{K, V})
    result = Dict{Any, Any}()
    for (i, pair) in enumerate(dict)
        identifiers, pair_ast = reify(identifiers, pair)
        k, v = pair_ast
        result[k] = v
    end
    identifiers, result
end


struct JuliaArrayFunction <: BackendArrayFunction
    func :: Function
    known_arguments :: Array{AbstractArray, 1}
    mutated_parameters_indices :: Set{Int64}
end


function (cafunc::JuliaArrayFunction)(args...)

    # The parameters being mutated must be backend arrays.
    # Technically, this check should be done in a compiled array function in any backend
    if length(cafunc.mutated_parameters_indices) > 0
        for (i, arg) in enumerate(args)
            if in(i, cafunc.mutated_parameters_indices)
                @assert isa(arg, BackendValue) && array_backend(arg) == JuliaBackend()
            end
        end
    end

    unwrapped_args = map(unwrap, args)

    # Cannot call `cafunc.func` directly because of Julia's restrictions
    # on dynamic method definition.
    # See "applicable method may be too new" error, and
    # https://docs.julialang.org/en/latest/manual/methods.html#Redefining-Methods-1
    results = Base.invokelatest(cafunc.func, cafunc.known_arguments..., unwrapped_args...)

    wrapped_results = map(wrap, results)
    length(wrapped_results) == 1 ? wrapped_results[1] : wrapped_results
end


function compile_array_function(::JuliaBackend, afunc::ArrayFunction)

    serialized_aliases = serialize(afunc)

    # Replace known/unknown arrays in the expressions to be returned with temporary identifiers
    body_identifiers = PersistentHashMap{ObjectID, Any}()
    body_identifiers, reified_aliases = reify(body_identifiers, serialized_aliases)
    identifiers, reified_parameters = reify(body_identifiers, afunc.parameters)
    identifiers, reified_mutates = reify(identifiers, afunc.mutates)
    identifiers, reified_returns = reify(identifiers, afunc.returns)

    # At this point we do not need object IDs anymore,
    # a mapping from symbols to values will suffice
    body_identifiers = Dict(pair[2] for pair in body_identifiers)
    identifiers = Dict(pair[2] for pair in identifiers)

    # Since we cannot create a context for the compiled function,
    # known arrays will have to be passed explicitly
    known_parameter_map = filter((sym, val) -> !isa(val, Unknown), identifiers)
    # According to docs, `keys` and `values` will have the same order
    known_parameter_symbols = collect(keys(known_parameter_map))
    known_arguments = collect(values(known_parameter_map))

    # Identify unused parameters and replace them with placeholders
    used_in_expressions = Set(
        filter(unknown_sym -> haskey(body_identifiers, unknown_sym), reified_parameters))
    used_in_mutations = Set(values(reified_mutates))
    used_unknown_parameters = union(used_in_expressions, used_in_mutations)
    unknown_parameter_symbols = [
        in(unknown_sym, used_unknown_parameters) ? unknown_sym : :_
        for unknown_sym in reified_parameters]

    function_name = gensym("array_function")

    # Construct the function
    expr = quote
        function $function_name(
                $(known_parameter_symbols...),
                $(unknown_parameter_symbols...))

            # Assignment to aliases
            #$(((if length(targets) == 1
            #        :( $(targets[1]) = $expr )
            #    else
            #        :( ($(targets...),) = $expr )
            #    end)
            #    for (targets, expr) in reified_aliases)...)
            $((:( const $target = $expr )
                for (target, expr) in reified_aliases)...)

            # Copying to mutated parameters
            $((:( copy!($target, $alias) )
                for (alias, target) in reified_mutates)...)

            # Returns
            $(if length(reified_returns) > 0
                :( ($(reified_returns...),) )
            end)
        end
    end

    #println("Function to compile:")
    #println("-----------------------------------")
    #println(expr)
    #println("-----------------------------------")

    func = eval(expr)

    JuliaArrayFunction(func, map(unwrap, known_arguments), mutated_parameters_indices(afunc))
end


struct JuliaBackendRandomState <: BackendRandomState
    random_state :: MersenneTwister
end

struct JuliaBackendArray <: BackendArray
    array :: AbstractArray
end


import Base: size, ndims, eltype

eltype(arr::JuliaBackendArray) = eltype(arr.array)
size(arr::JuliaBackendArray) = size(arr.array)
size(arr::JuliaBackendArray, dim) = size(arr.array, dim)
ndims(arr::JuliaBackendArray) = ndims(arr.array)

array_backend(::JuliaBackendRandomState) = JuliaBackend()
array_backend(::JuliaBackendArray) = JuliaBackend()
array_backend(::JuliaArrayFunction) = JuliaBackend()

unwrap(value::Number) = value
unwrap(value::AbstractArray) = value
unwrap(value::JuliaBackendRandomState) = value.random_state
unwrap(value::JuliaBackendArray) = value.array
unwrap(value::LazyArray) = value.value

wrap(value::Number) = value
wrap(value::AbstractArray) = JuliaBackendArray(value)
wrap(value::MersenneTwister) = JuliaBackendRandomState(value)


function random_state(::JuliaBackend; seed=nothing)
    if seed === nothing
        JuliaBackendRandomState(MersenneTwister())
    else
        JuliaBackendRandomState(MersenneTwister(seed))
    end
end


empty_array(::JuliaBackend, dtype, shape...) = JuliaBackendArray(Array(dtype, shape...))

to_backend(::JuliaBackend, arr::JuliaBackendArray) = arr
to_backend(::JuliaBackend, arr::AbstractArray) = JuliaBackendArray(arr)
from_backend(arr::JuliaBackendArray) = arr.array
