function promote_utype(t1::TypeOfUnknown, t2::TypeOfUnknown)
    return OpaqueType()
end


function compatible(s1::Nullable{Int64}, s2::Nullable{Int64})
    if isnull(s1) || isnull(s2)
        return true
    end

    s1 = get(s1)
    s2 = get(s2)

    s1 == s2 || s1 == 1 || s2 == 1
end


function broadcast(s1::Nullable{Int64}, s2::Nullable{Int64})
    if isnull(s1) || isnull(s2)
        return nothing
    end

    s1v = get(s1)
    s2v = get(s2)

    if s1v == s2v
        return s1
    end

    if s1v == 1
        return s2
    else
        return s1
    end
end


function promote_utype(atype1::ArrayType, atype2::ArrayType)
    shape1 = atype1.shape
    shape2 = atype2.shape

    if isnull(shape1) || isnull(shape2)
        res_shape = Nullable{Array{Nullable{Int64}, 1}}()
    else
        shape1 = get(shape1)
        shape2 = get(shape2)

        if length(shape1) != length(shape2)
            extend_dims = abs(length(shape1) - length(shape2))
            extend_shape = [Nullable{Int64}(1) for i in 1:extend_dims]

            if length(shape1) < length(shape2)
                shape1 = [shape1..., extend_shape...]
            else
                shape2 = [shape2..., extend_shape...]
            end
        end

        @assert all(compatible(s1, s2) for (s1, s2) in zip(shape1, shape2))
        res_shape = map((x) -> broadcast(x[1], x[2]), zip(shape1, shape2))
        res_shape = Nullable{Array{Nullable{Int64}, 1}}(res_shape)
    end

    eltype1 = atype1.eltype
    eltype2 = atype2.eltype

    if isnull(eltype1) || isnull(eltype2)
        res_eltype = Nullable{DataType}()
    else
        res_eltype = Nullable{DataType}(promote_type(get(eltype1), get(eltype2)))
    end

    ArrayType(res_shape, res_eltype)
end


# Some machinery for intercepting broadcast with `ArrayExpr` objects.
# Usage of e.g. `.*` transforms into a broadcast call with an anonymous function with `*` inside.
# Since we would like to distinguish between `.*` and `*` in array expressions,
# we wrap `ArrayExpr` objects into a special object, add arithmetic methods for it,
# call the anonymous function, and unwrap it back.

struct Broadcasted{T <: ArrayExpr}
    val::T
end

to_bc(x::Broadcasted) = x
to_bc(x) = Broadcasted(x)

from_bc(x::Broadcasted) = x.val

to_aexpr(x::ArrayExpr) = x
to_aexpr(x) = convert(ArrayExpr, x)

call_node(op, a, b) = CallNode(op, [a, b], promote_utype(expr_type(a), expr_type(b)))

for op in (:+, :-, :*, :/, :^)
    @eval begin
        Base.$op(a::Broadcasted, b::Broadcasted) =
            to_bc(call_node(Symbol(".", $op), from_bc(a), from_bc(b)))
        Base.$op(a::Broadcasted, b) = $op(a, to_bc(to_aexpr(b)))
        Base.$op(a, b::Broadcasted) = $op(to_bc(to_aexpr(a)), b)
    end
end

Base.Broadcast.containertype(::ArrayExpr) = ArrayExpr
Base.Broadcast.promote_containertype(::Type{ArrayExpr}, ::Type{ArrayExpr}) = ArrayExpr
# We would want to only handle the numbers and arrays of numbers here.
# But currently:
# - `containertype()` of a scalar type results in `Any`, so we cannot add a
#   `promote_containertype()` method for `ArrayExpr` and `Number` - it just will not be called.
# - there is already a `promote_containertype()` for `Any` and `Array` which would
#   intercept anything more specialized than the methods below.
# That's why constructions like e.g. `[x, y] .== x` where `x` is of some `ArrayExpr` subtype
# will fall into our custom broadcast and fail miserably.
Base.Broadcast.promote_containertype(::Type{ArrayExpr}, ::Type{Any}) = ArrayExpr
Base.Broadcast.promote_containertype(::Type{Any}, ::Type{ArrayExpr}) = ArrayExpr
Base.Broadcast.promote_containertype(::Type{ArrayExpr}, ::Type{Array}) = ArrayExpr
Base.Broadcast.promote_containertype(::Type{Array}, ::Type{ArrayExpr}) = ArrayExpr

# Custom broadcast on our container type
function Base.Broadcast.broadcast_c(f, ::Type{ArrayExpr}, as...)
    from_bc(f(to_bc.(to_aexpr.(as))...))
end


for op in (:+, :-, :*, :/, :^)
    @eval begin
        Base.$op(x::ArrayExpr, y::ArrayExpr) = call_node(Symbol($op), x, y)
    end

    for ext_type in (Number, AbstractArray)
        @eval begin
            Base.$op(x::ArrayExpr, y::$ext_type) = ($op)(x, to_aexpr(y))
            Base.$op(x::$ext_type, y::ArrayExpr) = ($op)(to_aexpr(x), y)
        end
    end
end


import Base: size, reshape, permutedims, repeat

function size_expr(arr::ArrayExpr)
    CallNode(:size, [arr], ArrayType([1], Int64))
end

function size_expr(arr::ArrayExpr, dim::ArrayExpr)
    CallNode(:size, [arr, dim], ArrayType([], Int64))
end


function size(arr::ArrayExpr)
    atype = expr_type(arr)
    if !isnull(atype.shape)
        shape = get(atype.shape)

        if !any(map(isnull, shape))
            shape = map(get, shape)
            return tuple(shape...)
        end
    end

    size_expr(arr)
end

function size(arr::ArrayExpr, dim)
    atype = expr_type(arr)
    if !isnull(atype.shape)
        shape = get(atype.shape)
        if !isnull(shape[dim])
            return get(shape[dim])
        end
    end
    size_expr(arr, to_aexpr(dim))
end

function size(arr, dim::ArrayExpr)
    size_expr(to_aexpr(arr), dim)
end


function reshape(arr::ArrayExpr, dims::ArrayExpr)
    CallNode(:reshape, [arr, dims], ArrayType(nothing, expr_type(arr).eltype))
end


normalize_dims(dim::Number) = [dim]
normalize_dims(dim::AbstractArray) = dim
normalize_dims{T <: Number,N}(dim::Tuple{Vararg{T,N}}) = collect(dim)
normalize_dims(dims...) = collect(dims)

function reshape(arr::ArrayExpr, dims...)
    dims = normalize_dims(dims)
    atype = expr_type(arr)

    if !isnull(atype.shape)
        shape = get(atype.shape)
        if !any(map(isnull, shape))
            shape = map(get, shape)
            @assert prod(shape) == prod(dims)
        end
    end

    CallNode(:reshape, [arr, to_aexpr(dims)], ArrayType(dims, atype.eltype))
end

function reshape(arr, dims::ArrayExpr)
    CallNode(:reshape, [to_aexpr(arr), dims], ArrayType(nothing, atype.eltype))
end


function permutedims(arr::ArrayExpr, perm)
    perm = collect(perm)
    @assert ndims(perm) == 1
    atype = expr_type(arr)

    if !isnull(atype.shape)
        shape = get(atype.shape)
        @assert length(shape) == length(perm)
        new_shape = shape[perm]
    else
        new_shape = nothing
    end

    CallNode(:permutedims, [arr, to_aexpr(perm)], ArrayType(new_shape, atype.eltype))
end

function permutedims(arr, perm::ArrayExpr)
    CallNode(:permutedims, [to_aexpr(arr), perm], ArrayType(nothing, atype.eltype))
end


normalize_outer(rep::Number) = [rep]
normalize_outer(rep::AbstractArray) = rep
normalize_outer{T <: Number,N}(rep::Tuple{Vararg{T,N}}) = collect(rep)

function repeat(arr::ArrayExpr; outer=nothing)
    if outer === nothing
        return arr
    end

    outer = normalize_outer(outer)
    atype = expr_type(arr)

    if !isnull(atype.shape)
        shape = get(atype.shape)
        @assert length(shape) == length(outer)

        new_shape = [isnull(x) ? nothing : get(x) * rep for (x, rep) in zip(shape, outer)]
    else
        new_shape = nothing
    end

    CallNode(:repeat_outer, [arr, to_aexpr(outer)], ArrayType(new_shape, atype.eltype))
end


for builtin_function in (:cosh, :fft, :ifft, :abs, :sqrt, :sum)
    @eval begin
        import Base: $builtin_function

        function $builtin_function(arg::ArrayExpr, args...)
            # This check is not necessary, [x, args...] should work anyway,
            # but it currently doesn't because of bug #17003
            if length(args) > 0
                args = [convert(ArrayExpr, arg) for arg in args]
                args = [arg, args...]
            else
                args = [arg]
            end
            CallNode($(Meta.quot(builtin_function)), args, expr_type(arg))
        end
    end
end


function unpack(aexpr::ArrayExpr)
    etype = expr_type(aexpr)
    if typeof(etype) != TupleType
        return aexpr
    else
        return tuple((
            CallNode(:unpack, [aexpr, convert(ArrayExpr, i)], typ)
            for (i, typ) in enumerate(etype.elements))...)
    end
end


as_aexpr(val) = convert(ArrayExpr, val)


function random_uniform(state::ArrayExpr, min, max, dims...)
    atype = ArrayType(collect(dims), Float64)
    dims = map(as_aexpr, dims)
    arr, new_state = unpack(
        CallNode(:random_uniform, [state, dims...], TupleType([atype, RandomStateType()])))
    arr .* (max .- min) .+ min, new_state
end


function random_normal(state::ArrayExpr, mean, std, dims...)
    atype = ArrayType(collect(dims), Float64)
    dims = map(as_aexpr, dims)
    arr, new_state = unpack(
        CallNode(:random_normal, [state, dims...], TupleType([atype, RandomStateType()])))
    arr .* std .+ mean, new_state
end
