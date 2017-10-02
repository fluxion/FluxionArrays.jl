abstract type TypeOfUnknown end


struct RandomStateType <: TypeOfUnknown end


struct TupleType <: TypeOfUnknown
    elements :: Array{TypeOfUnknown, 1}
end


struct ArrayType <: TypeOfUnknown
    shape :: Nullable{Array{Nullable{Int64}, 1}}
    eltype :: Nullable{DataType}
end


struct OpaqueType <: TypeOfUnknown
end


array_typeof(arr::Number) = ArrayType([], eltype(arr))
array_typeof(arr::AbstractArray) = ArrayType(collect(size(arr)), eltype(arr))


abstract type ArrayExpr end


struct Unknown <: ArrayExpr
    typ :: TypeOfUnknown

    # Technically, this object does not need a name, but:
    # 1. It makes debugging easier
    # 2. Julia automatically caches objects, so, say, two `Unknown(OpaqueType())`
    #    created seaparetly would have the same object ID, which would ruin the whole
    #    expression graph processing.
    name :: Symbol

    Unknown(typ; tag=nothing) = new(typ, tag === nothing ? gensym() : gensym(tag))
end


struct LazyArray <: ArrayExpr
    value :: Any
end


struct CallNode <: ArrayExpr
    func :: Symbol
    args :: Array{ArrayExpr, 1}
    typ :: TypeOfUnknown
end


map_accum(func, state, aexpr) = func(state, aexpr)

function map_accum(func, state, aexpr::CallNode)
    new_args = []
    for arg in aexpr.args
        state, new_arg = func(state, arg)
        push!(new_args, new_arg)
    end
    state, CallNode(aexpr.func, new_args, aexpr.typ)
end


expr_type(aexpr::Unknown) = aexpr.typ
expr_type(aexpr::LazyArray) = array_typeof(aexpr.value)
expr_type(aexpr::CallNode) = aexpr.typ

is_leaf(aexpr::ArrayExpr) = true
is_leaf(aexpr::CallNode) = false

is_node(aexpr::ArrayExpr) = false
is_node(aexpr::CallNode) = true

children(aexpr::ArrayExpr) = []
children(aexpr::CallNode) = aexpr.args
