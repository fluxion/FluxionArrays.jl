import Base: convert


convert(::Type{ArrayExpr}, val::Number) = LazyArray(val)
convert(::Type{ArrayExpr}, val::AbstractArray) = LazyArray(val)
convert(::Type{ArrayExpr}, val::ArrayExpr) = val

"""
Converts a Julia array into an object that can be used in array expressions.
This happens automatically when an array is used in an expression with an `ArrayExpr` object,
but it can be useful to wrap it explicitly to delay the computation.
"""
lazy_array(val::Number) = convert(ArrayExpr, val)
lazy_array(val::AbstractArray) = convert(ArrayExpr, val)


function unknown_array(; shape=nothing, eltype=nothing)
    Unknown(ArrayType(shape, eltype))
end


unknown_random_state() = Unknown(RandomStateType())

