abstract type ArrayBackend end

abstract type BackendArrayFunction end
abstract type BackendValue <: ArrayExpr end
abstract type BackendArray <: BackendValue end
abstract type BackendRandomState <: BackendValue end


default_array_backend() = JuliaBackend()


compile_array_function(afunc::ArrayFunction) =
    compile_array_function(default_array_backend(), afunc)
random_state(; seed=nothing) = random_state(default_array_backend(), seed=seed)
empty_array(dtype, shape...) = empty_array(default_array_backend(), dtype, shape...)
to_backend(arr::AbstractArray) = to_backend(default_array_backend(), arr)
from_backend(arr::BackendArray) = error("from_backend() not implemented for $(typeof(arr))")

expr_type(arr::BackendArray) = ArrayType([Nullable{Int64}(x) for x in size(arr)], eltype(arr))

#to_backend(backend, serialized_rs::SerializedRandomState)
#from_backend(rs::BackendSideRandomState)
