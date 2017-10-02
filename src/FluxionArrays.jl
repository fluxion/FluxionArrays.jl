__precompile__()

module FluxionArrays

include("types.jl")
export Unknown
export ArrayExpr

include("show.jl")

include("constructors.jl")
export lazy_array
export unknown_array
export unknown_random_state

include("operators.jl")
export random_uniform
export random_normal

include("function.jl")
export array_function

include("backend.jl")
export ArrayBackend
export BackendArray
export BackendArrayFunction
export compile_array_function
export random_state
export default_array_backend
export to_backend
export from_backend

include("backend_julia.jl")
export JuliaBackend

end
