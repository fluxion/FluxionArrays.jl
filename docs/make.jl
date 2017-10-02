using Documenter
using FluxionArrays


makedocs(
    modules = [FluxionArrays],
    format = :html,
    sitename = "FluxionArrays.jl",
    authors = "Bogdan Opanchuk",
    pages = [
        "Home" => "index.md",
    ],
)

deploydocs(
    repo = "github.com/fluxion/FluxionArrays.jl.git",
    target = "build",
    deps = nothing,
    make = nothing,
)
