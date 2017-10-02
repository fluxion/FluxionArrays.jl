import Base: show


function show(io::IO, x::RandomStateType)
    print(io, "RandomStateType")
end


function show(io::IO, x::OpaqueType)
    print(io, "OpaqueType")
end


function show(io::IO, x::ArrayType)
    et = x.eltype
    sh = x.shape

    if isnull(et)
        print(io, "unknown(")
    else
        print(io, "$(get(et))(")
    end

    if isnull(sh)
        print(io, "unknown")
    else
        labels = map(dim -> isnull(dim) ? "unknown" : string(get(dim)), get(sh))
        join(io, labels, ", ")
    end

    print(io, ")")
end


function show(io::IO, x::Unknown)
    print(io, "U[$(x.name), $(x.typ)]")
end


function show(io::IO, x::LazyArray)
    typ = array_typeof(x.value)
    print(io, "KA[$typ]")
end


function show(io::IO, aexpr::CallNode)
    print(io, "(")
    show(io, aexpr.func)
    print(io, ", ")

    args = aexpr.args
    for (i, arg) in enumerate(args)
        show(io, arg)
        if i != length(args)
            print(io, ", ")
        end
    end
    print(io, ")")
end
