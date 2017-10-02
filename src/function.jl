using FunctionalCollections


immutable ArrayFunction
    expressions :: Dict{Unknown, ArrayExpr}
    parameters :: Array{Unknown, 1}
    returns :: Array{Unknown, 1}
    mutates :: Dict{Unknown, Unknown}
end


function all_nonleaf_nodes(aexpr)
    if is_leaf(aexpr)
        []
    else
        vcat([aexpr], map(all_nonleaf_nodes, children(aexpr))...)
    end
end


function build_parent_map(aexprs)
    # Only including non-leaves here!
    # We don't need to know leaves' parents, we won't replace them by aliases anyway.
    parents = Dict{ArrayExpr, Array{ArrayExpr, 1}}()
    for aexpr in aexprs
        for node in all_nonleaf_nodes(aexpr)
            for child in children(node)
                if is_node(child)
                    # There may be identical nodes that will be removed at merge stage
                    # For now we treat them separately
                    if !haskey(parents, child)
                        parents[child] = [aexpr]
                    else
                        push!(parents[child], aexpr)
                    end
                end
            end
        end

        # Add the top node to the mapping
        parents[aexpr] = []
    end
    parents
end


function merge(graph)
    parents = graph.parents # obj_id => [node]

    # Mark the nodes that can be merged
    original_nodes = Dict() # node => node
    to_merge = Dict() # obj_id => node
    for node in depth_first(graph.expressions)

        # Construct a replacement node, so that we could compare it with original ones
        # and decide right away whether it will require merging as well.
        # Since it's a depth-first search, we've already processed all children.
        node_children = children(node)
        child_ids = map(object_id, node_children)
        to_replace = Dict(
            child => original_nodes[to_merge[object_id(child)]]
            for child in node_children
            if object_id(child) in to_merge)
        new_node = replace_expr(node, to_replace)

        if new_node in original_nodes
            # `new_node` is equal to the original node, but it's a different object.
            # We need the original object.
            to_merge[object_id(node)] = original_nodes[new_node]
        else
            original_nodes[node] = node
        end
    end

    # Replace the marked nodes
    for (node, replacement) in reverse(to_merge)

    end
end


function array_function(returns, parameters, mutates=[])
    # Ideally, we can do the following backend-independent actions before generating code:
    # - merge expressions, so that we don't calculate the same thing twice
    # - for every node, calculate the number of nodes it is used in
    #   (those that are used in more than one node will have to be assigned to temporary vars)

    return_aliases = [Unknown(expr_type(expr)) for expr in returns]

    mutate_aliases = [Unknown(expr_type(expr)) for (expr, target) in mutates]
    mutate_targets = [target for (expr, target) in mutates]
    mutate_expressions = [expr for (expr, target) in mutates]
    mutate_map = Dict(
        alias => target
        for (alias, target) in zip(mutate_aliases, mutate_targets))

    aliases = vcat(return_aliases, mutate_aliases)
    expressions = vcat(returns, mutate_expressions)
    expression_map = Dict(
        alias => expr
        for (alias, expr) in zip(aliases, expressions))

    ArrayFunction(expression_map, parameters, return_aliases, mutate_map)
end


function (afunc::ArrayFunction)(args...)
    @assert length(afunc.mutates) == 0 # For now, only pure functions can be nested
    replacements = Dict(param => arg for (param, arg) in zip(afunc.parameters, args))
    returns = [replace_expr(afunc.expressions[ret], replacements)[2] for ret in afunc.returns]
    tuple(returns...)
end


function _replace_expr(
        used_aliases::PersistentSet{Unknown}, aexpr::ArrayExpr, replacements,
        # `true` on first call, so that we do not replace an expression that already has an alias,
        # but only replace its subexpressions
        is_root_expr)
    if !is_root_expr && haskey(replacements, aexpr)
        alias = replacements[aexpr]
        used_aliases = conj(used_aliases, alias)
        return used_aliases, convert(ArrayExpr, alias)
    elseif is_node(aexpr)
        map_accum(used_aliases, aexpr) do ua, arg
            _replace_expr(ua, arg, replacements, false)
        end
    else
        return used_aliases, aexpr
    end
end

function replace_expr(aexpr::ArrayExpr, replacements)
    used_aliases, new_expr = _replace_expr(
        PersistentSet{Unknown}(), aexpr, replacements, true)
    Set(used_aliases), new_expr
end


function separate(afunc)

    parents = build_parent_map([expr for (alias, expr) in afunc.expressions])

    mp_nodes_to_aliases = Dict(
        (node => Unknown(expr_type(node)))
        for (node, parent_nodes) in parents
        if length(parent_nodes) > 1)

    mp_aliases_to_nodes = map(reverse, mp_nodes_to_aliases)

    res = Base.merge(afunc.expressions, mp_aliases_to_nodes)

    aliases = Dict(
        (alias => replace_expr(expr, mp_nodes_to_aliases))
        for (alias, expr) in res)

    aliases
end


function serialize(serialized_aliases, alias, aliases_map)

    used_aliases, expr = aliases_map[alias]

    serialized = []
    for used_alias in used_aliases
        if !in(used_alias, serialized_aliases)
            serialized_aliases, l = serialize(serialized_aliases, used_alias, aliases_map)
            append!(serialized, l)
        end
    end

    serialized_aliases = conj(serialized_aliases, alias)
    push!(serialized, (alias => expr))

    serialized_aliases, serialized
end


function serialize(afunc)
# returns:
# - a list of pairs `(Unknown => ArrayExpr)` denoting "assignments" or "aliases".
#   The LHS arrays can appear in other expressions later in the list.
# These two are already contained in `afunc`:
# - a list of `Unknown` objects corresponding to return values
#   (each is either one of the parameters or one of the introduced aliases)
# - a dict `(Unknown => Unknown)`, where RHS is either a parameter or an alias,
#   and LHS is a parameter to which the LHS will be stored
# `Unknown` can be an array or a random state
    aliases_map = separate(afunc)

    serialized_aliases = PersistentSet{Unknown}()
    serialized = []
    for alias in keys(afunc.expressions)
        serialized_aliases, l = serialize(serialized_aliases, alias, aliases_map)
        append!(serialized, l)
    end

    return serialized
end


function mutated_parameters_indices(afunc::ArrayFunction)
    Set(find(
        # Cannot use `afunc.parameters .== target` instead, because the broadcasting
        # cannot be redirected into default functions in this case.
        # See the broadcasting methods in operators.jl.
        [param == target for param in afunc.parameters][1]
        for target in values(afunc.mutates)))
end
