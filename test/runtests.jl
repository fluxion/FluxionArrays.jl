using Base.Test

using BenchmarkTools

using FluxionArrays


expr_type = FluxionArrays.expr_type


@testset "Expression building" begin
    s = 2
    data = rand(10)
    data2 = rand(10, 20)
    x = unknown_array()
    y = unknown_array()
    z = unknown_array()
    a = lazy_array(data)
    b = lazy_array(data2)

    func = x -> x.func

    @test func(x + y) == :+
    @test func(x .+ y) == :.+
    @test func(x + s) == :+
    @test func(x .+ s) == :.+
    @test func(s + x) == :+
    @test func(s .+ x) == :.+
    @test func(x + data) == :+
    @test func(x .+ data) == :.+
    @test func(x + y + z) == :+
    @test func(x .+ y .+ z) == :.+
    @test func(s .+ data2 .+ z) == :.+

    # The calls that involve a literal will be fused
    # into a broadcast of an anonymous function by default.
    # We check that the library builds a proper expression instead.
    @test func(x + 1) == :+
    @test func(x .+ 1) == :.+
    @test func(x + [1 2 3]) == :+
    @test func(x .+ [1 2 3]) == :.+
    @test func(x + y + 1) == :+
    @test func(x .+ y .+ 1) == :.+
    @test func(x + 1 + y) == :+
    @test func(x .+ 1 .+ y) == :.+
    @test func(1 + x + y) == :+
    @test func(1 .+ x .+ y) == :.+
    @test func(z .+ 1 .+ x .+ y) == :.+
    @test func(1 .+ [1 2 3] .+ x) == :.+

end


@testset "Type propagation" begin

    data = rand(10)
    data2 = rand(10, 20)
    x = unknown_array()
    y = unknown_array()
    a = lazy_array(data)
    b = lazy_array(data2)

    @test expr_type(x .+ y) == FluxionArrays.ArrayType(nothing, nothing)

    x = unknown_array(shape=[10, 20, 30])
    y = unknown_array(shape=[10, 20])

    @test size(permutedims(reshape(x .+ y .+ 1, 100, 60), [2, 1])) == (60, 100)
end


@testset "Timings" begin

    data = rand(1000000)
    data2 = rand(1000000)
    x = unknown_array()
    y = unknown_array()
    a = lazy_array(data)
    b = lazy_array(data2)

    x_data = rand(1000000)
    y_data = rand(1000000)

    c = a .+ 1 .+ x
    d = 2 .+ b .+ y
    e = c .+ d .+ size(x, 1) .+ reshape(x, size(x, 1))

    function ref(data, data2, x, y)
        (data .+ 1 .+ x) .+ (2 .+ data2 .+ y) .+ size(x, 1) .+ reshape(x, size(x, 1))
    end

    println("Timing compilation of an array expression")
    display(@benchmark compile_array_function(array_function([$e], [$x, $y])))
    println()
    f = compile_array_function(array_function([e], [x, y]))

    println("Timing the reference function")
    display(@benchmark $ref($data, $data2, $x_data, $y_data))
    println()

    println("Timing the compiled function")
    display(@benchmark $f($x_data, $y_data))
    println()

    res1 = from_backend(f(x_data, y_data))
    res2 = ref(data, data2, x_data, y_data)

    @test isapprox(res1, res2)

end

@testset "Multiple returns" begin
    data = rand(100)
    data2 = rand(100)
    x = unknown_array()
    y = unknown_array()
    a = lazy_array(data)
    b = lazy_array(data2)

    x_data = rand(100)
    y_data = rand(100)

    f = compile_array_function(array_function([a + 1 + x, 2 + b + y], [x, y]))
    res1, res2 = f(x_data, y_data)

    @test isapprox(from_backend(res1), data .+ 1 .+ x_data)
    @test isapprox(from_backend(res2), 2 .+ data2 .+ y_data)
end


@testset "Mutations" begin

    data = rand(100)
    data2 = rand(100)
    x = unknown_array()
    y = unknown_array()
    a = lazy_array(data)
    b = lazy_array(data2)

    x_data = rand(100)
    x_data_copy = copy(x_data)
    y_data = rand(100)

    x_data_backend = to_backend(x_data)

    f = compile_array_function(array_function([a + 1 + x], [x, y], [2 + b + y => x]))
    res1 = from_backend(f(x_data_backend, y_data))

    @test isapprox(res1, data .+ 1 .+ x_data_copy)
    @test isapprox(from_backend(x_data_backend), 2 .+ data2 .+ y_data)

    # Test that an otherwise unused parameter can be mutated
    f = compile_array_function(array_function([a + 1], [x, y], [3 + b + y => x]))
    res1 = from_backend(f(x_data_backend, y_data))

    @test isapprox(res1, data .+ 1)
    @test isapprox(from_backend(x_data_backend), 3 .+ data2 .+ y_data)

end

@testset "Randoms" begin

    r1 = unknown_random_state()
    r2 = unknown_random_state()

    shape1 = (10, 100)
    shape2 = (20, 50)

    n1, r1_new = random_normal(r1, 1, 2, shape1...)
    n2, r2_new = random_uniform(r2, 0, 10, shape1...)
    n3, r2_new = random_uniform(r2_new, 0, 11, shape2...)
    n4, r1_new = random_normal(r1_new, 1, 3, shape2...)

    # Also useful: mutating random
    # n3 = random!(r2_new, shape2..., distribution=:uniform, min=0, max=10)
    # r2_new is replaced by a new RandomState() object, while the old one is preserved.
    # can we actually do that?

    expr1 = n1 + n2
    expr2 = n3 + n4

    # No updates
    f = compile_array_function(array_function([expr1, expr2, r1_new, r2_new], [r1, r2]))
    rs1 = random_state(seed=123)
    rs2 = random_state(seed=456)
    arr1, arr2, new_rs1, new_rs2 = f(rs1, rs2)

    # With explicit updates (if the random state is very large and hard to copy, e.g. for Random123)
    f = compile_array_function(array_function([expr1, expr2], [r1, r2], [r1_new => r1, r2_new => r2]))
    rs1 = random_state(seed=123)
    rs2 = random_state(seed=456)
    arr1, arr2 = f(rs1, rs2) # requires rng1 and rng2 to be mutable!

end

@testset "Nested functions" begin

    data = rand(100)
    data2 = rand(100)
    x = unknown_array()
    y = unknown_array()
    a = lazy_array(data)
    b = lazy_array(data2)

    x_data = rand(100)
    x_data_copy = copy(x_data)
    y_data = rand(100)

    f1 = array_function([a + 1 + x, 3 + y], [x, y])
    t1, t2 = f1(y, x)
    f2 = array_function([t1 + x, t2 - y], [x, y])

    # reference
    t1 = data .+ 1 .+ y_data
    t2 = 3 .+ x_data
    ref1 = t1 .+ x_data
    ref2 = t2 .- y_data

    f2c = compile_array_function(f2)
    res1, res2 = f2c(x_data, y_data)

    @test isapprox(from_backend(res1), ref1)
    @test isapprox(from_backend(res2), ref2)

end
