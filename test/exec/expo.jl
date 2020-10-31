
# exponentiation rapide

function expo(x, n)
    # n >= 1
    if n == 1
        x
    else
        y = expo(x, div(n, 2))
        y = mult(y, y)
        if n % 2 == 0 y else mult(x, y) end
    end
end

function mult(x::Int64, y::Int64) x * y end

println(expo(2, 1))
println(expo(2, 10))

# fibonacci en temps O(log n)

struct Mat22 a11; a12; a21; a22 end

function mult(x::Mat22, y::Mat22)
    Mat22(x.a11 * y.a11 + x.a12 * y.a21, x.a11 * y.a12 + x.a12 * y.a22,
          x.a21 * y.a11 + x.a22 * y.a21, x.a21 * y.a12 + x.a22 * y.a22)
end

fibM = Mat22(1, 1, 1, 0)
function fib(n)
    m = expo(fibM, n)
    m.a12
end

for n = 1:10 println(fib(n)) end

println(fib(1000000000))  # avec un débordement, bien sûr, mais instantané

