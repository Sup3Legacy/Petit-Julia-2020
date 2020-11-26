
function fib(n)
    a = 0
    b = 1
    while n > 0
        b = b + a
        a = b - a
        n = n - 1
    end
    a
end

for n = 0:10
    println(fib(n))
end
