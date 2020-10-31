
function fact(n)
    r = 1
    while n > 1
        r = r * n
        n = n - 1
    end
    r
end

for n = 0:10
    println(fact(n))
end
