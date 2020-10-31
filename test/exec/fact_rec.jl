
function fact(n)
    if n <= 1
        return 1
    else
        return n * fact(n-1)
    end
end

for n = 0:10
    println(fact(n))
end
