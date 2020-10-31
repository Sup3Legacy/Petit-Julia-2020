
# les bornes de for ne sont évaluées qu'une seule fois

function f(x)
    print(x, ".")
    x
end

for n = f(1) : f(10)
    print(n)
end
println()
