
# la variable de la boucle for est locale

function f(x)
    print(x, ".")
    x
end

n = 42
println(n)

for n = f(1) : f(10)
    print(n)
end
println()

println(n)

