
# la variable de la boucle for est locale
# mais les bornes sont évaluées dans l'environnement global

function f(x)
    print(x, ".")
    x
end

n = 42
println(n)

for n = f(1) : f(n)
    print(n)
end
println()

println(n)

