
# https://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences

function f(n)
    if n == 0
        1
    else
        n - m(f(n - 1))
    end
end

function m(n)
    if n == 0
        0
    else
        n - f(m(n - 1))
    end
end

# les deux séquences diffèrent si et seulement si n+1 est un nb de Fibonacci

for n = 0 : 50
    if f(n) != m(n)
        println(n + 1)
    end
end


