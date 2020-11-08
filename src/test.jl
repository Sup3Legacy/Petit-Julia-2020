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
