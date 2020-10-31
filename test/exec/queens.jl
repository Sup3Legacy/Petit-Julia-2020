
# les N reines

# renvoie l'entier 00...0011..11 (n bits faibles à n)
# c'est-à-dire l'ensemble {0,1,...,n-1}
# version inefficace de ^(^0 << n)
function full(n)
    if n == 0
	0
    else
	full(n-1) * 2 + 1
    end
end

# le plus petit élément de x
# version inefficace de x & -x
function lowestbit(x)
    if x == 1
	1
    else
	2 * lowestbit(div(x, 2))
    end
end

# différence ensembliste
# version inefficace de x &^ y
function andnot(x, y)
    if x == 0
	return 0
    end
    z = 2 * andnot(div(x, 2), div(y, 2))
    if x % 2 == 1 && y % 2 == 0
	z + 1
    else
	z
    end
end

function t(a, b, c)
    if a == 0
	return 1
    end
    f = 0;
    e = andnot(andnot(a, b), c)
    while e > 0
	d = lowestbit(e)
	f = f + t(a - d, (b+d)*2, div(c+d,2))
	e = e - d
    end
    f
end

function queens(n)
    t(full(n), 0, 0)
end

for n = 0:10
    print(n)
    print(" ")
    print(queens(n))
    print("\n")
end
