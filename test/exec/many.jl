function many(a,b,c,d,e,f,g,h,i,j)
    print(a)
    print(b)
    print(c)
    print(d)
    print(e)
    print(f)
    print(g)
    print(h)
    print(i)
    print(j)
    println()
    if a < 9
	many(b, c, d, e, f, g, h, i, j, a)
    end
end

many(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

