
# arithmétique de virgule fixe
# precision q = 65536 i.e. 16 bits pour la partie décimale

function fp_add(x, y)
    x + y
end
function fp_sub(x, y)
    x - y
end
function fp_mul(x, y)
    t = x * y
    div(t + div(65536,2), 65536)
end
function fp_div(x, y)
    t = x * 65536
    div(t + div(y,2), y)
end
function fp_of_int(x)
    x * 65536
end

function iter(n, a, b, xn, yn)
    if n == 100
        return 1
    end
    xn2 = fp_mul(xn, xn)
    yn2 = fp_mul(yn, yn)
    if fp_add(xn2, yn2) > fp_of_int(4)
	0
    else
	iter(n+1, a, b, fp_add(fp_sub(xn2, yn2), a),
	     fp_add(fp_mul(fp_of_int(2), fp_mul(xn, yn)), b))
    end
end

function inside(x, y)
    iter(0, x, y, fp_of_int(0), fp_of_int(0)) > 0
end

function run(steps)
    xmin = fp_of_int(-2)
    xmax = fp_of_int(1)
    deltax = fp_div(fp_sub(xmax, xmin), fp_of_int(2*steps))
    ymin = fp_of_int(-1)
    ymax = fp_of_int(1)
    deltay = fp_div(fp_sub(ymax, ymin), fp_of_int(steps))
    for i = 0 : steps-1
	y = fp_add(ymin, fp_mul(fp_of_int(i), deltay))
	for j = 0 : 2*steps-1
	    x = fp_add(xmin, fp_mul(fp_of_int(j), deltax))
	    if inside(x, y)
		print("0")
	    else
		print("1")
	    end
	end
	print("\n")
    end
end

run(30)

