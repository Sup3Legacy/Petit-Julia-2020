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
