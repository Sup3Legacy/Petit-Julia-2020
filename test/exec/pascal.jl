
# triangle de Pascal modulo 7

mutable struct List
    head
    next
end

function get(l, i)
    if i == 0
        l.head
    else
	get(l.next, i-1)
    end
end

function set(l, i, v)
    if i == 0
        l.head = v
        return
    end
    set(l.next, i-1, v)
end

function create(n)
    if n == 0
        nothing
    else
        List(0, create(n-1))
    end
end

function print_row(r, i)
    for j = 0 : i
	if get(r, j) != 0
	    print("*")
	else
	    print(".")
	end
    end
    print("\n")
end

function compute_row(r, i)
    j = i
    while j > 0
	set(r, j, (get(r,j) + get(r,j-1)) % 7)
        j = j-1
    end
    set(r, 0, 1)
end

function pascal(n)
    r = create(n + 1)
    for i = 0 : n-1
	set(r, i, 0)
	compute_row(r, i)
	print_row(r, i)
    end
end

pascal(42)

