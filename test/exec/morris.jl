
# Calcul de la hauteur d'un arbre en espace constant en utilisant
#     Joseph M. Morris
#     Traversing binary trees simply and cheaply
#     Information Processing Letters 9(5), 1979

mutable struct Tree
    left
    right
end

function max(x, y)
    if x > y
        x
    else
        y
    end
end

function height(t)
    if t == nothing
        0
    else
        1 + max(height(t.left), height(t.right))
    end
end

function morris(t)
    d = 0
    h = 0
    while t != nothing
	if t.left == nothing
	    t = t.right
	    d = d+1
	    h = max(h, d)
	else
	    p = t.left
	    delta = 1
	    while p.right != nothing && p.right != t
		p = p.right
		delta = delta+1
	    end
	    if p.right == nothing
		p.right = t
		t = t.left
		d = d+1
		h = max(h, d)
	    else
		p.right = nothing
		t = t.right
		d = d - delta
	    end
	end
    end
    h
end

function linearl(t, n)
  if n == 0
      t
  else
      linearl(Tree(t, nothing), n - 1)
  end
end

function path(t, n)
  if n == 0
      return t
  end
  if n % 2 == 0
      path(Tree(nothing, t), n - 1)
  else
      path(Tree(t, nothing), n - 1)
  end
end

function mirror(t)
    if t == nothing
        t
    else
        Tree(mirror(t.right), mirror(t.left))
    end
end

function test(t)
    print(height(t))
    print(" ")
    print(morris(t))
    print(" ")
    print(morris(mirror(t)))
    print("\n")
end

test(nothing)
t1 = Tree(nothing, nothing)
test(t1)
t2 = Tree(t1, nothing)
test(t2)
t3 = Tree(Tree(nothing, nothing), t2)
test(t3)
test(Tree(Tree(Tree(nothing, Tree(nothing, nothing)),
	       Tree(Tree(nothing, nothing), t3)),
   	  Tree(nothing, Tree(nothing,Tree(nothing,nothing)))))
test(linearl(nothing, 42))
test(path(nothing, 42))

function fib()
    a = 0
    b = 1
    while a < 100
        test(path(nothing, a))
        b = a + b
        a = b - a
    end
end
fib()
