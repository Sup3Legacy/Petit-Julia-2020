
# arbres binaires de recherche (immuables)

struct BST
    left
    value
    right
end

function add(x, t)
    if t == nothing
	BST(nothing, x, nothing)
    elseif x < t.value
	BST(add(x, t.left), t.value, t.right)
    elseif x > t.value
	BST(t.left, t.value, add(x, t.right))
    else
        t
    end
end

function mem(x, a)
    if x == a.value
        return true
    end
    if x < a.value && a.left != nothing
        return mem(x, a.left)
    end
    if a.right != nothing
        return mem(x, a.right)
    end
    false
end

function print_tree(a)
    if a == nothing
        return
    end
    print("(")
    if a.left != nothing
        print_tree(a.left)
    end
    print(a.value)
    if a.right != nothing
        print_tree(a.right)
    end
    print(")")
end

function main()
    tree = nothing
    for i = 1 : 9
        x = (55 * i) % 34
        tree = add(x, tree)
        print_tree(tree)
        print("\n")
    end
    if mem(8, tree) && !mem(0, tree) && mem(32, tree) && !mem(22, tree)
        println("ok")
    end
    tree = add(42, tree)
    tree = add(-1, tree)
    print_tree(tree)
    print("\n")
end

main()
