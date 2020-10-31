
# arbres binaires de recherche mutables

mutable struct BST
    left
    value
    right
end

# principe : on modifie l'arbre *et* on renvoie toujours sa racine
#            ainsi on traite agréablement le cas de l'arbre vide
function add(x, t)
    if t == nothing
	return BST(nothing, x, nothing)
    end
    if x < t.value
	t.left = add(x, t.left)
    elseif x > t.value
	t.right = add(x, t.right)
    end
    return t
end

function mem(x, a)
    if x == a.value
        true
    elseif x < a.value && a.left != nothing
        mem(x, a.left)
    elseif a.right != nothing
        mem(x, a.right)
    else
        false
    end
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
