
# le problème de Josephus

# listes circulaires doublement chaînées

mutable struct L
    valeur
    suivant
    precedent
end

# liste réduite à un élément
function make(v)
    r = L(v, nothing, nothing)
    r.suivant = r
    r.precedent = r
    r
end

# insertion après un élément donnée
function inserer_apres(l, v)
    e = make(v)
    e.suivant = l.suivant
    l.suivant = e
    e.suivant.precedent = e
    e.precedent = l
end

# suppression d'un élément donné
function supprimer(l)
    l.precedent.suivant = l.suivant
    l.suivant.precedent = l.precedent
end

# affichage
function afficher(l)
    p = l
    print(p.valeur)
    p = p.suivant
    while p != l
	print(p.valeur)
	p = p.suivant
    end
    print("\n")
end

# problème de Josephus

# construction de la liste circulaire 1,2,...,n;
# l'élément renvoyé est celui contenant 1
function cercle(n)
    l = make(1)
    i = n
    while i >= 2
	inserer_apres(l, i)
        i = i - 1
    end
    l
end

function josephus(n, p)
    # c est le joueur courant, 1 au départ
    c = cercle(n)

    # tant qu'il reste plus d'un joueur
    while c != c.suivant
	# on élimine un joueur
	for i = 1 : p-1
	    c = c.suivant
	end
	supprimer(c)
	c = c.suivant
    end
    c.valeur
end

println(josephus(7, 5)) # 6
println(josephus(5, 5)) # 2
println(josephus(5, 17)) # 4
println(josephus(13, 2)) # 11

