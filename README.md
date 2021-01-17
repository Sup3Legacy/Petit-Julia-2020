![logo pjulia](logo.png)

# Projet compilation 2020
### Constantin Gierczak--Galle & Samuel Vivien
### Cours de compilation, Jean-Christophe Filliâtre & Basile Clément, ENS Ulm, 2020

## Les parties I] à VI] inclues n'ont pas été modifiées entre les deux rendus, simplement renuméroté II] à VII].

# 0] Prérequis

Notre projet requiert l'installation, via `opam` des bibliothèques `ppx_deriving`, `Core_kernel`, `Core`, `Yojson`, `cohttp-lwt-unix` et `cohttp-async`. Le paquet `lwt_ssl` doit aussi être installé via le gestionnaire de paquets de son choix.

De plus, l'utilisation optimale du **REPL** nécessite l'installation du **wrapper** `rlwrap`, disponible via un gestionnaire standard de packages Linux (disponible aussi sur mac).

L'interpréteur n'est pas à jour et ne supporte pas certaines fonctionnalités supportées par le compilateur. cf plus bas.

# I] Syntaxe

Mots-clés : else elseif end false for function if mutable return struct true while **dowhile** **assert** 

- < chiffre > ::= 0-9
- < alpha > ::= a-z | A-Z | _
- < ident > ::= < alpha > (< alpha > | < chiffre > )<sup>*</sup> 
- < entier > ::= < chiffre ><sup>+</sup>
- < car > ::= tout caratère ASCII compris entre 32 et 126 autre que \ et " | \\ | \" | \n | \t
- < chaîne > ::= " < car ><sup> * </sup> "
- < entier-ident > ::= < entier > < ident >
- < ident-parg > ::= < ident > (
- < entier-parg > ::= < entier > (
- < parg-ident > ::= )< ident >
- < docstring > ::= """ car<sup> * </sup> """
- < flottants > ::= ( < chiffre ><sup>+</sup> . | . < chiffre ><sup>+</sup> | < chiffre ><sup>+</sup>. < chiffre ><sup>+</sup>)((e|E)(-|+)? < chiffre ><sup>+</sup>)?
- < char > ::= '< car >'

Règles de priorité :

| opérateur | associativité |
| :---------| :------------ |
| **assert** | _ |
| return | - |
| = | droite |
| \|\| | gauche |
| && | gauche |
| ==, !=, >, >=, <, <= | gauche |
| +, - | gauche |
| *, % | gauche |
| - (unaire), ! | - |
| ^ | droite |
| **@** (concat) | gauche |
| . | gauche |

Grammaire :

- < fichier > **::=** < decl ><sup> * </sup> EOF
- < decl > **::=** < structure > | < fonction > | < expr > ;
- < structure > **::=** mutable? struct < ident > (< param >?)<sup> * </sup><sub>;</sub> end ;
- < fonction > **::=** **< docstring >?** function < ident-parg > < param ><sup> * </sup><sub>;</sub> (:: < ident >)? < bloc > end ;
- < param > **::=** < ident > (:: < ident >)?
- < expr > **::=**  \| < entier > \|< chaîne > \| true \| false \| **< flottant >**

 	\| **< char >** \| **[< expr ><sub>,</sub><sup> * </sup> ]** \| < entier-ident > 

 	\| < entier-parg > < bloc1 > ) \| (< bloc1 >) \| ( < expr >< pard-ident > 
 
	\| < ident-parg > < expr ><sup> * </sup><sub>,</sub> ) \| !< expr > \| - < expr > 

	\|  < expr > < operateur > < expr > \| < lvalue >  \| < lvalue > = < expr >

	\| return < expr >? \| **assert < expr >**  \| for < ident > = < expr > : < expr > < bloc > end 

	\| while < expr > < bloc > end \| **dowhile < bloc > end** \| if < expr > < bloc > < else >

- < lvalue > **::=** < ident > | < expr > . < ident > | **< expr > [ < expr > ]**
- < else > **::=** end | else < bloc > end | elseif < expr > < bloc > < else >
- < opérateur > **::=** == | != | < | <= | > | >= | + | - | * | % | ^ | && | || | **@**
- < bloc > **::=** < expr > ?<sub>,</sub><sup>* </sup>
- < bloc1 > **::=** < expr > (; < bloc > )?




# II] Lexer/Parser

La première étape a été de définir les **types** qui seront utilisés par les différentes étapes d'analyse. Dans un premier temps, nous avons suivi la **grammaire** donnée dans le sujet, que ce soit pour la définition de l'`ast` (les types correspondaient exactement aux **règles de grammaire**) ou bien pour le **lexer** et le **parser**.

Cela nous a donné un analyseur à peu près fonctionnel mais souffrant de dizaines de **conflits** (par exemple au niveau de la construction `while expr bloc`). Cela a été résolu par un **remodelage progressif** du parser, éliminant tous les conflits un par un!

Le parser a subi un nouveau remodelage général au début de notre travail sur le typeur car nous avions oublié de transmettre les positions des token dans l'`ast`, ce qui nous empêchait de pouvoir positionner précisement les erreurs de types.

Une erreur de parser affiche une erreur de syntaxe donnant la position du dernier lexème lu avant de planter et -s'il y a suffisament peu de possibilités- il donne la liste des catégories de lexèmes autorisés à ce moment-là.

Nous avons implémenté une règle dans le lexer et le parser permettant de donner aux fonction des **docstrings**, dans le même format que `Julia`. Pour l'instant, les docstringss sont reconnues et enregistrées dans l'arbre syntaxique mais nous n'avons pas encore eu le temps d'implémenter dans le **REPL** une fonctionnalité permettant d'accéder à la docstring d'une fonction (via `help [nom fonction]`, par exemple).


# III] Typer


Le **typeur** est une étape importante du projet et certaines décisions ont été prises par rapport à ce que l'on autorisait ou non. Ces décisions ont été prises en prenant en compte le fait que nous pouvions toujours revenir modifier certains points plus tard dans le projet si besoin.

Pour cela le fichier est parcouru **deux fois**. Il s'agit des parcours 1 et 2 (vive l'originalité !):

### Parcours 1 :

**Il a été choisi que les (en fait les constructeurs de) structures seraient gérées comme des fonctions. Cela signifie que pour nous, un constructeur de structure est un appel de fonction dont l'image est la structure construite.**

Le fichier est découpé en une liste de déclarations de **fonctions**, de déclarations de **structures** et d'**expressions**.

On fait le parcours en utilisant **4 environnements Map** :
- un pour les **variables définies**
- un pour toutes les **fonctions**, qui à tous les noms de fonctions associe l'ensemble des types (types de paramètres et type de l'image)
- un pour tous les **noms de structures**
- un pour tous les **noms d'attributs** associés à leur type, si la structure les contenant est mutable et le nom de la structure associée.


Si l'on rencontre une **déclaration de structure**, on effectue ces actions :
- on vérifie que son nom n'est pas `print`, `println` ou `div`
- on vérifie que l'on n'a pas d'autre structure du **même nom**
- on parcourt les champs de la struture en vérifiant que les types sont bien définis et les noms distincts et pas déjà attribués (par exemple deux structures différentes ne peuvent pas avoir le même champ `a`), puis on rajoute ses champs à l'ensemble des champs existants. Il a été décidé de ne pas autoriser un champ d'une structure `S` à être du type Struct `S` car sinon on n'arriverait pas à construire la première variable de type `S`
- on ajoute la structure (enfin plus exactement son constructeur) à l'ensemble des fonctions du même nom, ainsi qu'à l'ensemble des structures

Si l'on rencontre une **déclaration de fonction**, on vérifie dans l'ordre :
- que son nom n'est pas déjà associé à une variable
- que son nom n'est pas `print`, `println` ou `div`
- que ses arguments possèdent bien un **type existant** et que les noms sont deux-à-deux distincts
- que son type retourné est bien existant
- puis on rajoute la fonction à l'ensemble des fonctions du même nom en vérifiant qu'il n'existe pas déjà une autre fonction du même nom ayant exactement les mêmes types d'entrée

Si l'on rencontre une **expression**, on la parcourt récursivement et propageant l'environnement des variables existantes avec les règles suivantes :
- si l'on rencontre une expression utilisant une **variable**, on teste si elle existe. Si ce n'est pas le cas on lève une exception
- si l'on rencontre une **affectation de variable** on crée la variable associée dans l'environnement
- si l'on rencontre une **affectation d'attribut**, on vérifie si l'attribut existe et s'il est mutable
- si l'on rencontre un **appel de fonction**, on vérifie qu'il existe une fonction ou une struture du même nom
- si l'on rencontre une expression qui définie un nouvel environnement (boucles `for` et `while`), alors on fait un premier parcours pour récupérer l'ensemble des variables définies dans le bloc à l'intérieur (on ne descend pas dans les `for`/`while` suivants par contre). Puis on reparcourt le bloc en testant bien tous les types en commençant avec l'environnement global privé des valeurs qui seront définies par la suite (en réalité c'est beaucoup plus compliqué car il faut gérer les différences d'environnement global/local).

**La première différence notable avec ce qui est demandé dans le sujet est que le typeur teste si les variables sont bien définie AVANT leur utilisation et pas uniquement si elles sont bien définies dans le même champ. Ce qui permet de faire planter les tests dans exec-fail suivant : `undef1.jl`, `undef3.jl`, `undef4.jl` et `undef5.jl`**

### Parcours 2 :

On teste si tout est **correctement typé**, notamment pour toutes les affectations, on teste si on met bien dans une variable un type compatible avec le type que possède déjà la variable.

On vérifie aussi que toutes les expressions sont bien typées.

De plus, pour chaque appel de fonction, on regarde l'ensemble des fonctions compatibles. Pour cela, on calcule d'abord la liste des types des expressions données en arguments. Puis pour chaque fonction compatible on compte le nombre de `Any` dans ses types d'arguments et on récupère la liste des types des arguments dont l'expression à la position correspondante est de type `Any`. Il a été décidé que si toutes les fonctions avaient les mêmes listes et nombre alors le typage plantait car il y avait une impossibilité de savoir quelle fonction appeler. Cela permet notamment de faire planter le test `typing/bad/testfile-ambiguity-1.jl` (les deux fonctions on pour couple `(1,[])` à l'appel).

### Difficultés :

La principale difficulté rencontrée dans le typage de Petitjulia™ se cachait dans la **portée des variables**.

# IV] Samenhir

Ce projet, si l'on se contente de rester dans le cadre restreint du sujet laisse un sentiment de vide, on a l'impression d'avoir sauté des étapes, d'avoir triché pour arriver au bout. Cette étape ainsi sautée est bien sûr le **parser**! Nous nous contentons en effet dans ce projet d'utiliser `menhir` pour faire notre parser sans comprendre réellement ce qui se passe derrière. C'est de là qu'est venue l'idée de **Samenhir** (`NDLR :` contraction de "Menhir" et de "Samuel", ce dernier étant à l'origine de cette idée et le principal responsable et développeur de cet outil qui a multiplié le temps de compilation du projet par plus de `100`!) : l'idée d'implémenter une version simplifiée de `menhir` afin de l'utiliser dans le projet.

Actuellement **Samenhir** est totalement indépendant de `menhir`, c'est à dire capable de générer lui même son propre parser. Les tests du mercredi 2 décmebre à 23h40 affirment que Samenhir est pleinement opérationnel (`NDLR :` je précise que, dixit son créateur, "Samenhir semble marcher dans le cas particulier de petitjulia; je ne peux affirmer qu'il soit correct dans le cas général" :) ) et qu'il valide tous les tests de syntaxe et de typage.

### Grammaire à fournir à Samenhir :

**Samenhir** a besoin d'une grammaire ressemblant fortement à celle demandée par `menhir`. Cependant, par soucis de simplification de l'implémentation, certaines décisions ont été prises :
- Le parser doit être écrit dans un fichier `.sam` afin de différencier d'un fichier `.mly` car la syntaxe n'est pas entièrement compatible avec `Menhir` et `Ocamlyacc`
- la première lettre du nom d'une règle doit être **minuscule** et la première lettre du nom d'un token **majuscule**
- il est possible de mettre un bout de code au dessus (`%{ code ocaml %}`}) du parser mais pas en dessous
- une déclaration de règle nécessite de renseigner le **type de renvoi de la règle**. Cela permet d'éviter de faire nous-mêmes de l'**inférence de type** ou d'utiliser le module `Obj`, comme ceci :
```ocaml
rule<int * int>:
	| i1 = INT i2 = INT {(i1, i2)}
;
```
- en raison de la décision ci-dessus, il n'est plus nécessaire de renseigner le type de la règle de départ
- pour les **règles de priorité** (mais qui actuellement ne sont pas utilisées par Samenhir), on ne peut mettre qu'un seul mot par **règle d'associativité**
- il n'est pas possible d'utiliser des outils tel que `%inline` ainsi que `list`, `separated_list`, etc.

Il y a peut être d'autres points de divergence entre **Samenhir** et `Menhir` liés à l'ignorance de notre part de certaines spécificitées technique de `Menhir`!


### Algorithme utilisé pour construire l'analyseur :

Pour pouvoir construire l'**analyseur syntaxique**, **Samenhir** utilise l'**algorithme de Knuth** présenté slides 81-82 du cours `analyse syntaxique (1/2)`.

### Inconvénients :

Actuellement, **Samenhir** est très **peu optimisé**. Son utilisation dans le projet ralonge fortement la durée de compilation du compilateur de Petitjulia™. Cependant, le parser ainsi généré fonctionne comme il devrait, en **passant tous les tests** de typage ainsi que les tests de syntaxe.

Nous avons aussi la certitude que **Samenhir** n'est _pas entièrement correct_. Il manque de nombreuses sécurités par rapport aux différentes possibilités d'utilisations frauduleuses par un utilisateur De plus, il n'y a pas de typeur (on a considéré qu'un seul typeur dans le projet était suffisant). Cependant, le compilateur `pjuliac` utilise le fichier `parser.ml` généré par Samenhir et arrive à passer tous les tests de typage et de syntaxe. On part donc du principe suivant : `ça ne plante pas donc ça marche !`™.

Ces inconviénients sont faibles par rapport à la satisfaction personnelle d'utiliser un outil que l'on a développé soi-même plutôt que se reposer sur le travail de quelqu'un d'autre!

# V] Interpreter/REPL

## 1) Interpreter

Tandis que l'on avançait sur la construction du **compilateur**, il nous a semblé utile d'implémenter un **interpréteur** pour pouvoir facilement tester et débeuguer les étapes d'analyse syntaxique et de typage. L'implémentation de cet interpréteur n'a pas été très difficile et est calquée sur l'implémentation de l'interpréteur `Mini-Python` que nous avons vu en début d'année. Les quelques difficultés rencontrées avaient souvent rapport aux **différences de comportement de Julia** (que nous prenions comme référence pour certaines subtilités) entre le **mode REPL** et le **mode compilateur**; et cela d'autant plus que nous avons assez longuement hésité sur le mode à adopter : d'un côté cet interpréteur nous sert à tester le comportement de notre compilateur, donc il devrait avoir exactement le même comportement. De l'autre côté, un **REPL** (_cf._ ci-dessous) qui a un comportement de compilateur n'est pas très logique!

Ainsi, pour l'instant, nous avons un interpréteur qui fonctionne uniquement en mode **REPL** mais nous prévoyons d'éventuellement lui ajouter un mode "compilateur", pour répliquer le comportement attendu de ce dernier, afin de nous aider à le concevoir et à le débeuguer!


## 2) REPL

**/!\\ cette partie nécessite l'installation préalable de rlwrap**

**Version courte :** le **REPL** peut être exécuté directement (c'est le fichier `pjuliarepl`) ou via le script `pjuliarepl-rlwrap.sh` pour bénéficier des **fonctionnalités** de `rlwrap`!

Pour rendre l'utilisation de l'interpréteur plus intuitive et interactive, nous avons aussi implémenté un **REPL**, très semblable aux **REPL** `OCaml` ou `Python` (nos modèles) ou même à celui de `Julia`.

L'utilisation du **REPL** est très simple : entrer du code Petitjulia™ l'envoie vers l'interpréteur. Toutes les variables/fonctions/structures sont ajoutées à l'environnement global.

En plus de cela, nous avons ajouté quelques commandes spéciales :
- `#run file` permet de charger et exécuter un **fichier**. Cette commande diffère de la commande présente dans le REPL Julia (`include(file)`) car il nous semblait plus simple d'utiliser une **syntaxe spéciale** pour que le **REPL** puisse décider simplement s'il doit charger un fichier et le faire suivre à l'interpréteur ou bien s'il doit directement lui faire suivre l'entrée.
- `#flush` vide entièrement les **environnements de typage et interprétation**; un peu comme si on relançait le **REPL**. Cette commande est la solution à un problème que nous avons rencontré. Il peut arriver que l'utilisateur ait besoin de **redéfinir une fonction** précédemment définie. Les environnements de typage et d'interprétion restant les mêmes lors d'une session REPL, le typeur soulèverait une erreur de **double définition** d'une fonction, ce qui n'est pas autorisé par le langage.
- `#exit` permet de **fermer proprement le REPL**, au lieu de le faire sauvagement planter à grand renfort de `Ctrl+C`!
- Il y a d'autres **commandes easter-eggs** cachées dans le code; leur recherche est laissée comme exercice au lecteur :)

Le **REPL** est pourvu d'un système de **récupération d'exception** qui `catch` toutes les erreurs levées par l'analyse **lexicale**, **syntaxique**, le **typage**, l'**interprétation** ou bien même les erreurs levées par le **système** (par exemple en cas de fichier introuvable) et affiche les messages correspondants sur la sortie stantard. Cela nous permet d'avoir un **REPL** qui ne plante pas à la moindre faute de frappe mais affiche l'erreur et permet de corriger la commande entrée, notamment grâce à l'**historique de commandes** (voir ci-dessous).

Enfin, nous utilisons un **wrapper**, `rlwrap`, pour ajouter à notre **REPL** les fonctionnalités attendues d'un tel système : historique des commandes, auto-complétion, édition de la ligne courante, etc.

L'**auto-complétion** est assez rudimentaire : on fournit à `rlwrap` la liste des **mots-clé** de Petitjulia, dans le fichier `pjulia-words`; de plus, le wrapper retient les **mots utilisés**, en entrée comme en sortie. Cela fait que beaucoup de mots sont retenus (plus que nécessaire) mais c'est la seule solution simple que l'on ait trouvée pour avoir une auto-complétion adaptative.

L'**édition de la ligne courante** et l'**historique des commandes** sont des fonctionnalités de base de `rlwrap` et marchent très bien. Il est possible de modifier le nombre de commandes retenus mais la valeur par défaut, `300`, suffit à notre avis grandement.

Enfin `rlwrap` nous permet d'afficher le `ρjυλια>` en couleur, ce qui nous semble essentiel à tout **REPL** qui se respecte!

## 3) Performances du REPL

Bien entendu, ayant un REPL fonctionnel (en tout cas selon nos tests), nous avons voulu le comparer, en termes de **performances**, au **REPL** `Julia` officiel!

Nous avons donc utilisé la fonction de **Ackermann**, implémentée comme ceci :

```julia
function ackerman(m::Int64, n::Int64)::Int64
	if m == 0 (n + 1)
	elseif 0 == n ackerman(m-1, 1)
	else ackerman(m-1, ackerman(m, n-1)) end
end
```

Ensuite nous avons mesuré le temps mis par le calcul de `ackermann(3,8)` sur les deux interpréteurs. Sans rien configurer, notre interpréteur a subi une défaite cuisante contre l'interpréteur Julia. Le temps de calcul sur le notre avait en effet plus qu'un **facteur 50** par rapport à la référence (en utilisant exactement le même fichier de base).

Cependant, nous nous sommes souvenus que le **REPL** de `Julia` "triche" un peu, en cela qu'il compile à la volée le code pour ensuite l'exécuter dans le processus courant. Nous avons refait le même test, cette fois-ci en appelant le **REPL** `Julia` de cette façon `julia --compile=no`. Le résultat est devenu beaucoup plus intéressant : notre interpréteur ne met plus qu'environ **10% plus longtemps** pour calculer `ackermann(3,8)`, ce qui nous paraît être un très bon premier résultat!

Nous n'avons malheureusement pas encore eu le temps d'effectuer des **tests de performances** plus poussés et plus fiables, donc ce résultat est à prendre avec un peu de recul, et cela d'autant plus que le langage Julia est beaucoup plus complexe que notre petit fragment. On peut ainsi raisonnablement s'attendre à ce que l'**interpréteur Julia** ait des étapes d'interprétation supplémentaires (Par exemple la gestion des opérateurs `+ - *` qui sont surchargés du fait de la présence de flottants dans Julia) par rapport à notre interpréteur basique. Ces étapes allongeant plus ou moins le temps pris par l'interprétation, notre succès est un peu moindre.


# VI] Automatisation du build et des tests

En l'état actuel des choses, la compilation du compilateur (`pjuliac.exe`) et du REPL (`pjuliarepl.exe`) est gérée par `dune` via un `dune-project` commun. Elle est réalisable via notre `Makefile`. Nous avons paramétré celui-ci pour **automatiser** au maximum les tests et essais des différentes parties de notre projet.

Nous retrouvons :
- `make` : construit les deux fichiers, de plus pour le confort de l'utilisateur un exécutable `pjuliac` est mis dans le **répertoire courant**. Cependant cet exécutable est supprimé par la commande `make clean`. Pensez donc à le déplacer avant si vous voulez le garder après avoir nettoyé le reste du projet.
- ` make clean` : efface les fichiers engendrés par la compilation du projet.
- `make repl`/`make compil` : construit et exécute le fichier (le `pjuliarepl.exe` ou `pjuliac.exe`).
- `make testExec`/`make failsExec` : exécute les tests d'exécution positifs/négatifs. (dans `/test/exec/` et `/test/exec-fail`)
- `make testSyntax`/`make failsSyntax` : de même pour la syntaxe. (**NB :** nous utilisons aussi les tests d'exécution et de typage, ici, pour ajouter une batterie de tests positifs!)
- `make testTyping`/`make failsTyping` : de même pour le typage. (**NB :** nous utilisons aussi les tests d'exécution. Notre but étant de faire un typer un peu plus puissant que demandé, nous cherchons à faire en sorte que certains fichiers qui devraient planter à l'exécution plantent au typage!).

Le différents tests utilisent le fichier `pjuliac.exe` qui est présent dans les fichiers construits par `dune`. Ils sont donc indépendants de l'existence ou non du fichier `pjuliac`, qu'il est donc possible de supprimer/déplacer.

# VII] Conclusion partielle

Cette première partie du projet nous aura **beaucoup occupés**, d'autant plus que nous nous sommes posé des défis supplémentaires plus ou moins conséquents!

Nous avons pu mettre en place tous les outils nécessaires à la **suite du projet**, ainsi que d'autres outils nous permettant de l'approfondir.
Cependant, nous considérons nécessaire de continuer à travailler sur **Samenhir**, ne serait-ce que pour optimiser la production de code afin de **diminuer le temps de compilation** du compilateur.

Pour la deuxième partie du projet, nous projetons d'ajouter aussi à PetitJulia™ le support des **flottants** sur 64 bits ainsi que des **listes** (probablement tout simplement via des structures cachées derrière du sucre syntaxique); si nous en avons le temps!

# VIII] Corrections des problèmes du premier rendu

Nous avons corrigé les flags qui étaient mal écrits lors du premier rendu.

Nous avons aussi ajouté le support du multi-ligne au REPL : lorsque l'on entre quelque chose sur la ligne de commande, le REPL attend d'avoir vu passer deux `\n` successifs avant d'envoyer la commande à l'interpréteur. Ainsi, il est maintenant possible d'entrer du code sur plusieurs lignes, tant qu'il n'y a pas de ligne vide dans ce code.

L'inconvénient est cependant qu'il faut toujours penser à **faire deux retours à la ligne, même lorsque la commande ne fait qu'une seule ligne!**

La puissance du typeur a été diminuée car il rejetait des codes tels que 
```julia
function f()
	for i = 1:3
		y = 0
	end
	println(y)
	y = 0
end
```
Du fait de cela, il n'y a plus qu'un seul test dans `exec-fail/undef*.jl` qui plante au typage (par rapport aux quatre tests de `exec-fail/undef*.jl` qui étaient rejetés par notre typeur au premier rendu).

# IX] pPkg, depManager, namespace et pjulia-packages

Durant le travail sur ce projet, il nous a semblé intéressant d'implémenter non seulement un compilateur, mais aussi un environnement de développement 'complet' pour notre language. C'est pour ça que nous avons mis en place un système de paquets permettant, comme dans beaucoup d'autres languages, de scinder les composantes d'un projet en plusieurs fichiers, ou bien d'utiliser des fonctions pré-implémentées.

## 1) pjulia-packages

Nous avons créé un [repo Github](https://github.com/Sup3Legacy/pjulia-packages) contenant quelques modules de base (par exemple `matrix`, qui contient des méthodes utiles pour exploiter le potentiel des matrices). Ce repo contient de plus un répertoire des paquets disponibles, sous la forme d'un fichier JSON avec des enregistrements comme ceci :

```json
{
  "name": "random",
  "version": "0.1.4",
  "description": "Fonctions aléatoires utiles",
  "dependencies": [],
  "url": "https://raw.githubusercontent.com/Sup3Legacy/pjulia-packages/main/random.jl"
}
```

## 2) pPkg

De même que Julia a son gestionnaire de paquets : `Pkg`, nous avons doté petit-Julia de `pPkg`. Il s'agit d'un tout petit gestionnaire de paquets très basique incorporé dans notre REPL et avec lequel il est possible d'interagir via quelques commandes dans la console de ce dernier :
- `#update` : cette commande télécharge la liste des paquets disponibles (`index.json`).
- `#install package` : cette commande installe le paquet `package`. Les paquets sont installés dans un sous-répertoire `/packages`. (Si cette commande jette une erreur, il faut possiblement ajouter ce sous-dossier à la main). /!\ pour cela, il faut avoir au préalable téléchargé la liste des paquets avec `#update`
- `#remove package` : cette commande supprime le paquet `package`, s'il existe.

NB : 
* le temps nous a manqué pour ajouter à pPkg la gestion des dépendances et des versions. Il n'en tient donc pas compte.
* pPkg utilise plusieurs modules, notamment pour faire des requêtes HTTPS ou bien parser les fichiers JSON. Ces modules étant capricieux et n'ayant de notre côté pas eu beaucoup de temps pour les stabiliser, pPkg a parfois tendance à avoir des comportements plus ou moins indéfinis.

## 3) Namespace

Une fois un gestionnaire de paquets basique mis en place, nous avons réfléchi à la façon d'importer les paquets pour les utiliser dans des programmes pJulia. Après plusieurs tests plus ou moins ratés, nous avons statué sur ceci :
- L'importation d'un paquet se fait avec l'expression `include("package.jl")` (il ne faut pas oublier le `.jl`). Tous les `include` doivent être mis tout en haut du fichier, avant tout autre expression ou déclaration. /!\ Les paquets doivent être dans le sous-répertoire `/packages`.
- L'utilisation des fonctions ou variables globales définies dans un paquet, nous utilisons cette syntaxe : `package::fonction(a, package::variable)`.

Petit exemple :

```julia
include("matrix.jl")
include("gol.jl")
include("random.jl")

a = matrix::make_matrix(2, [2, 2], 0)
somme_de_a = matrix::sum_matrix(a)
println(somme_de_a)
gol::run()
```

Nous avons opté pour la syntaxe `package::objet` plutôt que `package.objet` pour plusieurs raisons : 

* Cela simplifie beaucoup l'analyse du fichier, en évitant la confusion avec les expressions `structure.champ`.
* Il s'agit d'une syntaxe qui nous semble jolie et qui est utilisée dans plusieurs autres langages de programmation (notamment Rust).

NB : Lors de la compilation, les `::` sont remplacés par des `.`, pour que le fichier ASM soit accepté par `gcc`. En effet, d'expérience, `gcc` apprécie très peu de voir des caractères `:` à une place inattendue! **/!\\** Cela est fait après le parsing, donc il n'y a pas de risque de confusion avec `structure.champ`.

## 4) depManager

Le composant central de notre système de paquets est `depManager.ml`, s'intégrant entre le parsing et le typage.

Lorsque depManager rencontre un appel à `include`, il charge le fichier correspondant, le parse, et modifie tous les identifiants (sauf primitives) apparaissant dans l'arbre syntaxique du paquet pour correspondre au nom par lequel on appelle ces fonctions et variables globales. Pour finir, il ajoute cet arbre de syntaxe abstraite au début l'arbre du fichier principal. Par exemple, si on se donne les fichiers :

```julia
#package1.jl
function succ(n :: Int64) :: Int64
	return n + 1
end;
varGlob = 69
```

```julia
#package2.jl
include("package1.jl")
function foo(n :: Int64) :: Int64
	return n
end;
bar = 42
```

et

```julia
#test.jl
include("package1.jl")
include("package2.jl")

println(package1::succ(5), package1::varGlob)
println(package2::package1::succ(5))
println(package2::bar)
```

L'AST généré par depManager lors de la compilation de `test.jl` correspond à un fichier d'origine ressemblant à  :

```julia
#test.jl 

#package1 importé depuis test
function succ(package1__n :: Int64) :: Int64
	#On peut voir ici qu'à tous les identifiants a été ajouté "package1."
	return package1__n + 1
end;
package1__varGlob = 69

#package1 importé depuis package2, lui-même importé depuis test
function succ(package2__package1__n :: Int64) :: Int64
	#on a déjà ajouté "package1." puis "package2."
	return package2__package1__n + 1
end;
package2__package1__varGlob = 69

#package2 importé depuis test
function foo(package2__n :: Int64) :: Int64
	return package2__n
end;
package2__bar = 42

println(package1__succ(5), package1__varGlob)
println(package2__package1__succ(5))
println(package2__bar)
```

On peut remarquer une chose : on ne peut pas importer plusieurs fois le même paquet depuis un même fichier. Cependant, on peut importer un même paquet depuis un fichier, tout en l'important aussi depuis un fichier lui-même importé. On se retrouve alors avec deux copies _a priori_ identiques du paquet, donc seuls les identifiants diffèrent. Cela ajoute une masse parfois importante aux exécutables, mais cela permet d'isoler le comportement des différents modules du programme. De plus ce système évite tout risque de collision de nom entre les paquets (on peut imaginer importer plusieurs paquets, chacun implémentant une méthode `new`).

# X] génération de code du projet de base (donc sans flottants ni arrays)

Toutes les valeurs sont composées de deux champs de 64 bits : le premier pour le type et le second pour la valeur en elle-même. Les valeurs "grandes", telles que les structures, sont composées d'un pointeur vers un emplacement dans la mémoire qui contient toute l'information. Les types `undef` et `nothing` sont aussi implémentés sur 128 bits pour simplifier leur utilisation.

Les variables globales sont définies dans `.data` et nommées `nom_val` et `nom_type`.
Les variables locales sont placées dans la pile et adressées avec une adresse relative par rapport à `%rbp`. Tous les emplacement de variables locales utilisés par une fonction seront tous créés à l'appel de cette fonction, ce qui permet de ne pas avoir à créer d'emplacements quand on rentre dans une boucle for, while ou dowhile.

Nous avons décidé de ne pas utiliser les conventions d'appel car le code n'est pas appelé depuis l'extérieur. Cependant, la fonction `print_value` ainsi que les fonctions associées respectent les conventions d'appel pour résoudre les problèmes autour des print récursifs. De plus nous avons gardé ces conventions dans un coin de notre tête car elles nous ont permis de pouvoir manipuler `printf` et `malloc` sans devoir sauvegarder tous nos registres.

Toutes les fonctions ont été renomées sous la forme `nom_id` ce qui permet de les surcharger (dispatch multiple : chaque id est un numéro correspondant à l'instance de fonction surchargée). De plus nous sommes plutôt confiants sur l'impossibilité à l'utilisateur de réussir à créer une collision entre les identifiants qu'il peut définir et ceux utilisés par le compilateur.

Nous n'avons malheureusement pas de GC, de qui implique parfois une consommation de mémoire importante. Cependant, l'utilisation intensive de la pile pour stocker les variables nous permet de ne pas trop allouer de mémoire inutile sur le tas, lorsque le programme utilise surtout de "petites" données, i.e. entiers, flottants et booléens.

# XI] extension flottants

La gestion des flottants est une extension que nous avions prévu d'apporter au compilateur depuis la première phase de travail sur le projet.

## 1) Modifications lexer/typer

Nous avons ajouté au lexer une règle pour accepter des constantes flottantes : `(chiffre+'.' | '.'chiffre+ | chiffre+'.'chiffre+)(('e'|'E')('-'|'+')?chiffre+)?`

Nous avons en même temps ajouté à l'AST un constructeur Flottant ainsi que les règles de typage dans le typer. Par exemple, un opérateur arithmétique prenant en argument au moins une valeur flottante renverra un flottant.

## 2) Modification production de code

Là encore, il n'y eu besoin que de compléter ce que nous avions déjà mis en place pour les entiers : chaque opérateur (`+, -, *, ^, <, >, <=, >=`) est découpé en 4, en fonction de la combinaison Entier-Entier/Entier-Flottant/... qu'il a en argument. Lorsque nécessaire, les entiers sont convertis en flottants au moyen de l'instruction `cvtsi2sdq` (cf la liste de nos ajouts à `x86-64.ml`).

Un point à remarquer : il n'est pas possible de déclarer des immédiats flottants, donc les constantes flottantes sont remplacées par des variables globales, définies à la compilation dans le segment `data`.

# XII] extension arrays

Un ajout que nous souhaitions faire dans notre projet était les tableaux. En effet, des structures utilisées astucieusement permettent de se passer de tableaux, mais il est toujours plus agréable d'utiliser des vrais tableaux, avec tout le sucre syntaxique qui facilite la vie du programmeur (par exemple l'accès et la modification d'une cellule avec `a[i]`).

## 1) première itération avec des structs

La première implémentation que nous avons essayée était une implémentation simple à mettre en place :

Nous nous donnons une structure :

```julia
mutable struct List head :: Any; tail :: List end;
```

Cette structure implémente une liste chaînée (comme c'est le cas dans un des tests fournis). Nous avons implémenté en Julia les primitives `list_length`, `get_element` et `set_element` (de bêtes fonctions récursives) et nous avons ajouté au parser quelques règles pour pouvoir utiliser du sucre syntaxique : `a[i]` est remplacé au moment de l'analyse syntaxique par `get_element(a, i)`, et de même pour la mutation `a[i] = b`, remplacée par `set_element(a, i, b)`. Enfin, `[A, b, c, ...]` est remplacé par `List(a, List(b, List(c, ...)))`.

Cette première implémentation avait le bon goût d'être très simple à mettre en place (juste quelques règles à ajouter au parser ainsi que des primitives basiques en pJulia). Cependant, elle posait un défaut conséquent : Il n'était pas vraiment possible de faire des listes ayant un type fixé. Deuxièmement, et principalement, notre but était de faire des tableaux, pas des listes. On avait là une pseudo-structure de tableau qui était en fait exactement une liste chaînée, avec les avantages mais aussi les incovénients (particulièrement l'accès en temps linéaire au lie ude constant) qui viennent avec. Nous avons donc totalement refondu cette implémentation.

## 2) deuxième itération, plus propre

Notre but dans cette refonte a été de faire des arrays en bonne et due forme : taille et type fixes, accès en temps constant aux éléments. Nous avons gardé le même sucre syntaxique, étant la syntaxe universelle pour la définition, l'accès et la mutation de tableaux.

Premièrement, nous nous sommes donné une primitive, `newarray(len, val)`, qui alloue sur le tas un tableau de `len + 2` mots avec toutes les cellules initialisées à la valeur initiale `val`. Les deux premiers mots alloués sont le type et la taille du tableau. Ensuite, on stocke une donnée par mot (contrairement à 2 pour les structures; ici, le type est commun à tous les éléments et est stocké dans le premier mot du tableau, pas besoin de le dupliquer `len` fois!). Ensuite, le sucre syntaxique `a[i]` et `a[i] = b` est interprété durant l'analyse syntaxique comme une règle supplémentaire de Lvalue (cf. plus haut la définition de notre grammaire). Enfin, le sucre syntaxique `[a, b, c, ...]` est remplacé au moment du parsing par une expression de la forme :

```julia
(
.temp_array = newarray(len, a) #len est la longueur du tableau représenté par le sucre syntaxique
.temp_array[1] = b
.temp_array[2] = c
...;
.temp_array
)
```

Cela évite d'avoir à créer une primitive d'initialisation de tableau qui prenne un nombre variable d'arguments (bien que cela soit possible, de la même façon que `print`).

Un problème a été délicat : comment intégrer les arrays dans notre système de types? 

Plusieurs possibilités ont été évoquées : 
- se donner un type unique `Array` : tous les arrays, peu importe leur dimension et le type de leurs cellules, ont le même type. Cela serait simple mais ne permettrait pas de conserver des tableaux avec un type fixe : si on se donne un tableau de tableaux, on peut remplacer un de ses éléments par un tableau de n'importe quelle dimension, car il aura toujours le même type `Array`
- se donner des types explicites, par exemple `Int64 Array Array` ou bien `Float64 Array` : cela assure la bonne définition des types des arrays, mais cela complexifie beaucoup l'étape de typage.

Nous avons alors décidé de partir sur une solution intermédiaire : Lors du parsing, tous les tableaux ont un unique type `Array`. Cependant, à l'exécution, les tableaux ont un type bien défini ressemblant à `Int64 Array Array`. En effet, le type d'un tableau est de la forme `i + n * a`, où `n` est la dimension du tableau, `a` l'entier associé au type `Array` et `i` l'entier associé au type de élémentaire contenu dans le tableau (pour `Int64 Array Array`, c'est `Int64`). Ainsi, on peut vérifier à l'exécution que les mutations et accès des éléments d'un tableaux sont licites.

Quelques petites remarques sur le fonctionnement des tableaux :
- il est possible d'accéder à un élément d'un tableau via un indice négatif : `a[-1]` renvoie le dernier élément du tableau, `a[-2]` le pénultième, etc.
- lorsqu'un tableau est multi-dimensionnel, on peut accéder à ses éléments via cette syntaxe : `a[i][j]...[z]`.
- Comme dans d'autres languages, l'initialisation d'un tableau avec une valeur ne duplique pas cette dernière. Par exemple, `newarray(2, newarray(2, 0))` renvoie un tableau dont les deux éléments pointent vers le même tableau. Pour créer un tableau en profondeur, il est préférable d'utiliser la fonction `make_matrix(d, lengths, init_value)`, dans le package `matrix` (initialise un tableau multi-dimensionnel de dimension `d` où la `i`-ème dimension a une taille `lengths[i]` et dont la valeur initiale de base (la plus en profondeur) est `init_value`)
- L'opérateur `@` permet de faire des concaténations. La concaténation de deux tableaux produit un nouveau tableau, sans les détruire.


# XIII] extension strings et chars

Afin de pouvoir manipuler les string plus librement il a été décidé de les convertir en tableau de caractère. Cela a multiplié la consommation en mémoire des string par 8 (chaque caractère prend un mot de 8 octets au lieu d'un seul octet comme le prévoit la norme UTF-8). Cependant cela nous permet d'avoir accès à tous les avantages qu'un tableau nous fournit : 
- concaténation
- mutabilité
- récupération d'une valeur à un indice précis
- calcul de la longeur

Il a aussi été décidé que les types String et le type Array seraient les mêmes à l'intérieur du typeur, notamment pour la surcharge de fonction.

Remarque : Comme nous utilisons nos propres chaînes de caractères, les chaînes définies à la compilation ne sont plus déclarées dans le segment `data` mais sont construites caractère par caractère dans le code. Cela présente l'inconvénient de générer des fichiers ASM absolument monstrueux lorsque de grosses chaînes de caractères sont définies dans un programme (cf les exemples dans le packaghe `brainfuck`). Cependant, les avantages apportés par notre implémentation nous semblent très rentables!

# XIV] extensions des primitives (input_int, delay, timestamp, typeof, int, float, input_string, etc.) + erreurs

La liste de toutes les primitives avec leurs types possible est défini en bas du typeur dans la fonction `resetFE`.

* `int` : Cette fonction convertit son argument (entier, flottant, booléen ou caractère) vers un entier, éventuellement en arrondissant à l'entier inférieur.

* `float` : Cette fonction convertit son argument (entier ou flottant) vers le flottant correspondant

* `char` : Cette fonction convertit son argument (entier ou caractère) vers le caractère correspondant

* `sqrt` : Cette fonction renvoie la racine carrée (sous forme d'un flottant) de son argument entier ou flottant

* `input_int` : Cette fonction permet de lire un entier sur l'entrée standard.

* `delay` : Cette fonction déclenche une pause de l'exécution du programme. L'argument est en secondes. /!\ la compatibilité n'est pas tout à fait bonne, comme cette fonction utilise un *syscall*. Elle a été testée et est fonctionnelle sous Ubuntu 20 mais ne semble pas marcher sur MacOS car les syscall n'y sont pas les même.

* `timestamp` : Cette fonction renvoie le timestamp actuel de la machine, lu sur le registre TSC. Cela peut être utile pour des fonctions de monitoring et/ou mesure de performances.

* `typeof` : Cette fonction renvoie le type de son argument, sous forme d'un entier.

# XV] Ajouts divers

## 1) docstrings et utilisation dans le REPL

Nous avons ajouté le support des docstrings à petitJulia : chaque fonction peut être précédée d'une docstring, entre `"""`. Elle est conservée sous la forme d'une chaîne de caractère dans l'AST et est affichée lorsque l'utilisateur entre `? nom_fonction` dans le REPL.

## 2) assert

Comme nous avons rajouté un certain nombre de fonctionnalités à notre langage, il nous a semblé important de pouvoir faire des tests automatisés pour s'assurer, à tout moment, que toutes nos fonctionnalités marchent correctement. C'est pourquoi nous avons implémenté un méchanisme d'assertions. Une assertion est déclarée dans un programme par le mot-clé `assert` suivi d'une expression. À l'exécution, si l'expression s'évalue à `true`, rien ne se passe. Autrement, le programme échoue et affiche une `assertionError`, contenant le nom du fichier où l'assertion a échoué ainsi que sa ligne (le nom et la ligne sont ajoutés dans l'AST au moment du parsing puis "hardcoded" dans l'exécutable).

Nous avons ajouté dans la bibliothèque standard un paquet `tester` qui contient quelques tests (encore incomplet, il contient surtout les tests des différentes comparaisons entier/entier, entier/flottant, flottant/flottant et de quelques opérations sur les arrays)

## 3) analytics

Nous avons ajouté au compilateur un petit compteur de performances : lorsque le drapeau `-analytics` est passé à `pjuliac`, le compilateur affiche à l'issue de la compilation quelques éléments d'analyse de performances très simples : le nombre de labels utilisés, selon leur type (`for`, `while` ou `if`), le nombre d'instructions `call` et le nombre d'appels à `malloc` présents dans le fichier généré. Nous aimerions également ajouter dans le futur le temps et la mémoire qui ont été nécessaires lors de la compilation.

# XVI] État de l'interpréteur

Étant bien occupés par la compilation, nous n'avons pas eu le temps de retravailler l'interpréteur. Il n'a donc pas été modifié depuis le premier rendu et ne supporte donc pas tous nos ajouts (tableaux, nouvelles chaînes de caractères, primitives, etc.). Cependant, il supporte partiellement les flottants.

Nous avons préféré nous concentrer sur la compilation. En effet, il nous semblait plus intéressant (et c'est un plus grand défi) d'ajouter de nouvelles fonctionnalités au compilateur en priorité, la mise à jour de l'interpréteur étant en soi une tâche assez facile, mais qui prend du temps, et nous avons préféré consacrer ce temps au paufinement du fonctionnement du compilateur!


# XVII] Conclusion

## 1) Partie commune

Ce projet a été pour nous l'occasion de découvrir le domaine de la programmation bas-niveau et de la compilation. 

L'étape de la génération de code a été très intéressante et l'occasion de tester "pour de vrai" les capacités de notre compilateur. Nous nous sommes pris au jeu d'ajouter à petitJulia le plus possible de fonctionnalités plus ou moins utiles ou anecdotiques! 

Cependant, il y a quelques éléments supplémentaires que nous aurions aimé ajouter à notre compilateur mais qui n'ont pas pu être réalisés par manque de temps : 

* Affichage de structures : le temps nous a tout simplement manqué et nous avons trouvé plus intéressant d'implémenter l'affichage de tableaux.
* Support de LLVM : cela aurait permis de gagner en performance et en portabilité. Cependant nous ne connaissions pas du tout le fonctionnement de cette plateforme et il nous aurait fallu du temps pour comprendre son fonctionnement!
* GC : un GC, même basique, aurait fait de notre compilateur un vrai compilateur utilisable en pratique. En effet, même si la consommation de mémoire des exécutables générés par notre compilateur sur les exemples fournis reste raisonnable, il nous paraît évident que certaines situation ferait exploser cette consommation du fait de l'absence de GC. Par exemple si une fonction instantiant une structure était appelée un grand nombre de fois.
* Nous avions évoqué la possibilité de faire un compilateur JIT. Nous nous sommes rendus compte que c'est une chose difficilement faisable en OCaml (ou bien nous n'avons pas trouvé les bonnes ressources) autrement que via la plateforme LLVM, que nous n'avions déjà pas le temps d'utiliser dans notre projet.
* La mise à jour de l'interpréteur. Cf plus haut
* Opérateurs `+=`, `-=` et `*=`. Ces opérateurs peuvent être utiles pour factoriser un peu du code!
* L'opérateur `/` pour la division, en plus de `div`.
* Pourquoi pas les entiers et flottants sur 128 bits et les caractères non UTF-8.


## 2) Constantin

## 3) Samuel

Ce projet fut pour moi à la fois un grand plaisir et une grande frustration. On a pus faire tellement de choses mais il en reste tellement à faire. J'aurais bien aimé rajouter au compilateur un GC, notre propre générateur d'analyseur lexicale ainsi qu'un compilateur optimisant plutôt que le notre qui n'est pas du tout optimisé et ne prend pas en compte les type calculé par le typeur. Il y a aussi une envie de refaire samenhir pour réussir à obtenir un code encore plus optimisé en terme de taille. Et aussi diminuer le temps de compilation. Malheureusement le temps nous as manqué. Notamment à cause des projets dans les autres cours d'informatique.


# XVIII] Annexes



## A] Drapeaux de pjuliac

* `-print_abstract` : affiche la syntaxe abstraite du fichier en utilisant `ppx_deriving`
* `--parse_only` : arrête l'exécution après avoir parsé le fichier
* `--type_only` : arrête l'exécution après avoir typé le fichier
* `--show_file_name` : affiche le nom du fichier si il ne plante pas à l'exécution
* `-analytics` : affiche à l'issue de la compilation quelques informations plus ou moins utiles à propos de cette dernière.
* `-o` : permet de choisir le nom du fichier assembleur

## B] Contenu de la bibliothèque standard

Voilà la liste des paquets disponiblessur le repo `pjulia-packages`, téléchargeables vie pPkg. Les paquets annotés "**(cf démo)**" seront présentés durant la démonstration en visio (donc il peut être préférable de ne pas trop se divulgâcher ces surprises!).

* `acker` : Implémentation de la fonction d'Ackerman
* `arithlib` : Fonctions arithmétiques
* `asciiFluid` : Implémentation d'[ASCIIfluid](https://youtu.be/QMYfkOtYYlg) **(cf démo)**
* `brainfuck` : Interpréteur Brainfuck **(cf démo)**
* `gol` : Jeu de la vie **(cf démo)**
* `list` : Implémentation d'une structure de liste chaînée et quelques primitives
* `matrix` : Primitives utiles pour les tableaux
* `pi` : Autour du nombre Pi **(cf démo)**
* `random` : Génération pseudo-aléatoire de nombres entiers et de flottants
* `tester` : Fichier pour tester le bon fonctionnement de notre production de code

## C] Nos ajouts à x86-64.ml
Nous avons ajouté quelques fonctionnalités à la bibliothèque `x86-64.ml` :
* la détection automatique du type d'OS : Linux ou MacOS. Elle sert dans certains cas de figure, par exemple pour `lab`.
* les registres flottants SSE 64 bits : les registres `%xmm0` à `%xmm15`.
* des opérations sur les flottants : par exemple `cvtsi2sdq`, instruction de conversion d'entier vers flottant, ainsi que les instruction de comparaison de flottants
* des instructions spéciales comme par exemple `rdtsc` pour la lecture du registre de timestamp ou bien `syscall` pour réaliser des appels système.


## D] Liste des fichiers
Ci-dessous sont listés les fichiers du projet, accompagnés d'une brève description de leur utilité.

* `ast.ml` : déclaration des types récursifs de l'arbre abstrait du programme.
* `astinterp.ml` : déclaration des types utilisés lors de l'interprétation.
* `astype.ml` : déclaration des types utilisés lors du typage.
* `compilNaif.ml` : génération de code
* `compilRef.ml` : déclaration de variables utiles lors de la génération de code
* `dune` : déclaration des directives de compilation (utilsé pour intégrer Samenhir!).
* `dune-project` : déclarations annexes de `dune`.
* `hyper.ml` : fichier contenant le code `OCaml` utilisé par le Lexer.
* `hyper2.ml` : quelques fonctions utilisées dans le Parser.
* `interp.ml` : fichier contenant toutes les fonctions d'interprétation de PetitJulia™.
* `lexer.mll` : déclaration du lexer (l'analyseur est construit par ocamllex).
* `logo.ml` : fichier contenant le joli logo coloré affiché au lancement du REPL.
* `logo.png` : logo de PetitJulia™
* `Makefile` : fichier principal de compilation. Il contient plein d'options.
* `parser.sam` : parser pour Samenhir.
* `parserExemple.sam` : exemple de petit parser si vous avez envie de voir la syntaxe demandé par Samenhir.
* `parserMenhir.mly` : ancien parser qu'utilisait l'outils Menhir.
* `pjuliac.ml` : fichier principal du compilateur. Il peut prendre plusieurs flags.
* `pjuliarepl.ml` : fichier principal du REPL. Il peut être utilisé tel quel ou bien avec `rlwrap` via le script ci-dessous!
* `pjuliarepl-rlwrap.sh` : petit script `bash` pour lancer le REPL en utilisant `rlwrap` avec les options que nous avons choisies
* `pjulia-words` : fichier contenant les mots-clé du langage, pour la complétion automatique dans le REPL
* `samenhir.ml` : fichier servant d'interface de Samenhir.
* `samenhir-utilities.ml` : le corps et l'âme de Samenhir.
* `samenhirAst.ml` : définission des types et structures utiles à Samenhir.
* `samenhirLexer.ml` : lexer de Samenhir (donné à ocamllex).
* `samenhirParserBuilder.ml` : constructeur du parseur de Samenhir.
* `typer.ml` : fichier principal de typage
* `x86_64.ml` : fichier contenant les déclarations de base nécessaires à la génération de code `x86_64`, détecte automatiquement si l'ordinateur est un mac ou non.
