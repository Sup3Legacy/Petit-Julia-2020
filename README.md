![logo pjulia](logo.png)

# Projet compilation 2020
### Constantin Gierczak--Galle & Samuel Vivien
### Cours de compilation, Jean-Christophe Filliâtre & Basile Clément, ENS Ulm, 2020

# 0] Prérequis

Notre projet requiert l'installation, via `opam` de la bibliothèque `ppx_deriving`. + Core_kernel + Core + Yojson + cohttp-lwt-unix + cohttp-async + lwt_ssl

De plus, l'utilisation optimale du **REPL** nécessite l'installation du **wrapper** `rlwrap`, disponible via un gestionnaire standard de packages Linux (disponible aussi sur mac).

# I] Lexer/Parser

La première étape a été de définir les **types** qui seront utilisés par les différentes étapes d'analyse. Dans un premier temps, nous avons suivi la **grammaire** donnée dans le sujet, que ce soit pour la définition de l'`ast` (les types correspondaient exactement aux **règles de grammaire**) ou bien pour le **lexer** et le **parser**.

Cela nous a donné un analyseur à peu près fonctionnel mais souffrant de dizaines de **conflits** (par exemple au niveau de la construction `while expr bloc`). Cela a été résolu par un **remodelage progressif** du parser, éliminant tous les conflits un par un!

Le parser a subi un nouveau remodelage général au début de notre travail sur le typeur car nous avions oublié de transmettre les positions des token dans l'`ast`, ce qui nous empêchait de pouvoir positionner précisement les erreurs de types.

Une erreur de parser affiche une erreur de syntaxe donnant la position du dernier lexème lu avant de planter et, s'il y a suffisament peu de possibilités, il donne la liste des catégories de lexèmes autorisés à ce moment-là.

Nous avons implémenté une règle dans le lexer et le parser permetant de donner aux fonction des **docstrings**, dans le même format que `Julia`. Pour l'instant, les docstringss sont reconnues et enregistrées dans l'arbre syntaxique mais nous n'avons pas encore eu le temps d'implémenter dans le **REPL** une fonctionnalité permettant d'accéder à la docstring d'une fonction (via `help [nom fonction]`, par exemple).


# II] Typer


Le **typeur** est une étape importante du projet et certaines décisions ont été prises par rapport à ce que l'on autorisait ou non. Ces décisions ont été prises en prenant en compte le fait que nous pouvions toujours revenir modifier certains points plus tard dans le projet si besoin.

Pour cela le fichier est parcouru **deux fois**. Il s'agit des parcours 1 et 2 (vive l'originalité !):

### Parcours 1 :

**Il a été choisi que les (en fait les constructeurs de) structures seraient gérées comme des fonctions. Cela signifie que pour nous, un constructeur de structure est un appel de fonction dont l'image est la structure construite**

Le fichier est découpé en une liste de déclarations de **fonctions**, de déclarations de **structures** et de **expressions**.

On fait le parcours en utilisant **4 environnements Map** :
- un pour les **variables définies**
- un pour toutes les **fonctions**, qui à tous les noms de fonctions associe l'ensemble des types (types de paramètres et type de l'image)
- un pour tous les **noms de structures**
- un pour tous les **noms d'attributs** associés à leur types, si la structure les contenant est mutable et le nom de la structure associée.


Si l'on rencontre une **déclaration de structure**, on effectue ces actions :
- on vérifie que son nom n'est pas `print`, `println` ou `div`
- on vérifie que l'on a pas d'autre structure du **même nom**
- on parcourt les champs de la struture en vérifiant que les types sont bien définis et les noms distincts et pas déjà attribués (par exemple deux structures différentes ne peuvent pas avoir le même champ `a`), puis on rajoute ses champs à l'ensemble des champs existants. Il a été décidé de ne pas autoriser un champ d'une structure `S` à être du type Struct `S` car sinon on n'arriverait pas à construire la première variable de type `S`
- on ajoute la structure (enfin plus exactement son constructeur) à l'ensemble des fonctions du même nom, ainsi que à l'ensemble des structures

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
- si l'on rencontre une expression qui définie un nouvel environnement (boucles `for` et `while`), alors on fait un premier parcours pour récupérer l'ensemble des variables définies dans le bloc à l'intérieur (on ne descend pas dans les `for`/`while` suivants par contre). Puis on reparcourt le bloc en testant bien tous les types en commençant avec l'environnement global privé des valeurs qui seront définies par la suite (en réalité c'est beaucoup plus compliquer car il faut gérer les différences d'environnement global/local).

**La première différence notable avec ce qui est demandé dans le sujet est que le typeur teste si les variables sont bien définie AVANT leur utilisation et pas uniquement si elle sont bien définies dans le même champs. Ce qui permet de faire planter les tests dans exec-fail suivant : `undef1.jl`, `undef3.jl`, `undef4.jl` et `undef5.jl`**

### Parcours 2 :

On teste si tout est **correctement typé**, notamment pour toutes les affectations, on teste si on met bien dans une variable un type compatible avec le type que possède déjà la variable.

On vérifie aussi que toutes les expressions sont bien typés.

De plus, pour chaque appel de fonction, on regarde l'ensemble des fonctions compatibles. Pour cela calcul d'abord la liste des types des expressions donné en arguments. Puis pour chaque fonction compatible on compte le nombre de `Any` dans ses types d'arguments et on récupère la liste des types des arguments dont l'expression à la position correspondante est de type `Any`. Il a été décidé que si toutes les fonctions avaient les mêmes listes et nombre alors le typage plantait car il y avait une impossibilité de savoir qu'elle fonction appeler. Cela permet notamment de faire planter le test `typing/bad/testfile-ambiguity-1.jl` (les deux fonctions on pour couple `(1,[])` à l'appel).

### Difficultés :

La principale difficulté rencontré dans le typage de Petitjulia™ se cachait dans la **portée des variables**.

# III] Samenhir

Ce projet, si l'on se contente de rester dans le cadre restreint du sujet laisse un sentiment de vide, on a l'impression d'avoir sauté des étapes, d'avoir triché pour arriver au bout. Cette étape ainsi sautée est bien sûr le **parser**! Nous nous contentons en effet dans ce projet d'utiliser `menhir` pour faire notre parser sans comprendre réellement ce qui se passe derrière. C'est de là qu'est venue l'idée de **Samenhir** (`NDLR :` contraction de "Menhir" et de "Samuel", ce dernier étant à l'origine de cette idée et le principal responsable et développeur de cet outil qui a multiplié le temps de compilation du projet par plus de `100`!) : l'idée d'implémenter une version simplifiée de `menhir` afin de l'utiliser dans le projet.

Actuellement **Samenhir** est totalement indépendant de `menhir`, c'est à dire capable de générer lui même son propre parser. Les tests du mercredi 2 décmebre à 23h40 affirment que Samenhir est pleinement opérationnel (`NDLR :` je précise que, dixit son créateur, "Samenhir semble marcher dans le cas particulier de petitjulia; je ne peux affirmer qu'il soit correct dans le cas général" :) ) et qu'il valide tous les tests de syntaxe et de typage.

### Grammaire à fournir à Samenhir :

**Samenhir** a besoin d'une grammaire ressemblant fortement à celle demandée par `menhir`. Cependant, par soucis de simplification de l'implémentation, certaines décisions ont été prises :
- Le parser doit être écrit dans un fichier `.sam` afin de différentier d'un fichier `.mly` car la syntaxe n'est pas entièrement compatible avec `Menhir` et `Ocamlyacc`
- la première lettre du nom d'une règle doit être **minuscule** et la première lettre du nom d'un token **majuscule**
- il est possible de mettre un bout de code au dessus (`%{ code ocaml %}`}) du parser mais pas en dessous
- une déclaration de règle nécessite de renseigner le **type de renvoi de la règle**. Cela permet d'éviter de faire nous-mêmes de l'**inférence de type** ou d'utiliser le module `Obj`, comme ceci :
```ocaml
rule<int * int>:
	| i1 = INT i2 = INT {(i1, i2)}
;
```
- en raison de la décision ci-dessus, il n'est plus nécessaire de renseigner le type de la règle de départ
- pour les **règles de priorité** (mais qui actuellement ne sont pas utilisées par Samenhir), on ne peut mettre qu'un seul' mot par **règle d'associativité**
- il n'est pas possible d'utiliser des outils tel que `%inline` ainsi que `list`, `separated_list`, etc.

Il y a peut être d'autres points de divergence entre **Samenhir** et `Menhir` liés à l'ignorance de notre part de certains spécificitées technique de `Menhir`!


### Algorithme utilisé pour construire l'analyseur :

Pour pouvoir construire l'**analyseur syntaxique**, **Samenhir** utilise l'**algorithme de Knuth** présenté slides 81-82 du cours `analyse syntaxique (1/2)`.

### Inconvénients :

Actuellement, **Samenhir** est très **peu optimisé**. Son utilisation dans le projet ralonge fortement la durée de compilation du compilateur de Petitjulia™. Cependant, le parser ainsi généré fonctionne comme il devrait, en **passant tous les tests** de typage ainsi que les tests de syntaxe.

Nous avons aussi la certitude que **Samenhir** n'est _pas entièrement correct_. Il manque de nombreuses sécurités par rapport aux différentes possibilités d'utilisations frauduleuses par un utilisateur De plus, il n'y as pas de typeur (on a considéré qu'un seul typeur dans le projet était suffisant). Cependant, le compilateur `pjuliac` utilise le fichier `parser.ml` généré par Samenhir et arrive à passer tous les tests de typages et de syntaxe. On part donc du principe suivant : `ça ne plante pas donc ça marche !`™.

Ces inconviénients sont faibles par rapport à la satisfaction personnelle d'utiliser un outil que l'on a développé soi-même plutôt que se reposer sur le travail de quelqu'un d'autre!

# IV] Interpreter/REPL

## 1) Interpreter

Tandis que l'on avançait sur la construction du **compilateur**, il nous a semblé utile d'implémenter un **interpréteur** pour pouvoir facilement tester et débeuguer les étapes d'analyse syntaxique et de typage. L'implémentation de cet interpréteur n'a pas été très difficile et est calquée sur l'implémentation de l'interpréteur `Mini-Python` que nous avons vu en début d'année. Les quelques difficultés rencontrées avaient souvent rapport aux **différences de comportement de Julia** (que nous prenions comme référence pour certaines subtilités) entre le **mode REPL** et le **mode compilateur**; et cela d'autant plus que nous avons assez longuement hésité sur le mode à adopter : d'un côté cet interpréteur nous sert à tester le comportement de notre compilateur, donc il devrait avoir exactement le même comportement. De l'autre côté, un **REPL** (_cf._ ci-dessous) qui a un comportement de compilateur n'est pas très logique!

Ainsi, pour l'instant, nous avons un interpréteur qui fonctionne uniquement en mode **REPL** mais nous prévoyons d'éventuellement lui ajouter un mode "compilateur", pour répliquer le comportement attendu de ce dernier, afin de nous aider à le concevoir et à le débeuguer!


## 2) REPL

**/!\\ cette partie nécessite l'installation préalable de rlwrap**

**Version courte :** le **REPL** peut être exécuté directement (c'est le fichier `pjuliarepl`) ou via le script `pjuliarepl-rlwrap.sh` pour bénéficier des **fonctionnalités** de `rlwrap`!

Pour rendre l'utilisation de l'interpréteur plus intuitive et interactive, nous avons aussi implémenté un **REPL**, très semblable aux **REPL** `OCaml` ou `Python` (nos modèles) ou même à celui de `Julia`.

L'utilisation du **REPL** est très simple : entrer du code Petitjulia™ l'envoie vers l'interpréteur. Toutes les variables/functions/structures sont ajoutées à l'environnement global.

En plus de cela, nous avons ajouté quelques commandes spéciales :
- `#run file` permet de charger et exécuter un **fichier**. Cette commande diffère de la commande présente dans le REPL Julia (`include(file)`) car il nous semblait plus simple d'utiliser une **syntaxe spéciale** pour que le **REPL** puisse décider simplement s'il doit charger un fichier et le faire suivre à l'interpréteur ou bien s'il doit directement lui faire suivre l'entrée.
- `#flush` vide entièrement les **environnements de typage et interprétation**; un peu comme si on relançait le **REPL**. Cette commande est la solution à un problème que nous avons rencontré. Il peut arriver que l'utilisateur ait besoin de **redéfinir une fonction** précédemment définie. Les environnement de typage et d'interprétion restant les mêmes lors d'une session REPL, le typeur soulèverait une erreur de **double définition** d'une fonction, ce qui n'est pas autorisé par le langage.
- `#exit` permet de **fermer proprement le REPL**, au lieu de le faire sauvagement planter à grand renfort de `Ctrl+C`!
- Il y a d'autres **commandes easter-eggs** cachées dans le code; leur recherche est laissée comme exercice au lecteur :)

Le **REPL** est pourvu d'un système de **récupération d'exception** qui `catch` toutes les erreurs levées par l'analyse **lexicale**, **syntaxique**, le **typage**, l'**interprétation** ou bien même les erreurs levées par le **système** (par exemple en cas de fichier introuvable) et affiche les messages correspondant sur la sortie stantard. Cela nous permet d'avoir un **REPL** qui ne plante pas à la moindre faute de frappe mais affiche l'erreur et permet de corriger la commande entrée, notamment grâce à l'**historique de commandes** (voir ci-dessous).

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

Cependant, nous nous sommes souvenus que le **REPL** de `Julia` "triche" un peu, en cela qu'il compile à la volée le code pour ensuite l'exécuter dans le processur courant. Nous avons refait le même test, cette fois-ci en appelant le **REPL** `Julia` de cette façon `julia --compile=no`. Le résultat est devenu beaucoup plus intéressant : notre interpréteur ne met plus qu'environ **10% plus longtemps** pour calculer `ackermann(3,8)`, ce qui nous paraît être un très bon premier résultat!

Nous n'avons malheureusement pas encore eu le temps d'effectuer des **tests de performances** plus poussés et plus fiables, donc ce résultat est à prendre avec un peu de recul, et cela d'autant plus que le langage Julia est beaucoup plus complexe que notre petit fragment. On peut ainsi raisonnablement s'attendre à ce que l'**interpréteur Julia** ait des étapes d'interprétation supplémentaires (Par exemple la gestion des opérateurs `+ - *` qui sont surchargés du fait de la présence de flottants dans Julia) par rapport à notre interpréteur basique. Ces étapes allongeant plus ou moins le temps pris par l'interprétation, notre succès est un peu moindre.


# V] Automatisation du build et des tests

En l'état actuel des choses, la compilation du compilateur (`pjuliac.exe`) et du REPL (`pjuliarepl.exe`) est gérée par `dune` via un `dune-project` commun. Elle est réalisable via notre `Makefile`. Nous avons paramétré celui-ci pour **automatiser** au maximum les tests et essais des différentes parties de notre projet.

Nous retrouvons :
- `make` : construit les deux fichiers, de plus pour le confort de l'utilisateur un exécutable `pjuliac` est mis dans le **répertoire courant**. Cependant cet exécutable est supprimé par la commande `make clean`. Pensez donc à le déplacer avant si vous voulez le garder après avoir nétoyé le reste du projet.
- ` make clean` : efface les fichiers engendrés par la compilation du projet.
- `make repl`/`make compil` : construit et exécute le fichier (le `pjuliarepl.exe` ou `pjuliac.exe`).
- `make testExec`/`make failsExec` : exécute les tests d'exécution positifs/négatifs. (dans `/test/exec/` et `/test/exec-fail`)
- `make testSyntax`/`make failsSyntax` : de même pour la syntaxe. (**NB :** nous utilisons aussi les tests d'exécution et de typage, ici, pour ajouter une batterie de tests positifs!)
- `make testTyping`/`make failsTyping` : de même pour le typage. (**NB :** nous utilisons aussi les tests d'exécution. Notre but étant de faire un typer un peu plus puissant que demandé, nous cherchons à faire en sorte que certains fichiers qui devraient planter à l'exécution plantent au typage!).

Le différents tests utilisent le fichier `pjuliac.exe` qui est présent dans les fichiers construits par `dune`. Ils sont donc indépendants de l'existence ou non du fichier `pjuliac`, qu'il est donc possible de supprimer/déplacer.

# VI] Conclusion partielle

Cette première partie du projet nous aura **beaucoup occupés**, d'autant plus que nous nous sommes posé des défis supplémentaires plus ou moins conséquents!

Nous avons pu mettre en place tous les outils nécessaires à la **suite du projet**, ainsi que d'autres outils nous permettant de l'approfondir.
Cependant, nous considérons nécessaire de continuer à travailler sur **Samenhir**, ne serait-ce que pour optimiser la production de code afin de **diminuer le temps de compilation** du compilateur.

Pour la deuxième partie du projet, nous projetons d'ajouter aussi à PetitJulia™ le support des **flottants** sur 64 bits ainsi que des **listes** (probablement tout simplement via des structures cachées derrière du sucre syntaxique); si nous en avons le temps!

# VII] Annexes

## A] Drapeaux de pjuliac

* `-print_abstract` : affiche la syntaxe abstraite du fichier
* `--parse_only` : arrête l'exécution après avoir parsé le fichier
* `--type_only` : arrête l'exécution après avoir typé le fichier
* `--show_file_name` : affiche le nom du fichier si il ne plante pas à l'exécution

## B] Liste des fichiers
Ci-dessous sont listés les fichiers du projet, accompagnés d'une brève description de leur utilité.

* `acker.jl` : définition de la fonction ackermann en PetitJulia™.
* `ast.ml` : déclaration des types récursifs de l'arbre abstrait du programme.
* `astinterp.ml` : déclaration des types utilisés lors de l'interprétation.
* `astype.ml` : déclaration des types utilisés lors du typage.
* `dune` : déclaration des directives de compilation (utilse pour intégrer Samenhir!).
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
* `test.jl` : petit fichier servant à tester notre compilateur (pratique car directement dans notre environnement de compilation).
* `typer.ml` : fichier principal de typage
* `x86_65.ml` : fichier contenant les déclarations de base nécessaires à la génération de code `x86_64`, détecte automatiquement si l'ordinateur est un mac ou non.
