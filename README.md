![alt text](logo.png)

# Projet compilation 2020
### Constantin Gierczak--Galle & Samuel Vivien
### Cours de compilation, Jean-Christophe Filliâtre, ENS Ulm, 2020

# I] Lexer/Parser

LA première étape a été de définir les types qui seront utilisées par les différentes étpes d'analyse. Dans un premeir temps, nous avons suivi la grammaire donnée dans le sujet, que ce soit pour la définition de l'`ast`  ou bien pour le lexer et le parser. Cela nous a donné un analyseur à peu près fonctionnel mais souffrant de dizaines de conflits (par exemple au niveau de la construction `while expr bloc`). Cela a été résolu par un remodelage du parser progressif, éléminant tous les conflits un par un!


# II] Typer


Le typeur est une étape importante du projet et certaines décisions ont été prise par rapport à ce que l'on autorisait ou non. C'est décision ont été prise en prenant en compte que l'on pouvait revenir dessus par la suite.

Pour cela le fichier est parcouru deux fois. Il s'agit des parcours 1 et 2 (vive l'originalité !):

### Parcours 1 :

Le fichier est découpé en une liste de déclaration de fonctions, déclarations de structures et expressions.

On fait le parcours en utilisant 4 environnements Map :
- un pour les variables définies
- un pour toutes les fonctions qui à tous les noms de fonctions associe l'ensemble des types (types de paramètres et type de l'image)
- un pour tous les noms de structures
- un pour tous les noms d'attribus associés à leur types, si la structure les contenant est mutable et le nom de la structure associé


Si l'on rencontre une déclaration de structure on vérifie dans l'ordre :
- que son nom n'est pas "print", "println" ou "div"
- que l'on a pas d'autre structure du même nom
- on parcours les attribus de la struture en vérifiant que les types sont bien définies et les noms pas attribué, puis on rajoute ses attribus à l'ensemble des attribus existants. Il a été décidé de ne pas autorisé un attribu d'une structure S d'être du type Struct `S` car sinon on n'arriverais pas à construire la première variable de type `S`
- on ajoute la structure à l'ensemble des fonction du même noms, ainsi que à l'ensemble des structures

Si l'on rencontre une déclaration de fonction on vérifie dans l'ordre : 
- que son nom n'est pas déjà associé à une variable
- que son nom n'est pas "print", "println" ou "div"
- que ses arguments possèdent bien un type existant et sont deux à deux disjoints
- que sont type d'image est bien existant
- rajoute la fonction à l'ensemble des fonctions du même noms en vérifiant qu'il n'existe pas déjà une autre fonction du même nom ayant exactement les mêmes types d'entrée

La première différence notable avec ce qui est demandé dans le sujet est que le typeur teste si les variables sont bien définie AVANT leur utilisation et pas uniquement si elle sont bien définies dans le même champs.




# III] Samenhir

Ce projet, si l'on se contente de rester dans le cadre restreint du sujet laisse un sentiment de vide, on a l'impression d'avoir sauté des étapes, d'avoir triché pour arriver au bout. Cette étape ainsi sauté c'est le parser! Car dans ce projet on se contente d'utiliser menhir pour faire notre parser sans comprendre réellement ce qui se passe derrière. C'est de là qu'est venu l'idée de Samenhir (contraction de Samuel et de menhir) : l'idée d'implémenter une version simplifié de menhir afin de l'utiliser dans le projet.

Actuellement Samenhir est totalement indépendant de menhir, c'est à dire capable de générer lui même son propre parser. Les tests du mercredi 2 décmebre à 23h40 affirment que Samenhir est pleinement opérationnel et qu'il valide tous les tests de syntaxe et de typage.

### Grammaire à fournir à Samenhir :

Samenhir a besoin d'une grammaire ressemblant fortement à celle demandé par menhir, cependant par soucis de simplification de l'implémentation certaines décisions ont été prises :
- Le parser doit être écrit dans un fichier .txt car en utilisant un fichier .mly menhir voulait le parser
- la première lettre du nom d'une régle doit être écrit en minuscule et la première lettre du nom d'un token en majuscule
- il est possible de mettre un bout de code au dessus (`%{ code ocaml %}`}) du parser mais pas en dessous
- une déclaration de régle nécessite de renseigner le type de la valeur de renvoie, cela permet d'éviter de faire du typage ou d'utiliser la librairies obj_magic
- en raison de la décision ci-dessus, il n'est plus nécessaire de renseigner le type de la règle de départ
```
rule<int * int>:
	| i1 = INT i2 = INT {(i1, i2)}
;
```
- pour les régles de priorité (mais qui actuellement ne sont pas utilisé ensuite), on ne peut mettre que 1 mot par régle d'associativité
- il n'est pas possible d'utiliser des outils tel que `%inline` et les différents outils `list`, `separated_list`, et cetera.

Il y a peut être d'autres spécificité lié à l'ignorance de notre part de ces contraires/opportunité lié à l'utilisation de menhir.


### Algorithme utilisé pour construire l'analyseur :

Pour pouvoir construire l'analyseur syntaxique Samenhir utilise l'algorithme présenté slides 81-82 du cours `analyse syntaxique (1/2)`.

### Inconvénients : 

Actuellement Samenhir est très peu optimisé, il faut compter 5 minutes d'exécution pour réussir à générer le parser de Petitjulia™. Puis il faut attendre une minute de plus pour compiler ce fichier. Cependant le parser ainsi généré fonctionne comme il devrait en passant tous les tests de typages ainsi que les tests de syntaxe


# IV] Interpreter/REPL

## 1) Interpreter

Tandis que l'on avançait sur la construction du compilateur, il nous a semblé utile d'implémenter un interpréteur pour pouvoir facilement tester et débugger les étapes d'analyse syntaxique et de typage. L'implémentation de cet interpréteur n'a pas été très difficile. Les quelques difficultés rencontrées avaient souvent rapport aux différences de comportement de Julia (que nous prenions comme référence pour certaines subtilités) entre le mode interpréteur et le mode compilateur


## 2) REPL

**/!\\ cette partie nécessite l'installation préalable de rlwrap**

Pour rendre l'utilisation de l'interpréteur plus intuitive et interractive, nous avons aussi implémenté un **REPL**, très semblable aux REPL OCaml ou Python (nos modèles) ou même à celui de Julia.

L'utilisation du REPL est très simple : entrer du code Petitjulia™ l'envoie vers l'interpréteur. Toutes les variables/functions/structures sont ajoutées à l'environnement global.

En plus de cela, nous avons ajouté quelques commandes spéciales :
- `#run file` permet de charger et exécuter un fichier. Cette commande diffère de la commande présente dans le REPL Julia (`include(file)`) car il nous semblait plus simple d'utiliser une syntaxe spéciale pour que le REPL puisse décider simplement s'il doit charger un fichier et le faire suivre à l'interpréteur ou bien s'il doit directement lui faire suivre l'entrée.
- `#flush` vide entièrement les environnement de typage et interprétation; un peu comme si on relançait le REPL. Cette commande est la solution à un problème que nous avons rencontré. Il peut arriver que l'utilisateur ait besoin de redéfinir une fonction précédemment définie. Les environnement de typage et d'interprétion restant les mêmes lors d'une session REPL, le typeur soulèverait une erreur de double définitio nd'une fonction, ce qui n'est pas autorisé par le langage.
- `#exit` permet de fermer proprement le REPL, au lieu de le faire sauvagement planter à grand renfort de `Ctrl+C`!

Le REPL est pourvu d'un système de récupération d'exception qui `catch` toutes les erreurs levées par l'analyse lexicale, syntaxique, le typage, l'interprétation ou bien même les erreurs levées par le système (par exemple en cas de fichier introuvable) et affiche les messages correspondant sur la sortie stantard. Cela nous permet d'avoir un REPL qui ne plante pas à la moindre faute de frappe mais affiche l'erreur et permet de corriger la commande entrée, notamment grâce à l'historique de commandes (voir ci-dessous).

Enfin, nous utilisons un wrapper, `rlwrap`, pour ajouter à notre REPL les fonctionnalités attendues d'un tel système : historique des commandes, auto-complétion, édition de la ligne courante, etc.

L'auto-complétion est assez rudimentaire : on fournit à `rlwrap` la liste des mots-clé de Petitjulia, dans le fichier `pjulia-words`; de plus, le wrapper retient  les mots utilisés, en entrée comme en sortie. Cela fait que beaucoup de mots sont retenus (plus que nécessaire) mais c'est la seule solution simple que l'on ait trouvée pour avoir une auto-complétion.

L'édition de la ligne courante et l'historique des commandes sont des fonctionnalités de base de `rlwrap` et marchent très bien. Il est possible de modifier le nombre decommandes retenus mais la valeur par défaut, 300, suffit à notre avis grandement.

Enfin `rlwrap` nous permet d'afficher le `ρjυλια>` en couleur, ce qui nous semble essentiel à tout REPL qui se respecte!

# V] Automatisation du build et des tests

En l'état actuel des choses, la compilation du compilateur (`pjuliac.exe`) et du REPL (`pjuliarepl.exe`) est gérée par `dune` via un `dune-project` commun. Elle est réalisable via noptre `Makefile`. Nous avons paramétré celui-ci pour automatiser au maximum les tests et essais des différentes parties de notre projet.

Nous retrouvons :
- `make` : construit les deux fichiers.
- ` make clean` : efface les fichiers engendrés par la compilation du projet.
- `make repl`/`make compil` : construit et exécute le fichier (le `pjuliarepl.exe` ou `pjuliac.exe`)
- `make testExec`/`make failsExec` : exécute les tests d'exécution positifs/négatifs. (dans `/test/exec/` et `/test/exec-fail`)
- `make testSyntax`/`make failsSyntax` : de même pour la syntaxe. (NB : nous utilisons aussi les tests d'exécution et de typage, ici, pour ajouter une batterie de tests positifs!)
- `make testTyping`/`make failsTyping` : de même pour le typage. (NB : nous utilisons aussi les tests d'exécution. Notre but étant de faire un typer un peu plus puissant que demandé, nous cherchons à faire en sorte que certains fichiers qui devraient planter à l'exécution plantent au typage!).

# VI] Conclusion partielle

Cette première partie du projet nous aura beaucoup occupés, d'autant plus que nous nous sommes posé des défis supplémentaires plus ou moins conséquents!

Nous avons pu mettre en place tous les outils nécessaires à la suite du projet, ainsi que d'autres outils nous permettant de l'approfondir. Samenhir n'est pas encore totalement fonctionnel (problèmes de conflits non réglés, etc.) mais devrait l'être bientôt et pourra remplacer l'outil Menhir comme parser!
