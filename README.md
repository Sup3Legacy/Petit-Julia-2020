![alt text](logo.png)

# Projet compilation 2020
### Constantin Gierczak--Galle & Samuel Vivien
### Cours de compilation, Jean-Christophe Filliâtre, ENS Ulm, 2020

# I] Lexer/Parser

# II] Typer

# III] Samenhir

# IV] Interpreter/REPL

## 1) Interpreter

Tandis que l'on avançait sur la construction du compilateur, il nous a semblé utile d'implémenter un interpréteur pour pouvoir facilement tester et débugger les étapes d'analyse syntaxique et de typage.


## 2) REPL

Pour rendre l'utilisation de l'interpréteur plus intuitive et interractive, nous avons aussi implémenté un **REPL**, très semblable aux différents REPL OCaml ou Python (nos modèles) ou même à celui de Julia.

L'utilisation du REPL est très simple : entrer du code Petitjulia $\copyright$ l'envoie vers l'interpréteur. Toutes les variables/functions/structures sont ajoutées à l'environnement global.

En plus de cela, nous avons ajouté quelques commandes spéciales :
- `#run file` permet de charger et exécuter un fichier. Cette commande diffère de la commande présente dans le REPL Julia (`include(file)`) car il nous semblait plus simple d'utiliser une syntaxe spéciale pour que le REPL puisse décider simplement s'il doti charger un fichier et le faire suivre à l'interpréteur ou bien s'il doit directement lui faire suivre l'entrée.
- `#flush` vide entièrement les environnement de typage et interprétion; un peu comme si on relançait le REPL. Cette commande est la solution à un problème que nous avosn rencontré. Il peut arriver que l'utilisateur ait besoin de redéfinir une fonction précédemment définie. Les environnement de typage et d'interprétion restant les mêmes lors d'une session REPL, le typeur soulèverait une erreur de double définitio nd'une fonction, ce qui n'est pas autorisé par le langage.
- `#exit` permet de fermer proprement le REPL, au lieu de le faire sauvagement planter à grand renfort de `Ctrl+C`!


Enfin, nous utilisons un wrapper, `rlwrap`, pour ajouter à notre REPL les fonctionnalités attendues d'un tel système : historique des commandes, auto-complétion, édition de la ligne courante, etc.

L'auto-complétion est assez rudimentaire : on fournit à `rlwrap` la liste des mots-clé de Petitjulia, dans le fichier `pjulia-words`; de plus, le wrapper retient  les mots utilisés, en entrée comme en sortie. Cela fait que beaucoup de mots sont retenus (plsu que nécessaire) mais c'est la seule solution simple que l'on ait trouvée pour avoir une auto-complétion.

L'édition de la ligne courante et l'historique des commandes sont des fonctionnalités de base de `rlwrap` et marchent très bien. Il est possible de modifier le nombre decommandes retenus mais la valeur par défaut, $300$, suffit à notre avis grandement.

Enfin `rlwrap` nous permet d'afficher le `ρjυλια>` en couleur, ce qui nous semble essentiel à tout REPL qui se respecte!

# V] Automatisation du build et des tests



# VI] Conclusion partielle
