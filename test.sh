#!/bin/bash

shopt -s nullglob

# script de test pour le projet de compilation

option=$1
compilo=$2
score=0
max=0
verbose=0


echo "Test de $2"

echo

compile () {
if [[ $verbose != 0 ]]; then
  echo Compile $1 $2
  $compilo $1 $2;
else
  $compilo $1 $2 > /dev/null 2>&1;
fi;
}


##############################################################################
# partie 1 : tests d'analyse syntaxique
##############################################################################

partie1 () {

score=0
max=0

echo "Partie 1"

# les mauvais
echo -n "mauvais "
for f in test/syntax/bad/*.jl; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"0")
	echo
	echo "ECHEC sur "$f" (devrait �chouer)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

# les bons
echo -n "bons "
for f in test/syntax/good/*.jl test/typing/bad/*.jl test/typing/good/*.jl test/exec/*.jl test/exec-fail/*.jl; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"1")
	echo
	echo "ECHEC sur "$f" (devrait reussir)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo -n "Partie 1: $score/$max : $percent%"; }

##############################################################################
# partie 2 : tests d'analyse s�mantique
##############################################################################

partie2 () {
echo
echo "Partie 2"

score=0
max=0

# les mauvais
echo -n "mauvais "
for f in test/typing/bad/*.jl; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
    case $? in
	"0")
	echo
	echo "ECHEC sur "$f" (devrait �chouer)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

# les bons
echo -n "bons "
for f in test/typing/good/*.jl test/exec/*.jl test/exec-fail/*.jl; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
    case $? in
	"1")
	echo
	echo "ECHEC sur "$f" (devrait reussir)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo    "Partie 2: $score/$max : $percent%";
}


##############################################################################
# partie 3 : tests d'ex�cution
##############################################################################
partie3 () {

score_comp=0
score_out=0
score_test=0
max=0

echo
echo "Partie 3"
echo "Execution normale"
echo "-----------------"

timeout="why3-cpulimit 30 0 -h"

for f in test/exec/*.jl; do
    echo -n "."
    asm=test/exec/`basename $f .jl`.s
    rm -f $asm
    expected=test/exec/`basename $f .jl`.out
    max=`expr $max + 1`;
    if compile $f; then
	rm -f out
	score_comp=`expr $score_comp + 1`;
	if gcc -no-pie $asm && ./a.out > out; then
	    score_out=`expr $score_out + 1`;
	    if cmp --quiet out $expected; then
		score_test=`expr $score_test + 1`;
	    else
		echo
		echo "ECHEC : mauvaise sortie pour $f"
	    fi
	else
		echo
		echo "ECHEC du code produit pour $f"
	fi
    else
	echo
	echo "ECHEC de la compilation sur $f (devrait r�ussir)"
    fi
done
echo

echo "Execution conduisant � un �chec"
echo "-------------------------------"

for f in test/exec-fail/*.jl; do
    echo -n "."
    asm=test/exec-fail/`basename $f .jl`.s
    rm -f $asm
    max=`expr $max + 1`;
    if compile $f && gcc -no-pie $asm; then
	score_comp=`expr $score_comp + 1`;
	if ./a.out > out; then
	    echo
	    echo "ECHEC : devrait �chouer sur $f"
	else
	    score_test=`expr $score_test + 1`;
	    score_out=`expr $score_out + 1`;
	fi
    else
	echo
	echo "ECHEC de la compilation sur $f (devrait r�ussir)"
    fi
done

echo
percent=`expr 100 \* $score / $max`;

echo "Partie 3:";
percent=`expr 100 \* $score_comp / $max`;
echo "Compilation : $score_comp/$max : $percent%";
percent=`expr 100 \* $score_out / $max`;
echo "Code produit : $score_out/$max : $percent%";
percent=`expr 100 \* $score_test / $max`;
echo "Comportement du code : $score_test/$max : $percent%";}

##############################################################################
interp () {

score_test=0
max=0

echo
echo "Interpr�te"
echo "Execution normale"
echo "-----------------"

timeout="why3-cpulimit 30 0 -h"

for f in test/exec/*.jl; do
    echo -n "."
    expected=test/exec/`basename $f .jl`.out
    max=`expr $max + 1`;
    rm -f out
    if $compilo $f > out; then
	if cmp --quiet out $expected; then
	    score_test=`expr $score_test + 1`;
	else
	    echo
	    echo "ECHEC : mauvaise sortie pour $f"
	fi
    else
	echo
	echo "ECHEC de l'interpr�tation sur $f"
    fi
done
echo

echo "Execution conduisant � un �chec"
echo "-------------------------------"

for f in test/exec-fail/*.jl; do
    echo -n "."
    max=`expr $max + 1`;
    if $compilo $f > /dev/null 2>&1; then
	echo
	echo "ECHEC : le code $f devrait �chouer"
    else
	score_test=`expr $score_test + 1`;
    fi
done

echo
percent=`expr 100 \* $score / $max`;

echo "Interpr�te:";
percent=`expr 100 \* $score_test / $max`;
echo "Comportement du code : $score_test/$max : $percent%";}

case $option in
    "-1" )
        partie1;;
    "-2" )
        partie2;;
    "-3" )
        partie3;;
    "-v1" )
	verbose=1;
	partie1;;
    "-v2" )
    	verbose=1;
        partie2;;
    "-v3" )
    	verbose=1;
        partie3;;
    "-all" )
    	partie1;
    	partie2;
    	partie3;;
    "-i" )
        interp;;
    * )
        echo "usage : $0 <option> <compilo>"
        echo "sp�cifier une option parmi : "
        echo "-1      : tester la partie 1"
        echo "-2      : tester la partie 2"
        echo "-3      : tester la partie 3"
        echo "-i      : tester avec un interpr�te"
        echo "-v1     : tester la partie 1 (verbose)"
        echo "-v2     : tester la partie 2 (verbose)"
        echo "-v3     : tester la partie 3 (verbose)"
        echo "-all    : tout tester";;

esac
echo
