a = 2
function bar() println(a); if false a = 3 end end
# rien à voir avec le fait d'être exécuté ou non
# la présence *statique* de l'affectation a=3 en fait une variable locale
bar()
