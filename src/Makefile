all:pjuliac.exe pjuliarepl.exe
	echo "Built everything! \n"

compil: pjuliac.exe
	dune exec ./pjuliac.exe test.jl

repl: pjuliarepl.exe
	clear
	rlwrap -f pjulia-words -c -pCyan dune exec ./pjuliarepl.exe

pjuliac.exe:
	dune build pjuliac.exe
	cp _build/default/pjuliac.exe pjuliac

pjuliarepl.exe :
	dune build pjuliarepl.exe
	cp _build/default/pjuliarepl.exe pjuliarepl

clean:
	rm -f pjuliac
	rm -f pjuliarepl
	rm -f out.s
	dune clean

explain:
	menhir --base /tmp/parser --dump --explain parser.mly
	cat /tmp/parser.conflicts

testExec:
	for g in ../test/exec/*.jl; do ./_build/default/pjuliac.exe -show_file_name $$g; done

testSyntax:
	for g in ../test/exec/*.jl; do ./_build/default/pjuliac.exe -show_file_name --parse_only $$g; done
	for g in ../test/exec-fail/*.jl; do ./_build/default/pjuliac.exe -show_file_name --parse_only $$g; done
	for g in ../test/syntax/good/*.jl; do ./_build/default/pjuliac.exe -show_file_name --parse_only $$g; done
	for g in ../test/typing/good/*.jl; do ./_build/default/pjuliac.exe -show_file_name --parse_only $$g; done


failsSyntax:
	for g in ../test/syntax/bad/*.jl; do ./_build/default/pjuliac.exe -show_file_name --parse_only $$g; done


testTyping:
	for g in ../test/exec/*.jl; do ./_build/default/pjuliac.exe -show_file_name --type_only $$g; done
	for g in ../test/exec-fail/*.jl; do ./_build/default/pjuliac.exe -show_file_name --type_only $$g; done
	for g in ../test/typing/good/*.jl; do ./_build/default/pjuliac.exe -show_file_name --type_only $$g; done


failsTyping:
	for g in ../test/typing/bad/*.jl; do ./_build/default/pjuliac.exe -show_file_name --type_only $$g; done

failsExec:
	for g in ../test/exec-fail/*.jl; do ./_build/default/pjuliac.exe -show_file_name $$g; done
