tc: yac lex 
	mlton src/tc.mlb

all: yac lex
	mlton tiger/tiger.mlb

ast: tiger/ast.sml target/mips.sml src/tc.sml 
	mlton tiger/ast.sml
	mlton target/mips.sml
	mlton src/tc.sml

yac :
	mlyacc tiger/tiger.grm 

lex:
	mllex tiger/tiger.lex

clean: 

	rm tiger/tiger.grm.sml
	rm tiger/tiger.grm.desc
	rm tiger/tiger.grm.sig
	rm tiger/tiger.lex.sml
	rm tiger/tiger
	rm src/tc

other:
	rm tiger/ast
	rm target/mips
	rm src/tc 