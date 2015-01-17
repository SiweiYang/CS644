
src/res/joos1w.lr1: lib/jlalr/Jlr1.class lib/joos1w.bnf
	cd lib; ./simplify.rb joos1w.bnf | java jlalr.Jlr1 > ../src/res/joos1w.lr1

lib/jlalr/Jlr1.class: lib/jlalr/Jlalr1.java
	javac lib/jlalr/Jlalr1.java
