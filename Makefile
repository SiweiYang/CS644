src/res/joos1w.lr1: tools/jlalr/Jlr1.class tools/joos1w.bnf
	cd tools; ./simplify.rb joos1w.bnf | java jlalr.Jlr1 > ../src/res/joos1w.lr1

tools/jlalr/Jlr1.class: tools/jlalr/Jlalr1.java
	javac tools/jlalr/Jlalr1.java

clean:
	rm -rf src/res/joos1w.lr1
