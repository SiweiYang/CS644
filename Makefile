.PHONY: joosc
joosc: grammar
	cd src; ulimit -s 10240; ghc A1.hs; mv A1 ../joosc

.PHONY: grammar
grammar: tools/jlalr/Jlr1.class tools/joos1w.bnf
	cd tools; ruby simplify.rb joos1w.bnf | java jlalr.Jlr1 > ../res/joos1w.lr1

tools/jlalr/Jlr1.class: tools/jlalr/Jlalr1.java
	javac tools/jlalr/Jlalr1.java

.PHONY: submission
submission: clean
	zip -r submission src/ tools/ res/ Makefile

clean:
	rm -rf res/joos1w.lr1
	rm -f src/*.hi
	rm -f src/*.o
	rm -f tools/jlalr/*.class
