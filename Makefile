.PHONY: joosc
joosc: 
	cd src; ulimit -s 10240; ghc A5.hs; mv A5 ../joosc

grammar: tools/jlalr/Jlr1.class tools/joos1w.bnf
	cd tools; ruby simplify.rb joos1w.bnf | java jlalr.Jlr1 > ../res/joos1w.lr1

tools/jlalr/Jlr1.class: tools/jlalr/Jlalr1.java
	javac tools/jlalr/Jlalr1.java

.PHONY: submission
submission: clean
	zip -r submission src/ tools/ res/ Makefile
	zip -d submission.zip res/UWLogo.jpg

clean:
	rm -rf res/joos1w.lr1
	rm -f src/*.hi
	rm -f src/*.o
	rm -f tools/jlalr/*.class
	rm -f submission.zip
