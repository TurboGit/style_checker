
all:
	mkdir -p obj
	gnatmake -Pstyle_checker

clean:
	gnatclean -Pstyle_checker
	make -C regtests clean
	rm -f style_checker.tar* *~

distrib:
	git archive --prefix=style_checker/ HEAD | gzip > style_checker.tar.gz

force:

regtests: force
	make -C regtests
