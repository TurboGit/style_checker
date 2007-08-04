
all:
	gnatmake -Pstyle_checker

clean:
	gnatclean -Pstyle_checker
	make -C regtests clean
	rm -f style_checker.tar* *~

distrib:
	(cd ..; \
	ln -s trunk style_checker;\
	tar --exclude=".svn" --create --dereference \
		--file=style_checker.tar style_checker/;\
	rm -f style_checker)
	mv ../style_checker.tar .
	gzip -9 style_checker.tar

force:

regtests: force
	make -C regtests
