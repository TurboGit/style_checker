VERSION     = $(shell git describe --abbrev=0 2>/dev/null || echo devel)
VERSION_ALL = $(shell git describe 2>/dev/null || echo devel)

all:
	mkdir -p obj
	gnatmake -Pstyle_checker

clean:
	gnatclean -Pstyle_checker
	make -C regtests clean
	rm -f style_checker.tar* *~ src/version.ads

setup:
	sed -e 's,\$$VERSION\$$,$(VERSION),g' \
	-e 's,\$$VERSION_ALL\$$,$(VERSION_ALL),g' src/version.tads > src/version.ads

distrib:
	git archive --prefix=style_checker/ HEAD | gzip > style_checker.tar.gz

force:

regtests: force
	make -C regtests
