VERSION     = $(shell git describe --abbrev=0 2>/dev/null)
VERSION_ALL = $(shell git describe 2>/dev/null)

all:
	mkdir -p obj
	gnatmake -Pstyle_checker

clean:
	gnatclean -Pstyle_checker
	make -C regtests clean
	rm -f style_checker.tar* *~ src/version.ads

setup:
# If git is not present then use the version.ads provided in distrib
ifneq ("$(VERSION)", "")
	sed -e 's,\$$VERSION\$$,$(VERSION),g' \
	-e 's,\$$VERSION_ALL\$$,$(VERSION_ALL),g' src/version.tads > src/version.ads
endif

distrib:
	git archive --prefix=style_checker/ HEAD > style_checker.tar
	tar -C ../ -r --file=style_checker.tar style_checker/src/version.ads
	gzip -f style_checker.tar

force:

regtests: force
	make -C regtests
