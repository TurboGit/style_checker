############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2008, Pascal Obry                      #
#                                                                          #
#  This library is free software; you can redistribute it and/or modify    #
#  it under the terms of the GNU General Public License as published by    #
#  the Free Software Foundation; either version 2 of the License, or (at   #
#  your option) any later version.                                         #
#                                                                          #
#  This library is distributed in the hope that it will be useful, but     #
#  WITHOUT ANY WARRANTY; without even the implied warranty of              #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       #
#  General Public License for more details.                                #
#                                                                          #
#  You should have received a copy of the GNU General Public License       #
#  along with this library; if not, write to the Free Software Foundation, #
#  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          #
#                                                                          #
############################################################################

VERSION     = $(shell git describe --abbrev=0 2>/dev/null)
VERSION_ALL = $(shell git describe 2>/dev/null)

all: setup
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
