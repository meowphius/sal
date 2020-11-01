# top level tasks for SAL.
#
# See ../Doc/sal.html for complete installation instructions
# 
# Required environment variables:
# MAKERULES : absolute path to Makerules directory
# SAL       : absolute path to SAL directory

all : 

CVS_VERSION := 1_70
ZIP_VERSION := 1.70

.PHONY : tag zip 

include $(MAKERULES)/common_rules.make

# for source-clean (only run from here)
SOURCE_ROOT := $(SAL)

release-clean :: source-clean
	make -C debug_windows_x86_gnu -r release-clean
	make -C release_windows_x86_gnu -r release-clean
	make -C x86_gnu_windows_debug_gtk -r release-clean
	make -C Doc -r release-clean
	-find $(MAKERULES) -name "*~" -print | xargs rm -v
	rm -f *.tar*

maintainer-clean :: 
	make -C debug_windows_x86_gnu -r maintainer-clean
	make -C release_windows_x86_gnu -r maintainer-clean
	make -C Doc -r maintainer-clean

tag :
	cd ..; cvs tag -c release_$(CVS_VERSION)

TARFILE := sal-$(ZIP_VERSION).tar
ZIPFILE := $(TARFILE).gz
zip : release-clean $(ZIPFILE)

zipfile : $(ZIPFILE)

$(ZIPFILE) : force
	tar cvf $(TARFILE) -C $(MAKERULES)/.. --exclude=*.tar.gz --exclude=CVS --exclude=.cvsignore --no-anchor makerules-$(ZIP_VERSION)
	tar --append -vf $(TARFILE) -C ../.. --exclude=Packages --exclude=*.tar.gz --exclude=*.tar --exclude=CVS --exclude=.cvsignore --no-anchor sal-$(ZIP_VERSION)
	gzip $(TARFILE)

# end of file
