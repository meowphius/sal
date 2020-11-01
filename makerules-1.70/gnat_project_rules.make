# gnu make rules for GNAT, using project files.

# We use GNAT project files; standard settings are in
# 'standard_release.gpr' and 'standard_debug.gpr'. Objects,
# executables and test outputs go in the directory with the Makefile
# and project file.

.PHONY : force maintainer-clean register

# The C compilation results are considered intermediate and deleted,
# which we don't want, so declare them precious.
.SECONDARY : %.o

# Main compile and link rule Note that there is no way to specify the
# project search path on the gnatmake command line; it must be in the
# environment variable ADA_PROJECT_PATH
%.exe : %.adb force; gnatmake -k -C -P$(GNAT_PROJECT) $(GNATMAKE_ARGS) $*

# Compile individual files
%.o : %.adb force; gnatmake -k -C -c -P$(GNAT_PROJECT) $*
%.o : %.ads force; gnatmake -k -C -c -P$(GNAT_PROJECT) $*

# we occasionally link C code with Ada code. Provide the C compile and
# clean rules here, rather than forcing "include gnu_c_rules", since
# that would declare conflicting link rules.
%.o : %.c ; gcc -c $(CFLAGS) $(INCLUDES) $<
%.dep : %.c ; gcc -M $(CFLAGS) $(INCLUDES) $< > $*.dep
lib%.a : ; ar rc lib$*.a $?

ifeq ($(GNAT_VERSION), 3.15p)
clean ::
	rm -f b~*.ad? *.a *.dll *.exe *.o 
else
# gnatclean removes all compiler-generated files that the project file
# knows about; that usually does not include most test executables, so
# remove them (and their object and ali files) separately.
clean ::
	gnatclean -q -r -P$(GNAT_PROJECT)
	rm -f *.a *.o *.ali b~* *.exe
endif

local-clean ::
	gnatclean -q -P$(GNAT_PROJECT)
	rm -f *.a *.o

release-clean :: clean

# Set separator for ADA_PROJECT_PATH according to OS. This assumes we
# are running Cygwin make, not GNAT make for windows.
ifneq "$(WINDIR)" ""
# Windows/Cygwin
PATH_SEP := ;
else
# Unix
PATH_SEP := :
endif

# end of file
