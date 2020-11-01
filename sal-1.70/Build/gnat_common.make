# common parts of makefiles using GNAT
# See Gnat_Debug/Makefile and Gnat_Release/Makefile

.PHONY : lib tests times

# ignore whitespace in diff, so we can ignore Unix vs DOS line endings
# in test output files. Note that this has to come _after_
# common_rules.make.
DIFF_OPT = -u -w

VPATH = ../../Source_Common ../../Source_Common/Test ../../Source_Gnat/Test ../../Doc
VPATH += ../../gtk_more/test

lib : all_sal.o
lib-text_io : all_sal_text_io.o

# end of file
