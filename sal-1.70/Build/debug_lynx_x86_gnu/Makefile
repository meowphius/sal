# gnu make file for debug version of SAL using GNAT
# 
# See SAL/Doc/sal.html for complete installation instructions
# 
# Required environment variables (all absolute paths to directories):
# MAKERULES : Makerules directory

# We don't make 'all_sal.o' here, because we want to check that the
# tests test everything.
# 
# All of the Text_IO packages are built and tested by the Auto_Text_IO
# makefile

#one : test_gtk_more-all_harness.run
all : tests times 

include $(MAKERULES)/common_rules.make
include $(MAKERULES)/gnat_project_rules.make
include $(MAKERULES)/auto_text_io_rules.make
include ../common.make
include ../gnat_common.make

GNAT_PROJECT = sal_test.gpr

ADA_PROJECT_PATH := $(MAKERULES)
ADA_PROJECT_PATH := $(ADA_PROJECT_PATH):$(GTKADA)/include/gtkada
export ADA_PROJECT_PATH

#
#Local Variables:
#eval: (ada-parse-prj-file "sal.adp")
#eval: (makefile-mode)
#End:
# end of file
