# See SAL/Doc/sal.html for complete installation instructions
# 
# Required environment variables (all absolute paths to directories):
# MAKERULES : Makerules directory

#one : test_gtk_more-all_harness.run
#one : test_gtk_more-one_harness.run
all : tests times 

include $(MAKERULES)/common_rules.make
include $(MAKERULES)/gnat_project_rules.make
include ../common.make
include ../gnat_common.make

# FIXME: should inherit SAL project, do only gtk stuff here
tests :: test_gtk_more-all_harness.diff

GNAT_PROJECT = sal_test.gpr

ADA_PROJECT_PATH := $(MAKERULES)
ADA_PROJECT_PATH := $(ADA_PROJECT_PATH);$(GTKADA)/include/gtkada
export ADA_PROJECT_PATH

#
#Local Variables:
#eval: (ada-parse-prj-file "sal.prj")
#eval: (makefile-mode)
#End:
# end of file
