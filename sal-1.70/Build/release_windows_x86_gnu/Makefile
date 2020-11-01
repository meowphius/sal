# gnu make file for release version of SAL
# 
# See SAL/Doc/sal.html for complete installation
# instructions
# 
# Required environment variables (all absolute paths to directories):
# MAKERULES : Makerules directory

all : lib lib-text_io tests times 

include $(MAKERULES)/common_rules.make
include $(MAKERULES)/gnat_project_rules.make
include ../common.make
include ../gnat_common.make

GNAT_PROJECT = sal_test.gpr

export ADA_PROJECT_PATH := $(MAKERULES)

#
#Local Variables:
#eval: (ada-parse-prj-file "sal.adp")
#eval: (makefile-mode)
#End:
# end of file
