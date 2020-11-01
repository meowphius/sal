# rules for using gnatprep where each template file builds one output file.

%.ads : %.ads.gp ; gnatprep $^ $(^D)/$(GNATPREP_SUBDIR)/$(@F) $(GNATPREP_SYMBOLS) $(GNATPREP_SWITCHES) 
%.adb : %.adb.gp ; gnatprep $^ $(^D)/$(GNATPREP_SUBDIR)/$(@F) $(GNATPREP_SYMBOLS) $(GNATPREP_SWITCHES) 

# gnatprep-clean should delete all output of gnatprep
# default gnatprep-clean so make doesn't complain when we aren't using it
gnatprep-clean ::

# end of file
