# gnu make rules for GNAT, not using project files

# Our standard GNAT style settings, for use in GNATMAKE_OPTS.
# This is not included in the compilation rules, because we sometimes
# compile stuff from other people that voilate it.
GNAT_STYLE = -gnaty3abefhiklM120nprt

# Disable elaboration warnings; use Elaborate_All only for program logic.
GNAT_DEBUG_DEFAULTS = -g -O1 -gnatf -gnato -gnatwa -gnatwL -gnata -gnatwe $(GNAT_STYLE)
GNAT_RELEASE_DEFAULTS = -O3 -gnatn -gnatf -gnatwa -gnatwL $(GNAT_STYLE)

GNATBIND_ARGS_DEFAULTS = -E

.PHONY : force maintainer-clean

# The C compilation results are considered intermediate and deleted,
# which we don't want, so declare them precious.
.PRECIOUS : %.o

# Main compile and link rule
%.exe : %.adb force $(GNATLINK_OBJS); gnatmake -k $(GNATMAKE_OPTS) $(INCLUDES) $* -largs $(GNATLINK_ARGS) $(GNATLINK_OBJS) -bargs $(GNATBIND_ARGS) -cargs $(GNATMAKE_CARGS)

# Compile individual files
%.o : %.adb force; gnatmake -k -c $(GNATMAKE_OPTS) $(INCLUDES) $* -cargs $(GNATMAKE_CARGS)
%.o : %.ads force; gnatmake -k -c $(GNATMAKE_OPTS) $(INCLUDES) $* -cargs $(GNATMAKE_CARGS)

# we occasionally link C code with Ada code. Provide the compile rule
# here, rather than forcing "include gnu_c_rules", since that would
# declare conflicting link rules.
%.o : %.c ; gcc -c $(CFLAGS) $(INCLUDES) $<

# for use in dll rules 
%.ali : %.adb force ; gnatmake -k -c $(GNATMAKE_OPTS) $(INCLUDES) $*
%.ali : %.ads force ; gnatmake -k -c $(GNATMAKE_OPTS) $(INCLUDES) $*

# preprocessor rules
%.ads : %.ads.gp ; gnatprep $^ $(^D)/$(GNATPREP_SUBDIR)/$(@F) $(GNATPREP_SYMBOLS) $(GNATPREP_SWITCHES) 
%.adb : %.adb.gp ; gnatprep $^ $(^D)/$(GNATPREP_SUBDIR)/$(@F) $(GNATPREP_SYMBOLS) $(GNATPREP_SWITCHES) 

# gnatprep-clean should delete all output of gnatprep
# default gnatprep-clean so make doesn't complain when we aren't using it
gnatprep-clean ::

# binding Ada for non-Ada main
# no -gnaty; binder violates it!
b~%.adb :
	gnatbind -n $(INCLUDES) -o $@ $(GNATBIND_ARGS) $^
	gcc -c -gnatf -gnato -g -O1 $(INCLUDES) $@

clean ::
	rm -f b~*.ad? *.a *.dll *.exe *.o *.res 

maintainer-clean :: clean gnatprep-clean
	rm -f *.ali *.adt

#end of file
