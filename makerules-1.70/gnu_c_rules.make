# gnu make rules for gnu C 

# use '=', not ':=', to allow user to override this.
GCC = gcc

DEFAULT_CFLAGS = -Wall -Wstrict-prototypes -ansi -Werror

# The library contents are considered intermediate and deleted, which
# means they are always rebuilt, so declare them secondary. Should use
# gnu make library rules.
.SECONDARY : %.o

%.o : %.c ; $(GCC) -c $(CFLAGS) $(INCLUDES) $<

# Library build rule.
lib%.a : ; ar rc lib$*.a $?

# C dependency builder rule. GNAT gcc on Windows produces backslashes,
# but that seems to work even with Cygwin make.
%.dep : %.c ; gcc -M $(CFLAGS) $(INCLUDES) $< > $*.dep

%.exe : %.o ; $(GCC) $(C_LINK_FLAGS) $^ $(C_LIBS) -o $*.exe 

clean ::
	rm -f *.o *.a *.exe

cm-clean ::
	rm -f *.dep

maintainer-clean :: cm-clean
#end of file
