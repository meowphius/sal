# Build Docs for SAL

# use "TEX_OPTIONS=" to get verbose error messages

#one : sal_ada_letters_2004.view
all : html pdfs maxima-diffs

.PHONY : html pdfs maxima-diffs

.PRECIOUS : %.out

include $(MAKERULES)/common_rules.make
include $(MAKERULES)/latex_rules.make

DIFF_OPT += -w

VPATH = ../../Doc

LATEX_INCLUDES = $(addprefix -include-directory ,$(VPATH))

html :
	rm -f ../../Doc/Packages/*.*
	cd ../../Source_Common; gnathtml.pl -I ../Build/release_windows_x86_gnu -o ../Doc/Packages -d all_sal.adb

pdfs : sal_ada_letters_2004.pdf
pdfs : spacecraft_math.pdf

maxima-diffs : derive_quat_times_vect.diff
maxima-diffs : derive_quat_times_quat.diff
maxima-diffs : derive_vect_scalar_quat.diff
maxima-diffs : derive_angle_axis_to_rot_matrix.diff
maxima-diffs : derive_quat_times_vect_kane.diff

%.out : %.maxima
	cd $(<D); maxima -b $(<F) > $(CURDIR)/$*.out

spacecraft_math.dvi : spacecraft_math.tex math_entities.sty

maintainer-clean ::
	rm -f ../../Doc/Packages/*.*

#
#Local Variables:
#eval: (c-parse-prj-file "sal_doc.prj")
#End:
# end of file
