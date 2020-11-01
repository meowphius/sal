echo 
rem Plain DOS script to build SAL with GNAT. It will leave all the
rem object files in the SAL\Build\Gnat_Release directory.

cd Gnat_Release
set ADA_PROJECT_PATH=..\..\..\Makerules
gnatmake -c -Psal_release.gpr all_sal

cd ..

rem that's all, folks!
