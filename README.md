# EDET

Stand-alone version of NASA public domain "Empirical Drag Estimation Toolkit."

Includes Python wrapper to write input file, run program from command line,
and plot output.

To compile (from msys in Windows): gfortran edetx.f -o edet.exe
If error "The program can't start because libgcc_s_dw2-1.dll is missing" occurs,
consider compiling with static link, or change PATH to include dll (typically at 
"C:\MinGW\bin").

