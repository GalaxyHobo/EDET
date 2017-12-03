# EDET

Stand-alone version of NASA public domain "Empirical Drag Estimation Toolkit."

Includes Python wrapper to write input file, run program from command line,
and plot output.

To compile (from msys in Windows): 
gfortran edetx.f -o edet.exe

If error "The program can't start because libgcc_s_dw2-1.dll is missing" occurs,
consider compiling with static link, or change PATH to include dll (typically at 
"C:\MinGW\bin").

To run: assemble an input file named "edet.in" - samples are provided for A-4F,
RA-5C, and 737-500 - see ".in" files in repo. Rename accordingly to run. To
run manually from command line, type "edet" and output will appear as "EDET.out"
A better option is to run cases via edetWrapper. Set EDET inputs (which the script
will write to to an input file) to the Python dictionary, and set the path to
the edet.exe. The script will run EDET and plot results. See EdetSamplePlots.png.




