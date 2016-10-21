@echo off
if exist Output.txt del Output.txt
echo -
echo -
echo - Copying Hump Yard file
copy P:\CS-1520-Ada\Lab09\Hump_Yard.adb Hump_Yard.adb
echo -
echo - Compiling to produce listings of Queue package files
gcc -c -gnatcl -gnatwcdfgkmruvwz -gnaty3aAeiklM110rtx Unbounded_Queue.adb  > Queue.lst
echo -
echo - Compiling, binding, and linking code
gnatmake -f -gnato Hump_Yard
echo -
echo -
echo - Running Program
Hump_Yard > output.txt
echo - Please conserve paper.  Don't print until you have a working program.
echo -    You can check your output by looking at the files called Output.txt
echo -    You can check for compiler warnings by looking at the file Queue.lst
echo - 
set /p choice=Do you wish to print a copy to turn in? [Type "Yes" to print]  
if not "%choice%"=="Yes" goto end
echo -
echo - Printing results
C:\enscript\enscript --landscape --pretty-print=ada -fCourier9  Queue.lst output.txt
echo - Pick up your output from the printer
echo -
:end
echo -
echo - Deleting all .ali, .o, and .exe files
gnatclean *
echo -
echo - All done