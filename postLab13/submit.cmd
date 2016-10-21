@echo off
if exist Output1.txt del Output1.txt
if exist Output2.txt del Output2.txt
echo -
echo -
echo - Copying test programs and tree specification files
copy P:\CS-1520-Ada\lab13\Test_File_Tree1.adb Test_File_Tree1.adb
copy P:\CS-1520-Ada\lab13\Test_File_Tree2.adb Test_File_Tree2.adb
copy P:\CS-1520-Ada\lab13\file_tree.ads file_tree.ads
echo -
echo - Deleting any tree files (IGNORE ANY WARNINGS about not finding the files)
echo -
del Tree.dat
del Header.dat
echo -
echo - Compiling to produce listings
gcc -c -gnat05 -gnatcl -gnatyeikM110 file_tree.adb  > file_tree.lst
echo -
echo - Compiling, binding, and linking code for the two test programs
gnatmake -f -gnat05 -gnato Test_File_Tree1
gnatmake -f -gnat05 -gnato Test_File_Tree2
echo -
echo -
echo - Running First Test Program
Test_File_Tree1 > output1.txt
echo - Running Second Test Program
Test_File_Tree2 > output2.txt
echo -
echo - Please conserve paper.  Don't print until you have a working program.
echo -    You can check your output by looking at the files Output1.txt and Output2.txt
echo -    You can check for compiler warnings by looking at the file called File_Tree.lst
echo - 
set /p choice=Do you wish to print a copy to turn in? [Type "Yes" to print]  
if not "%choice%"=="Yes" goto end
echo -
echo - Printing results
C:\enscript\enscript --landscape --pretty-print=ada -fCourier9  file_tree.lst output1.txt output2.txt
echo -
echo - Pick up your output from the printer
:end
echo -
echo - Deleting all .ali, .o, and .exe files
gnatclean *
echo -
echo - All done
