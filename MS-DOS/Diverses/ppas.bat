@echo off
SET THEFILE=test(1)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s1.o -w-orphan-labels  test.sl\test0s1.s
if errorlevel 1 goto asmend
SET THEFILE=test(2)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s2.o -w-orphan-labels  test.sl\test0s2.s
if errorlevel 1 goto asmend
SET THEFILE=test(3)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s3.o -w-orphan-labels  test.sl\test0s3.s
if errorlevel 1 goto asmend
SET THEFILE=test(4)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s4.o -w-orphan-labels  test.sl\test0s4.s
if errorlevel 1 goto asmend
SET THEFILE=test(5)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s5.o -w-orphan-labels  test.sl\test0s5.s
if errorlevel 1 goto asmend
SET THEFILE=test(6)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s6.o -w-orphan-labels  test.sl\test0s6.s
if errorlevel 1 goto asmend
SET THEFILE=test(7)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s7.o -w-orphan-labels  test.sl\test0s7.s
if errorlevel 1 goto asmend
SET THEFILE=test(8)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s8.o -w-orphan-labels  test.sl\test0s8.s
if errorlevel 1 goto asmend
SET THEFILE=test(9)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s9.o -w-orphan-labels  test.sl\test0s9.s
if errorlevel 1 goto asmend
SET THEFILE=test(10)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s10.o -w-orphan-labels  test.sl\test0s10.s
if errorlevel 1 goto asmend
SET THEFILE=test(11)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s11.o -w-orphan-labels  test.sl\test0s11.s
if errorlevel 1 goto asmend
SET THEFILE=test(12)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o test.sl\test0s12.o -w-orphan-labels  test.sl\test0s12.s
if errorlevel 1 goto asmend
c:\FPC\3.0.0\bin\i386-win32\wlib.exe -q -fo -c -b test.a  test.sl\test0s12.o test.sl\test0s11.o test.sl\test0s10.o test.sl\test0s9.o test.sl\test0s8.o test.sl\test0s7.o test.sl\test0s6.o test.sl\test0s5.o test.sl\test0s4.o test.sl\test0s3.o test.sl\test0s2.o test.sl\test0s1.o
if errorlevel 1 goto linkend
SET THEFILE=test.exe
echo Linking %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\wlink.exe  @link.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occured while assembling %THEFILE%
goto end
:linkend
echo An error occured while linking %THEFILE%
:end
