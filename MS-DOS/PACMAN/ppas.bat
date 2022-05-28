@echo off
SET THEFILE=joystick(1)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s1.o -w-orphan-labels  JOYSTICK.sl\joystick0s1.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(2)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s2.o -w-orphan-labels  JOYSTICK.sl\joystick0s2.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(3)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s3.o -w-orphan-labels  JOYSTICK.sl\joystick0s3.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(4)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s4.o -w-orphan-labels  JOYSTICK.sl\joystick0s4.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(5)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s5.o -w-orphan-labels  JOYSTICK.sl\joystick0s5.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(6)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s6.o -w-orphan-labels  JOYSTICK.sl\joystick0s6.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(7)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s7.o -w-orphan-labels  JOYSTICK.sl\joystick0s7.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(8)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s8.o -w-orphan-labels  JOYSTICK.sl\joystick0s8.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(9)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s9.o -w-orphan-labels  JOYSTICK.sl\joystick0s9.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(10)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s10.o -w-orphan-labels  JOYSTICK.sl\joystick0s10.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(11)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s11.o -w-orphan-labels  JOYSTICK.sl\joystick0s11.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(12)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s12.o -w-orphan-labels  JOYSTICK.sl\joystick0s12.s
if errorlevel 1 goto asmend
SET THEFILE=joystick(13)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o JOYSTICK.sl\joystick0s13.o -w-orphan-labels  JOYSTICK.sl\joystick0s13.s
if errorlevel 1 goto asmend
c:\FPC\3.0.0\bin\i386-win32\wlib.exe -q -fo -c -b JOYSTICK.a  JOYSTICK.sl\joystick0s13.o JOYSTICK.sl\joystick0s12.o JOYSTICK.sl\joystick0s11.o JOYSTICK.sl\joystick0s10.o JOYSTICK.sl\joystick0s9.o JOYSTICK.sl\joystick0s8.o JOYSTICK.sl\joystick0s7.o JOYSTICK.sl\joystick0s6.o JOYSTICK.sl\joystick0s5.o JOYSTICK.sl\joystick0s4.o JOYSTICK.sl\joystick0s3.o JOYSTICK.sl\joystick0s2.o JOYSTICK.sl\joystick0s1.o
if errorlevel 1 goto linkend
SET THEFILE=pacman(1)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s1.o -w-orphan-labels  PACMAN.sl\PACMAN0s1.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(2)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s2.o -w-orphan-labels  PACMAN.sl\PACMAN0s2.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(3)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s3.o -w-orphan-labels  PACMAN.sl\PACMAN0s3.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(4)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s4.o -w-orphan-labels  PACMAN.sl\PACMAN0s4.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(5)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s5.o -w-orphan-labels  PACMAN.sl\PACMAN0s5.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(6)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s6.o -w-orphan-labels  PACMAN.sl\PACMAN0s6.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(7)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s7.o -w-orphan-labels  PACMAN.sl\PACMAN0s7.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(8)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s8.o -w-orphan-labels  PACMAN.sl\PACMAN0s8.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(9)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s9.o -w-orphan-labels  PACMAN.sl\PACMAN0s9.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(10)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s10.o -w-orphan-labels  PACMAN.sl\PACMAN0s10.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(11)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s11.o -w-orphan-labels  PACMAN.sl\PACMAN0s11.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(12)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s12.o -w-orphan-labels  PACMAN.sl\PACMAN0s12.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(13)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s13.o -w-orphan-labels  PACMAN.sl\PACMAN0s13.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(14)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s14.o -w-orphan-labels  PACMAN.sl\PACMAN0s14.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(15)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s15.o -w-orphan-labels  PACMAN.sl\PACMAN0s15.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(16)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s16.o -w-orphan-labels  PACMAN.sl\PACMAN0s16.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(17)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s17.o -w-orphan-labels  PACMAN.sl\PACMAN0s17.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(18)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s18.o -w-orphan-labels  PACMAN.sl\PACMAN0s18.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(19)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s19.o -w-orphan-labels  PACMAN.sl\PACMAN0s19.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(20)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s20.o -w-orphan-labels  PACMAN.sl\PACMAN0s20.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(21)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s21.o -w-orphan-labels  PACMAN.sl\PACMAN0s21.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(22)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s22.o -w-orphan-labels  PACMAN.sl\PACMAN0s22.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(23)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s23.o -w-orphan-labels  PACMAN.sl\PACMAN0s23.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(24)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s24.o -w-orphan-labels  PACMAN.sl\PACMAN0s24.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(25)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s25.o -w-orphan-labels  PACMAN.sl\PACMAN0s25.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(26)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s26.o -w-orphan-labels  PACMAN.sl\PACMAN0s26.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(27)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s27.o -w-orphan-labels  PACMAN.sl\PACMAN0s27.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(28)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s28.o -w-orphan-labels  PACMAN.sl\PACMAN0s28.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(29)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s29.o -w-orphan-labels  PACMAN.sl\PACMAN0s29.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(30)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s30.o -w-orphan-labels  PACMAN.sl\PACMAN0s30.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(31)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s31.o -w-orphan-labels  PACMAN.sl\PACMAN0s31.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(32)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s32.o -w-orphan-labels  PACMAN.sl\PACMAN0s32.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(33)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s33.o -w-orphan-labels  PACMAN.sl\PACMAN0s33.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(34)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s34.o -w-orphan-labels  PACMAN.sl\PACMAN0s34.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(35)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s35.o -w-orphan-labels  PACMAN.sl\PACMAN0s35.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(36)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s36.o -w-orphan-labels  PACMAN.sl\PACMAN0s36.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(37)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s37.o -w-orphan-labels  PACMAN.sl\PACMAN0s37.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(38)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s38.o -w-orphan-labels  PACMAN.sl\PACMAN0s38.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(39)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s39.o -w-orphan-labels  PACMAN.sl\PACMAN0s39.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(40)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s40.o -w-orphan-labels  PACMAN.sl\PACMAN0s40.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(41)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s41.o -w-orphan-labels  PACMAN.sl\PACMAN0s41.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(42)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s42.o -w-orphan-labels  PACMAN.sl\PACMAN0s42.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(43)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s43.o -w-orphan-labels  PACMAN.sl\PACMAN0s43.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(44)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s44.o -w-orphan-labels  PACMAN.sl\PACMAN0s44.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(45)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s45.o -w-orphan-labels  PACMAN.sl\PACMAN0s45.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(46)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s46.o -w-orphan-labels  PACMAN.sl\PACMAN0s46.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(47)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s47.o -w-orphan-labels  PACMAN.sl\PACMAN0s47.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(48)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s48.o -w-orphan-labels  PACMAN.sl\PACMAN0s48.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(49)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s49.o -w-orphan-labels  PACMAN.sl\PACMAN0s49.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(50)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s50.o -w-orphan-labels  PACMAN.sl\PACMAN0s50.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(51)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s51.o -w-orphan-labels  PACMAN.sl\PACMAN0s51.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(52)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s52.o -w-orphan-labels  PACMAN.sl\PACMAN0s52.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(53)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s53.o -w-orphan-labels  PACMAN.sl\PACMAN0s53.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(54)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s54.o -w-orphan-labels  PACMAN.sl\PACMAN0s54.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(55)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s55.o -w-orphan-labels  PACMAN.sl\PACMAN0s55.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(56)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s56.o -w-orphan-labels  PACMAN.sl\PACMAN0s56.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(57)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s57.o -w-orphan-labels  PACMAN.sl\PACMAN0s57.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(58)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s58.o -w-orphan-labels  PACMAN.sl\PACMAN0s58.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(59)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s59.o -w-orphan-labels  PACMAN.sl\PACMAN0s59.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(60)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s60.o -w-orphan-labels  PACMAN.sl\PACMAN0s60.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(61)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s61.o -w-orphan-labels  PACMAN.sl\PACMAN0s61.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(62)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s62.o -w-orphan-labels  PACMAN.sl\PACMAN0s62.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(63)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s63.o -w-orphan-labels  PACMAN.sl\PACMAN0s63.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(64)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s64.o -w-orphan-labels  PACMAN.sl\PACMAN0s64.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(65)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s65.o -w-orphan-labels  PACMAN.sl\PACMAN0s65.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(66)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s66.o -w-orphan-labels  PACMAN.sl\PACMAN0s66.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(67)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s67.o -w-orphan-labels  PACMAN.sl\PACMAN0s67.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(68)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s68.o -w-orphan-labels  PACMAN.sl\PACMAN0s68.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(69)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s69.o -w-orphan-labels  PACMAN.sl\PACMAN0s69.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(70)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s70.o -w-orphan-labels  PACMAN.sl\PACMAN0s70.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(71)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s71.o -w-orphan-labels  PACMAN.sl\PACMAN0s71.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(72)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s72.o -w-orphan-labels  PACMAN.sl\PACMAN0s72.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(73)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s73.o -w-orphan-labels  PACMAN.sl\PACMAN0s73.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(74)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s74.o -w-orphan-labels  PACMAN.sl\PACMAN0s74.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(75)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s75.o -w-orphan-labels  PACMAN.sl\PACMAN0s75.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(76)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s76.o -w-orphan-labels  PACMAN.sl\PACMAN0s76.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(77)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s77.o -w-orphan-labels  PACMAN.sl\PACMAN0s77.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(78)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s78.o -w-orphan-labels  PACMAN.sl\PACMAN0s78.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(79)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s79.o -w-orphan-labels  PACMAN.sl\PACMAN0s79.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(80)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s80.o -w-orphan-labels  PACMAN.sl\PACMAN0s80.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(81)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s81.o -w-orphan-labels  PACMAN.sl\PACMAN0s81.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(82)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s82.o -w-orphan-labels  PACMAN.sl\PACMAN0s82.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(83)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s83.o -w-orphan-labels  PACMAN.sl\PACMAN0s83.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(84)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s84.o -w-orphan-labels  PACMAN.sl\PACMAN0s84.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(85)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s85.o -w-orphan-labels  PACMAN.sl\PACMAN0s85.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(86)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s86.o -w-orphan-labels  PACMAN.sl\PACMAN0s86.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(87)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s87.o -w-orphan-labels  PACMAN.sl\PACMAN0s87.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(88)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s88.o -w-orphan-labels  PACMAN.sl\PACMAN0s88.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(89)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s89.o -w-orphan-labels  PACMAN.sl\PACMAN0s89.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(90)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s90.o -w-orphan-labels  PACMAN.sl\PACMAN0s90.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(91)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s91.o -w-orphan-labels  PACMAN.sl\PACMAN0s91.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(92)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s92.o -w-orphan-labels  PACMAN.sl\PACMAN0s92.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(93)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s93.o -w-orphan-labels  PACMAN.sl\PACMAN0s93.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(94)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s94.o -w-orphan-labels  PACMAN.sl\PACMAN0s94.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(95)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s95.o -w-orphan-labels  PACMAN.sl\PACMAN0s95.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(96)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s96.o -w-orphan-labels  PACMAN.sl\PACMAN0s96.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(97)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s97.o -w-orphan-labels  PACMAN.sl\PACMAN0s97.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(98)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s98.o -w-orphan-labels  PACMAN.sl\PACMAN0s98.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(99)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s99.o -w-orphan-labels  PACMAN.sl\PACMAN0s99.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(100)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s100.o -w-orphan-labels  PACMAN.sl\PACMAN0s100.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(101)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s101.o -w-orphan-labels  PACMAN.sl\PACMAN0s101.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(102)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s102.o -w-orphan-labels  PACMAN.sl\PACMAN0s102.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(103)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s103.o -w-orphan-labels  PACMAN.sl\PACMAN0s103.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(104)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s104.o -w-orphan-labels  PACMAN.sl\PACMAN0s104.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(105)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s105.o -w-orphan-labels  PACMAN.sl\PACMAN0s105.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(106)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s106.o -w-orphan-labels  PACMAN.sl\PACMAN0s106.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(107)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s107.o -w-orphan-labels  PACMAN.sl\PACMAN0s107.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(108)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s108.o -w-orphan-labels  PACMAN.sl\PACMAN0s108.s
if errorlevel 1 goto asmend
SET THEFILE=pacman(109)
echo Assembling %THEFILE%
c:\FPC\3.0.0\bin\i386-win32\nasm.exe -f obj -o PACMAN.sl\PACMAN0s109.o -w-orphan-labels  PACMAN.sl\PACMAN0s109.s
if errorlevel 1 goto asmend
c:\FPC\3.0.0\bin\i386-win32\wlib.exe -q -fo -c -b PACMAN.a  PACMAN.sl\PACMAN0s109.o PACMAN.sl\PACMAN0s108.o PACMAN.sl\PACMAN0s107.o PACMAN.sl\PACMAN0s106.o PACMAN.sl\PACMAN0s105.o PACMAN.sl\PACMAN0s104.o PACMAN.sl\PACMAN0s103.o PACMAN.sl\PACMAN0s102.o PACMAN.sl\PACMAN0s101.o PACMAN.sl\PACMAN0s100.o PACMAN.sl\PACMAN0s99.o PACMAN.sl\PACMAN0s98.o PACMAN.sl\PACMAN0s97.o PACMAN.sl\PACMAN0s96.o PACMAN.sl\PACMAN0s95.o PACMAN.sl\PACMAN0s94.o PACMAN.sl\PACMAN0s93.o PACMAN.sl\PACMAN0s92.o PACMAN.sl\PACMAN0s91.o PACMAN.sl\PACMAN0s90.o PACMAN.sl\PACMAN0s89.o PACMAN.sl\PACMAN0s88.o PACMAN.sl\PACMAN0s87.o PACMAN.sl\PACMAN0s86.o PACMAN.sl\PACMAN0s85.o PACMAN.sl\PACMAN0s84.o PACMAN.sl\PACMAN0s83.o PACMAN.sl\PACMAN0s82.o PACMAN.sl\PACMAN0s81.o PACMAN.sl\PACMAN0s80.o PACMAN.sl\PACMAN0s79.o PACMAN.sl\PACMAN0s78.o PACMAN.sl\PACMAN0s77.o PACMAN.sl\PACMAN0s76.o PACMAN.sl\PACMAN0s75.o PACMAN.sl\PACMAN0s74.o PACMAN.sl\PACMAN0s73.o PACMAN.sl\PACMAN0s72.o PACMAN.sl\PACMAN0s71.o PACMAN.sl\PACMAN0s70.o PACMAN.sl\PACMAN0s69.o PACMAN.sl\PACMAN0s68.o PACMAN.sl\PACMAN0s67.o PACMAN.sl\PACMAN0s66.o PACMAN.sl\PACMAN0s65.o PACMAN.sl\PACMAN0s64.o PACMAN.sl\PACMAN0s63.o PACMAN.sl\PACMAN0s62.o PACMAN.sl\PACMAN0s61.o PACMAN.sl\PACMAN0s60.o PACMAN.sl\PACMAN0s59.o PACMAN.sl\PACMAN0s58.o PACMAN.sl\PACMAN0s57.o PACMAN.sl\PACMAN0s56.o PACMAN.sl\PACMAN0s55.o PACMAN.sl\PACMAN0s54.o PACMAN.sl\PACMAN0s53.o PACMAN.sl\PACMAN0s52.o PACMAN.sl\PACMAN0s51.o PACMAN.sl\PACMAN0s50.o PACMAN.sl\PACMAN0s49.o PACMAN.sl\PACMAN0s48.o PACMAN.sl\PACMAN0s47.o PACMAN.sl\PACMAN0s46.o PACMAN.sl\PACMAN0s45.o PACMAN.sl\PACMAN0s44.o PACMAN.sl\PACMAN0s43.o PACMAN.sl\PACMAN0s42.o PACMAN.sl\PACMAN0s41.o PACMAN.sl\PACMAN0s40.o PACMAN.sl\PACMAN0s39.o PACMAN.sl\PACMAN0s38.o PACMAN.sl\PACMAN0s37.o PACMAN.sl\PACMAN0s36.o PACMAN.sl\PACMAN0s35.o PACMAN.sl\PACMAN0s34.o PACMAN.sl\PACMAN0s33.o PACMAN.sl\PACMAN0s32.o PACMAN.sl\PACMAN0s31.o PACMAN.sl\PACMAN0s30.o PACMAN.sl\PACMAN0s29.o PACMAN.sl\PACMAN0s28.o PACMAN.sl\PACMAN0s27.o PACMAN.sl\PACMAN0s26.o PACMAN.sl\PACMAN0s25.o PACMAN.sl\PACMAN0s24.o PACMAN.sl\PACMAN0s23.o PACMAN.sl\PACMAN0s22.o
if errorlevel 1 goto linkend
c:\FPC\3.0.0\bin\i386-win32\wlib.exe -q -fo -c -b PACMAN.a  PACMAN.sl\PACMAN0s21.o PACMAN.sl\PACMAN0s20.o PACMAN.sl\PACMAN0s19.o PACMAN.sl\PACMAN0s18.o PACMAN.sl\PACMAN0s17.o PACMAN.sl\PACMAN0s16.o PACMAN.sl\PACMAN0s15.o PACMAN.sl\PACMAN0s14.o PACMAN.sl\PACMAN0s13.o PACMAN.sl\PACMAN0s12.o PACMAN.sl\PACMAN0s11.o PACMAN.sl\PACMAN0s10.o PACMAN.sl\PACMAN0s9.o PACMAN.sl\PACMAN0s8.o PACMAN.sl\PACMAN0s7.o PACMAN.sl\PACMAN0s6.o PACMAN.sl\PACMAN0s5.o PACMAN.sl\PACMAN0s4.o PACMAN.sl\PACMAN0s3.o PACMAN.sl\PACMAN0s2.o PACMAN.sl\PACMAN0s1.o
if errorlevel 1 goto linkend
SET THEFILE=PACMAN.exe
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
