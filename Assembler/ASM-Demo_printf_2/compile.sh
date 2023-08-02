echo "Assembling with Nasm"
nasm -felf64 hello.asm

echo "Linking ... "
# ld hello.o -o hello
ld hello.o -o hello -lc -I /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2


echo "Done !"
./hello

