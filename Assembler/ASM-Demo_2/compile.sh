nasm -felf64 hello.asm
# ld hello.o -o hello
# ld hello.o -o hello -lc -I /lib/x86_64-linux-gnu/ld-linux-x86-64.so
ld hello.o -o hello -lc -I /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2

# ld hello.o -lc -o hello 
./hello

