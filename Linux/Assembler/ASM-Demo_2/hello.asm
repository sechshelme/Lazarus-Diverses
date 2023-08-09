; nasm -felf64 hello.asm && ld hello.o && ./a.out
; https://cs.lmu.edu/~ray/notes/nasmtutorial/


;nasm main.asm -felf
;gcc -c foo.c -o foo.o -m32
;ld -o main main.o foo.o -melf_i386 -lc -I/lib32/ld-linux.so.2
;./main

; 64Bit
;nasm -felf64 hello.asm
;ld hello.o -o hello -lc -I /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2
;./hello


; https://stackoverflow.com/questions/27995554/how-to-call-c-functions-that-call-c-standard-library-in-nasm
; https://stackoverflow.com/questions/24991944/linking-c-with-nasm

          extern    puts
          extern    exit

          global    _start

          section   .data
message:  db        27,"[93m",  "Hello, World", 10, 10,0      ; note the newline at the end

          section   .text
_start:                                       ; This is called by the C library startup code
          mov       rdi, message            ; First integer (or pointer) argument in rdi
          call      puts                    ; puts(message)

          push      0
          call      exit

