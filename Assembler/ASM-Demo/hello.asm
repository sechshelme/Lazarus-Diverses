; nasm -felf64 hello.asm && ld hello.o && ./a.out
; https://cs.lmu.edu/~ray/notes/nasmtutorial/

          section   .data
message:  db        27,"[93m",  "Hello, World", 10, 10      ; note the newline at the end

          global    _start

          section   .text
_start:   mov       rax, 1                  ; system call for write
          mov       rdi, 1                  ; file handle 1 is stdout
          mov       rsi, message            ; address of string to output
          mov       rdx, 19                 ; number of bytes
          syscall                           ; invoke operating system to do the write
          mov       rax, 60                 ; system call for exit
          xor       rdi, rdi                ; exit code 0
          syscall                           ; invoke operating system to exit


