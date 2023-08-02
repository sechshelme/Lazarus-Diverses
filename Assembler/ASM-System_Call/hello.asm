; nasm -felf64 hello.asm && ld hello.o && ./a.out
; https://cs.lmu.edu/~ray/notes/nasmtutorial/
; https://codereview.stackexchange.com/questions/206381/simple-puts-function-in-x64-assembly

section  .data
kmessage: db        27,"[93m",  "Write with kernel interrupt", 10 
smessage: db        27,"[92m",  "Write with syscall", 10      
lmessage: db        27,"[91m",  "Write with Lenght", 10, 10, 0      

          global    _start

section   .text

; ----- kernel interrupt
_start:   mov       edx, 33                 ; Message Länge
          mov       ecx, kmessage           ; Message für Ausgabe
          mov       ebx, 1                  ; (stdout)
          mov       eax, 4                  ; System Call Nummer (sys_write)
          int 0x80                          ; Kernel Interrupt

; ----- syscall
          mov       rax, 1                  ; system call for write
          mov       rdi, 1                  ; file handle 1 is stdout
          mov       rsi, smessage           ; address of string to output
          mov       rdx, 25                 ; number of bytes
          syscall                           ; invoke operating syst

; ---- with Length
         mov        rsi, lmessage
         call       strlen
         mov        rdi, 1                  ; file handle 1 is stdout
         mov        rax, 1
         syscall

       
; ----- kernel interrupt Exit
          mov       eax, 1                  ; system call for exit
          int 0x80                          ; Kernel Interrupt



; --- strlen
strlen:
  xor rdx, rdx
  dec rdx          ; This compensates for the INC that is happening first.
.next:
  inc rdx
  cmp byte [rsi + rdx], 0
  jne .next
  ret
