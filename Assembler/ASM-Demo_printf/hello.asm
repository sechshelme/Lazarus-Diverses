; https://redirect.cs.umbc.edu/portal/help/nasm/sample_64.shtml
; https://stackoverflow.com/questions/10168743/which-variable-size-to-use-db-dw-dd-with-x86-assembly

extern printf

global _start

section .text
_start:

; ---- Integer 1 ----

    mov rdi,message
    mov rsi, 1000
    mov rdx, -2000
    mov rcx, -3000
    mov r8, -4000
    mov r9, 5000
    mov dword[rsp], 6000
    mov dword[rsp+8], 7000
    mov dword[rsp+16], 8000
    mov dword[rsp+24], 9000

    xor rax,rax
    call printf


; ---- Integer 2 ----
    
    mov rdi,message

    mov rsi, [integer]
    mov rdx, [integer+8]
    mov rcx, [integer+16]
    mov r8, [integer+24]
    mov r9, [integer+32]

    mov rax, [integer+40]     
    mov [rsp], rax

    mov rax, [integer+48]     
    mov[rsp+8], rax

    mov rax, [integer+56]     
    mov[rsp+16], rax

    mov rax, [integer+64]     
    mov[rsp+24], rax

    xor rax,rax
    call printf

; ---------------------------------


    mov rdi,strmsg
    mov rsi, str0
    mov rdx, str1
    mov rcx, str2
    mov r8, str3
    mov r9, str4
    mov qword [rsp], str5
    mov qword [rsp+8], str6
    mov qword [rsp+16], str7
    mov qword [rsp+24], str8
    mov qword [rsp+32], str9

    xor rax,rax
    call printf
;    call printf WRT ..plt

; ---- With push -----

	sub	rsp, 8
	push	20
	push	19
	push	18
	push	17
	push	16
	push	15
	push	14
	push	13
	push	12
	push	11
	push	10
	push	9
	push	8
	push	7
	push	6    
    mov	r9d, 5
	mov	r8d, 4
	mov	ecx, 3
	mov	edx, 2
	mov	esi, 1

	mov	rdi, pushMessage
	mov	eax, 0
	call	printf

add	rsp, 80

; ---------------------------------

    mov rax,60	; exit
    syscall


section .data
;    integer:       db  11, 22, 33, 44, 55, 66, 77, 88, 99
    integer:       dq  1111, 2222, 3333, 4444, 5555, 6666, 7777, 8888, 9999
    message:       db  "Integer: %d %d %d %d %d %d %d %d %d", 10, 10, 0
    pushMessage:   db  "Integer: %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d", 10, 10, 0

    str0:          db  "Ich bin der 0 String", 0
    str1:          db  "Ich bin der 1 String", 0
    str2:          db  "Ich bin der 2 String", 0
    str3:          db  "Ich bin der 3 String", 0
    str4:          db  "Ich bin der 4 String", 0
    str5:          db  "Ich bin der 5 String", 0
    str6:          db  "Ich bin der 6 String", 0
    str7:          db  "Ich bin der 7 String", 0
    str8:          db  "Ich bin der 8 String", 0
    str9:          db  "Ich bin der 9 String", 0
    strmsg         db  27, "[93m", "String: ", 10, 27, "[94m%s ", 10, 
                   db  "%s", 10, "%s", 10, "%s", 10, "%s", 10, "%s", 10, "%s", 10, "%s", 10, "%s", 10, "%s",  10, 10, 0 

