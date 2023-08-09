program project1;

{$MODE objFPC}
{$ASMMODE intel}
{$DEFINE RELEASE}

uses
  ctypes;

  function printf(str: PChar): cint; varargs cdecl; external 'c';

  procedure print1;
  var
    hello: PChar = 'Hello World 1 ! '#10;
  var
    p: Pointer;
  begin
    p := hello;
    asm
             Mov     Rax, 1
             Mov     Rdi,1
             Mov     Rsi, hello
             Mov     Rdx,17
             Syscall
    end;
  end;

var
  hello2: PChar = 'Hello World 2 ! '#10;

procedure print2;
begin
  asm
           Mov     Rax, [hello2 wrt ..gotpcrel]
           Mov     Rsi, qword ptr[Rax]

           Mov     Rax, 1
           Mov     Rdi,1
           Mov     Rdx,17
           Syscall
  end;
end;

procedure print3;
const
  hello3: PChar = 'Hello World 3 ! '#10;
begin
  asm
           Mov     Rax, [hello3 wrt ..gotpcrel]
           Mov     Rsi, qword ptr[Rax]

           Mov     Rax, 1
           Mov     Rdi,1
           Mov     Rdx,17
           Syscall
  end;
end;

procedure print4;   assembler;
asm
           Mov     Rax, [hello2 wrt ..gotpcrel]
           Mov     Rsi, qword ptr[Rax]

           Mov     Rax, 1
           Mov     Rdi,1
           Mov     Rdx,17
           Syscall
end;

procedure print5;   assembler;
asm
//           jmp     @start
//@hello5:   db      "Hello World 5 !"
//
////@start:     Mov     Rax, [@hello5 wrt ..gotpcrel]
//@start:     Mov     Rax, [@hello5 wrt ..gotpcrel]
//           Mov     Rsi, qword ptr[Rax]
//
//           Mov     Rax, 1
//           Mov     Rdi,1
//           Mov     Rdx,17
//           Syscall
end;

begin
  print1;
  print2;
  print3;
  print4;
  print5;
  //  printf('Hello World\n', 111, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  //  printf('Hello World\n',111,2,3);
end.
