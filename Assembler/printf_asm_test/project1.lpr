program project1;

{$MODE objFPC}
{$ASMMODE intel}
{$DEFINE RELEASE}

{$POINTERMATH On}

uses
  ctypes;

  function printf(str: PChar): cint; varargs cdecl; external 'c';

const
  hello: PChar = 'Hello World ! %d';
var
  p2: Pointer;

  procedure print3; // assembler;
  var
    p: PtrInt;
  begin
    p := PtrInt(hello);
    asm
             Jmp     @start
             @hello:
             Db      'Hello World !'
             @Nr:
             Dq      123
             @start:
             Mov     Rdi, p
             //             Movq     Rsi,@hello
             Xor     Rax,Rax
             Call    printf

    end;
  end;

  procedure print1;
  const
    hello: PChar = 'Hello World ! '#10;
  var
    p: Pointer;
  begin
    p := hello;
    asm
             Mov     Rax, 1
             Mov     Rdi,1
             Mov     Rsi, p
             Mov     Rdx,15
             Syscall
    end;
  end;


  procedure print2;
  var
    hello2: PChar = 'Hello World ! '#10;
  begin
    asm
             Mov     Rax, 1
             Mov     Rdi,1
             Mov     Rsi, hello2;
             Mov     Rdx,15
             Syscall
    end;
  end;

begin
  //p2 := Pointer(hello);
  //  print;
  print1;
  print2;
  //  printf('Hello World\n', 111, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  //  printf('Hello World\n',111,2,3);
end.
