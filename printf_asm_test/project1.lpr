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
  var p2:Pointer;

  procedure print; // assembler;
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
             Movq     Rsi,p2
             Xor     Rax,Rax
             Call    printf

    end;
  end;

begin
  p2:=Pointer(hello);
  print;
  //  printf('Hello World\n', 111, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  //  printf('Hello World\n',111,2,3);
end.
