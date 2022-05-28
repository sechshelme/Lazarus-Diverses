unit Unit1;

{$mode objfpc}{$H+}

(*
-al
-CfAVX2
-CpCOREAVX2
-O3
-Sv
-OpCOREAVX2
-OoFASTMATH
*)


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type


  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

type
  TVector4f = array[0..3] of single;
  TVector8f = array[0..7] of single;

var
  a: TVector4f = (1.0, 2.0, 3.0, 4.0);
  b: TVector4f = (5.0, 6.0, 7.0, 8.0);
  c: TVector4f;
  f: single = 2.0;

  aa: TVector8f = (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0);
  bb: TVector8f = (11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0);
  cc: TVector8f;


{$asmmode intel}

procedure WriteVector(v: TVector4f);
begin
  WriteLn(v[0]: 8: 4, '  ', v[1]: 8: 4, '  ', v[2]: 8: 4, '  ', v[3]: 8: 4, '  ');
end;

procedure WriteVector(v: TVector8f);
begin
  WriteLn(v[0]: 8: 4, '  ', v[1]: 8: 4, '  ', v[2]: 8: 4, '  ', v[3]: 8: 4, '  ', v[4]: 8: 4, '  ', v[5]: 8: 4, '  ', v[6]: 8: 4, '  ', v[7]: 8: 4);
end;

(*
Result[0] := v0[0] + v1[0];
Result[1] := v0[1] + v1[1];
Result[2] := v0[2] + v1[2];
Result[3] := v0[3] + v1[3];
*)


{$if defined(cpux86_64) or defined(cpux86)}
function Vec_Add(const v0, v1: TVector4f): TVector4f; assembler; nostackframe; register;
asm
         Movups  Xmm0, [v0]
         Movups  Xmm1, [v1]
         Addps   Xmm1, Xmm0
         Movups  [Result], Xmm1
end;
{$else}
function Vec_Add(const v0, v1: TVector4f): TVector4f; inline;
begin
  Result[0] := v0[0] + v1[0];
  Result[1] := v0[1] + v1[1];
  Result[2] := v0[2] + v1[2];
  Result[3] := v0[3] + v1[3];
end;
{$endif}

(*
Result[0] := v0[0] - v1[0];
Result[1] := v0[1] - v1[1];
Result[2] := v0[2] - v1[2];
Result[3] := v0[3] - v1[3];
*)
function Vec_Sub(const v0, v1: TVector4f): TVector4f; assembler; nostackframe; register;
asm
         Movups  Xmm0, [v1]
         Movups  Xmm1, [v0]
         Subps   Xmm1, Xmm0
         Movups  [Result], Xmm1
end;

(*
Result[0] := v0[0] * v1[0];
Result[1] := v0[1] * v1[1];
Result[2] := v0[2] * v1[2];
Result[3] := v0[3] * v1[3];
*)
function Vec_Multiply(const v0, v1: TVector4f): TVector4f; assembler; nostackframe; register;
asm
         Movups  Xmm0, [v1]
         Movups  Xmm1, [v0]
         Mulps   Xmm1, Xmm0
         Movups  [Result], Xmm1
end;

(*
Result[0] := v0[0] / v1[0];
Result[1] := v0[1] / v1[1];
Result[2] := v0[2] / v1[2];
Result[3] := v0[3] / v1[3];
*)
function Vec_Divide(const v0, v1: TVector4f): TVector4f; assembler; nostackframe; register;
asm
         Movups  Xmm0, [v1]
         Movups  Xmm1, [v0]
         Divps   Xmm1, Xmm0
         Movups  [Result], Xmm1
end;

(*
Result[0] := v[3];
Result[1] := v[2];
Result[2] := v[1];
Result[3] := v[0];
*)
function Vec_Swap(const v: TVector4f): TVector4f; assembler; nostackframe; register;
asm
         Movups  Xmm0, [v]
         Pshufd  Xmm1,Xmm0, $1b
         Movups  [Result], Xmm1
end;


(*
Result[0] := v[0] * f;
Result[1] := v[1] * f;
Result[2] := v[2] * f;
Result[3] := v[3] * f;
*)
function Vec_Multiply_All(const v: TVector4f; f: single): TVector4f; assembler; inline;
asm
         Movss   Xmm0, f
         Pshufd  Xmm0, Xmm0, $00
         Movups  Xmm1, [v]
         Mulps   Xmm1, Xmm0
         Movups  [Result], Xmm1
end;


function Vec_Add256(const v0, v1: TVector8f): TVector8f; assembler; nostackframe; register;
asm
         Vmovups  ymm0, [v0]
         Vmovups  ymm1, [v1]
         Vmulps  ymm3, ymm1, ymm0
         Vmovups  [Result], ymm3
end;

function TestVektor(const v: TVector8f): TVector8f; assembler; nostackframe; register;
asm
//         Vbroadcastss    ymm0, [v + $00]
         Vmovups  [Result], ymm0
end;


function All4(f: PSingle): TVector4f; assembler; nostackframe; register;
asm
         Movss   Xmm0, [f]
         Pshufd  Xmm0, Xmm0, $00
         Movups  [Result], Xmm0
end;

function All4(f: single): TVector4f; assembler; nostackframe; register;
asm
         Movss   Xmm0, f
         Pshufd  Xmm0, Xmm0, $00
         Movups  [Result], Xmm0
end;

function All8(f: PSingle): TVector8f; assembler; nostackframe; register;
asm
//         Vbroadcastss    ymm0,  [f]
         Vmovups  [Result], ymm0
end;

function All8(f: single): TVector8f; assembler; nostackframe; register;
asm
//         Vbroadcastss    ymm0,  f            // Hier SIGILL
         Vmovups  [Result], ymm0
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  WriteLn('Addieren');
  c := Vec_Add(a, b);
  WriteVector(c);
  WriteLn();

  WriteLn('Subtrahieren');
  c := Vec_Sub(b, a);
  WriteVector(c);
  WriteLn();

  WriteLn('Multipizieren');
  c := Vec_Multiply(b, a);
  WriteVector(c);
  WriteLn();

  WriteLn('Dividieren');
  c := Vec_Divide(b, a);
  WriteVector(c);
  WriteLn();

  WriteLn('Vertauschen');
  c := Vec_Swap(a);
  WriteVector(c);
  WriteLn();

  WriteLn('Alle Multipizieren');
  f := 2.2;
  c := Vec_Multiply_All(a, f);
  WriteVector(c);
  WriteLn();

  cc := Vec_Add256(aa, bb);
  WriteVector(cc);
  WriteLn();
  WriteLn();


  WriteLn('4 Vektor mit Pointer');
  f := 12.34;
  c := All4(@f);
  WriteVector(c);
  WriteLn();

  WriteLn('4 Vektor ohne Pointer');
  f := 23.45;
  c := All4(f);
  WriteVector(c);
  WriteLn();

  WriteLn('8 Vektor mit Pointer');
  f := 34.56;
  cc := All8(@f);
  WriteVector(cc);
  WriteLn();

  WriteLn('8 Vektor ohne Pointer');
  f := 45.67;
  cc := All8(f);
  WriteVector(cc);
  WriteLn();

end;

end.


















