program ConstMatrixExampleObjFPC;

{$mode ObjFPC}
{$modeswitch AdvancedRecords}

type
   String3 = String;

   generic TRawMatrix<T; const N: SizeUInt> = array[0..N-1] of array[0..N-1] of T;

   generic TMatrix<T; const N: SizeUInt> = record
   private type
     ArrayType = specialize TRawMatrix<T, N>;
   private
     Data: ArrayType;
   public
     class operator :=(constref Arr: ArrayType): TMatrix; inline;
     procedure Display;
   end;

   class operator TMatrix.:=(constref Arr: ArrayType): TMatrix;
   begin
     Result.Data := Arr;
   end;

   procedure TMatrix.Display;
   var I, J: SizeInt;
   begin
     WriteLn('[');
     for I := 0 to N - 1 do begin
       Write(' [');
       for J := 0 to N - 2 do
         Write(Data[I, J], ', ');
       Write(Data[I, N - 1]);
       Writeln('] ');
     end;
     Write(']');
   end;

generic procedure Swap<T>(a, b: T);
var
   c: T;
begin
  c := a;
  a := b;
  b := c;
end;

const RawMat: specialize TRawMatrix<single, 4> = (
   (1, 0, 0, 0),
   (0, 1, 0, 0),
   (0, 0, 1, 0),
   (0, 0,0, 1)
);

var Mat: specialize TMatrix<single, 4>;


  type
    generic TSomeMicroGPIO<const Base: PtrUInt> = record
    private
      procedure SetPin(aIndex: SizeInt; aEnable: Boolean); inline;
      function GetPin(aIndex: SizeInt): Boolean; inline;
    public
      property Pin[Index: SizeInt]: Boolean read GetPin write SetPin;
    end;

  procedure TSomeMicroGPIO.SetPin(aIndex: SizeInt; aEnable: Boolean);
  begin
     if aEnable then
       PLongWord(Base)[2] := PLongWord(Base)[2] or (1 shl aIndex)
     else
       PLongWord(Base)[2] := PLongWord(Base)[2] and not (1 shl aIndex);
  end;

  function TSomeMicroGPIO.GetPin(aIndex: SizeInt): Boolean;
  begin
     Result := (PLongWord(Base)[2] and (1 shl aIndex)) <> 0
  end;

  var
     GPIOA: specialize TSomeMicroGPIO<$8000F000>;
     GPIOB: specialize TSomeMicroGPIO<$8000F100>;
     GPIOC: specialize TSomeMicroGPIO<$8000F200>;

var
   s0, s1: string;

begin
   Mat := RawMat;
   Mat.Display();

//   GPIOA.Pin[2] := True;

  s0 := 'null';
  s1 := 'eins';
  WriteLn(s0, '  ', s1);
  specialize Swap(<string>, s0, s1);
  WriteLn(s0, '  ', s1);

end.
