program Project1;

uses
  Crt;

var
  stellen: integer;

  procedure CalcPi;
  var
    r, p: string;
    m, i, j, u, z: integer;

    c: array of integer;
  begin
    SetLength(c, 10 * (Stellen + 1) div 3 + 1);
    r := '';
    p := '';
    m := 10 * (Stellen + 1) div 3;
    for i := 1 to m do begin
      c[i] := 2;
    end;
    for j := 0 to Stellen do begin
      if j mod 100 = 0 then begin
        GotoXY(1, 1);
        WriteLn('Stelle: ', j);
      end;

      u := 0;
      for i := m downto 2 do begin
        z := 10 * c[i] + u;
        c[i] := z mod (i * 2 - 1);
        u := z div (i * 2 - 1) * (i - 1);
      end;
      z := 10 * c[1] + u;
      c[1] := z mod 10;
      u := z div 10;
      if u <= 8 then begin
        r := r + p;
        p := Chr(48 + u);
      end;
      if u = 9 then begin
        p := p + Chr(48 + u);
      end;
      if u = 10 then begin
        for i := 1 to Length(p) do begin
          Val(p[i], z, u);
          Inc(z);
          if z = 10 then begin
            z := 0;
          end;
          p[i] := Chr(48 + z);
        end;
        p := p + '0';
      end;
    end;
    p := r[1] + '.' + Copy(r, 2, Length(r)) + p;
    WriteLn(p);
    SetLength(c, 0);
  end;

begin
  ClrScr;
  Writeln('Bitte Stellen eingeben: ');
  readln(stellen);
  CalcPi;
end.
