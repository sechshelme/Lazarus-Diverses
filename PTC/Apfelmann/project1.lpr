program project1;

uses
  ptcgraph;
//  ptcgraph;

const
  o: single = 1.2;
  u: single = -1.2;
  l: single = -2.2;
  r: single = 1.2;
  maxX = 1200;
  maxY = 800;
  gd: smallint = 0;
  gm: smallint = 0;

  procedure Normal;
  var
    Farbe, x, y: word;
    SqrX, SqrY, XPos, YPos, XStep, YStep, creal, cimag, links, rechts, oben, unten: single;

  begin
    links := l;
    rechts := r;
    oben := o;
    unten := u;
    XStep := (rechts - links) / maxX;
    YStep := (oben - unten) / maxY;
    creal := links;
    for x := 0 to maxX - 1 do begin
      cimag := unten;
      for y := 0 to maxY - 1 do begin
        farbe := 0;
        XPos := 0;
        YPos := 0;
        repeat
          SqrX := Sqr(XPos);
          SqrY := Sqr(YPos);
          YPos := 2 * XPos * YPos + cimag;
          XPos := SqrX - SqrY + creal;
          Inc(farbe);
        until (SqrX + SqrY > 8) or (farbe > 1000);
        if Farbe > 100 then begin
          farbe := 0;
        end;
        PutPixel(x, y, Farbe);
        cimag := cimag + YStep;
      end;
      creal := creal + XStep;
    end;
  end;

begin
  gm := 0;
  gd := 0;
  InitGraph(gd, gm, 'c:\pp\bgi');
  Normal;
  readln;
end.
