program Project1;

uses
  Strings,
  BaseUnix,
  Unix,
  confname;

  function sysconf(__name: cint): cint; cdecl; external 'c';


  procedure errx(const s: string);
  begin
    WriteLn(s);
    Halt(1);
  end;


const
  path = 'Project1.pas';
//  path = '/dev/stdin';


  function main: cint;
  var
    ar4: cint = 1;
    fd: cint;
    sb: stat;
    offset: culong = 0;
    len: SizeInt = 10;
    pa_offset: culong;
    addr: Pointer;
    s: TsSize;
  begin
    fd := FpOpen(path, O_RDONLY);
    if fd = -1 then begin
      errx('open');
    end;

    if FpFStat(fd, sb) = -1 then begin
      errx('fstat');
    end;

    pa_offset := offset and not (sysconf(_SC_PAGESIZE) - 1);

    WriteLn('size: ',sb.st_size);

    if offset >= sb.st_size then begin
      errx('Versatz ist hinter dem Dateiende');
    end;

    if ar4 = 4 then begin
      len := ar4;
      if offset + len > sb.st_size then begin
        len := sb.st_size - offset;
      end;
    end else begin
      len := sb.st_size - offset;
    end;
    len := 10;

    addr := Fpmmap(nil, len + offset - pa_offset, PROT_READ, MAP_PRIVATE, fd, pa_offset);
    if addr = MAP_FAILED then begin
      errx('mmap');
    end;

    s := FpWrite(StdOutputHandle, addr + offset - pa_offset, len);
    if s <> len then begin
      if s = -1 then begin
        errx('write');
      end;
      WriteLn('Schreiben unvollständig');
      Exit(1);
    end;

    Fpmunmap(addr, len + offset - pa_offset);

    FpClose(fd);
    Result := 0;
  end;

begin
  main;
end.
