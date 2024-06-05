program project1;

uses
  ctypes;

const
  {$IFDEF Linux}
  libc = 'c';
  {$ENDIF}

  {$IFDEF Windows}
  libc = 'msvcrt.dll';
  {$ENDIF}


//
//  // https://cplusplus.com/reference/cstdlib/mbstowcs/
//  //function mbstowcs(dest:PDWord; src:PChar; max :SizeInt): SizeInt; cdecl; external libc;
//  function mbstowcs(dest: Pointer; src: PChar; max: SizeInt): SizeInt; cdecl; external libc;
//  function mblen(pmb: PDWord; max: SizeInt): cint; cdecl; external libc;
//
//  function setlocale(catogory: cint; locale: PChar): PChar; cdecl; external libc;
//
//  // https://cplusplus.com/reference/cuchar/mbrtoc32/
//  // size_t mbrtoc32 ( char32_t * pc32, const char * pmb, size_t max, mbstate_t * ps);
//  function mbrtoc32(dest: PDWord; src: PChar; max: SizeInt; state: Pointer): SizeInt; cdecl; external libc;
//


  // https://www.ibm.com/docs/en/zos/2.4.0?topic=functions-c16rtomb-convert-char16-t-character-multibyte-character
  function mbrtoc16(pc: PWord; s: PChar; max: SizeInt): cint; cdecl; external libc;



  // https://cplusplus.com/reference/cstdlib/mblen/
  // https://cplusplus.com/reference/cstdlib/mbtowc/

  function mbstowcs(dest: PDWord; src: PChar; max: SizeInt): SizeInt; cdecl; external libc;
  function mblen(pmb: PChar; max: SizeInt): cint; cdecl; external libc;
  function mbtowc(pwc: PDWord; pmb: PChar; max: SizeInt): cint; cdecl; external libc;
  function printf(c: PChar): cint; varargs cdecl; external libc;

  procedure printbuffer_mblen(pt: PChar; max: SizeInt);
  var
    len: cint;
    dest: DWord;
  begin
    mblen(nil, 0);
    mbtowc(nil, nil, 0);
    while max > 0 do begin
      len := mblen(pt, max);
      if len < 1 then begin
        Break;
      end;
      mbtowc(@dest, pt, len);
      printf('[%1c]', dest);
      pt += len;
      max -= len;
    end;
    printf(#10);
  end;

  procedure printbuffer_mbtowc(pt: PChar; max: SizeInt);
  var
    len: cint;
    dest: DWord;
  begin
    mbtowc(nil, nil, 0);
    while max > 0 do begin
      len := mbtowc(@dest, pt, max);
      if len < 1 then begin
        Break;
      end;
      printf('[%1c]', dest);
      pt += len;
      max -= len;
    end;
    printf(#10);
  end;

  procedure main;
  const
    s_mblen: PChar = 'test string';
    s_mbtowc: PChar = 'mbtowc example';
  begin
    printbuffer_mblen(s_mblen, Length(s_mblen));
    printbuffer_mbtowc(s_mbtowc, Length(s_mbtowc));
  end;

begin
  main;
end.
