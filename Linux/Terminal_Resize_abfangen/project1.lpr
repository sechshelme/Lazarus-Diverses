program project1;

uses
    BaseUnix,
  Unix;

// https://forum.ubuntuusers.de/topic/zeilenanzahl-im-terminal-ermitteln/

const
  STDIN_FILENO = 0;//  /* Standard input.  */
  STDOUT_FILENO = 1;//  /* Standard output.  */
  STDERR_FILENO = 2;//  /* Standard error output.  */

  TIOCGWINSZ = $5413;
  TIOCSWINSZ=	$5414;

   SIGWINCH=	28; //	/* Window size change (4.3 BSD, Sun).  */


type
  Twinsize = record
    ws_row, ws_col, ws_xpixel, ws_ypixel: cshort;
  end;
var
  w: Twinsize;
  s:String='Hello World !'#10;

procedure resize(signal: longint); cdecl;
begin
  FpIOCtl(STDOUT_FILENO, TIOCGWINSZ, @w);
  WriteLn(w.ws_col, 'x', w.ws_row);
  WriteLn(signal);
end;

begin
  FpIOCtl(STDOUT_FILENO, TIOCGWINSZ, @w);
  WriteLn(w.ws_col, 'x', w.ws_row);
  WriteLn(w.ws_xpixel, 'x', w.ws_ypixel);

  FpIOCtl(STDOUT_FILENO, TIOCSWINSZ, @w);
  WriteLn(w.ws_col, 'x', w.ws_row);
  WriteLn(w.ws_xpixel, 'x', w.ws_ypixel);

  FpWrite(STDOUT_FILENO, PChar(s), Length(s));
  FpWrite(STDOUT_FILENO, PChar(s), Length(s));

  FpSignal(SIGWINCH, @resize);

  repeat until False;

end.
