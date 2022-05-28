program Project1;

uses
  ncurses;

var
  i: integer;


begin
  initscr;
  start_color;

  for i := 0 to 255 do begin
//    init_color(i, 255-i,i,i);
    init_pair(i, i, 159 - i);
    color_set(i, nil);
    mvaddstr(i div 60, i mod 60, 'X');
  end;

  attrset(A_UNDERLINE);
  color_set(1, nil);
  mvaddstr(5, 5, 'Hello World');
  attrset(A_BOLD);
  mvaddstr(6, 5, 'Hello World');
  attrset(A_BOLD + A_UNDERLINE);
  mvaddstr(7, 5, 'Hello World');
  attrset(A_BLINK);
  mvaddstr(8, 5, 'Blink');
  attrset(A_DIM);
  mvaddstr(9, 5, 'Dim');
  refresh;
  getch;
end.
