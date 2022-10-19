program project1;

uses
  ncurses;

var
  ch: longint;
begin
  initscr;
  raw;
  keypad(stdscr, True);
  noecho;
  printw('Type any character to see it in bold\n');
  ch := getch;
  if ch = KEY_F(1) then begin
    printw('F1 Key pressed');
  end else begin
    printw('The pressed key is ');
    attron(A_BOLD);
    printw('%c', ch);
    attroff(A_BOLD);
  end;
  refresh;
  getch;
  endwin;
end.
