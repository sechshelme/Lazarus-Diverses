program project1;

uses
ncurses;

(*
A_NORMAL        Normal display (no highlight)
A_STANDOUT      Best highlighting mode of the terminal
A_UNDERLINE     Underlining
A_REVERSE       Reverse video
A_BLINK         Blinking
A_DIM           Half bright
A_BOLD          Extra bright or bold
A_PROTECT       Protected mode
A_INVIS         Invisible or blank mode
A_ALTCHARSET    Alternate character set
A_CHARTEXT      Bit-mask to extract a character
COLOR_PAIR(n)   Color-pair number n
*)

begin
  initscr;

  attron(A_NORMAL);
  printw('Hello World !!!' + LineEnding);
  attroff(A_NORMAL);

  attron(A_STANDOUT);
  printw('Hello World !!!' + LineEnding);
  attroff(A_STANDOUT);

  attron(A_UNDERLINE);
  printw('Hello World !!!' + LineEnding);
  attroff(A_UNDERLINE);

  attron(A_REVERSE);
  printw('Hello World !!!' + LineEnding);
  attroff(A_REVERSE);

  attron(A_BLINK);
  printw('Hello World !!!' + LineEnding);
  attroff(A_BLINK);

  attron(A_DIM);
  printw('Hello World !!!' + LineEnding);
  attroff(A_DIM);

  attron(A_BOLD);
  printw('Hello World !!!' + LineEnding);
  attroff(A_BOLD);

  attron(A_PROTECT);
  printw('Hello World !!!' + LineEnding);
  attroff(A_PROTECT);

  attron(A_INVIS);
  printw('Hello World !!!' + LineEnding);
  attroff(A_INVIS);

  attron(A_ALTCHARSET);
  printw('Hello World !!!' + LineEnding);
  attroff(A_ALTCHARSET);

  attron(A_CHARTEXT);
  printw('Hello World !!!' + LineEnding);
  attroff(A_CHARTEXT);

  attron(COLOR_PAIR(23));
  printw('Hello World !!!' + LineEnding+LineEnding);
  attroff(COLOR_PAIR(23));

  attron(A_BOLD);
  attron(A_UNDERLINE);
  printw('Hello World !!!' + LineEnding);
  attroff(A_UNDERLINE);
  attroff(A_BOLD);

  attron(A_DIM);
  printw('Hello World !!!' + LineEnding);
  attroff(A_DIM);

  refresh;
  getch;
  endwin;
end.
