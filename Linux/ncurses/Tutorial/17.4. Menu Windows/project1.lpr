program project1;

uses
  ctypes,
  ncurses,
  menu;


var
  choices: array of PChar = ('Choic 1', 'Choic 2', 'Choic 3', 'Choic 4', 'Exit');
  n_choices: SizeInt;
  my_items: array of pITEM;
  c: cint = 0;
  i: integer;
  my_menu: pMENU;
  my_menu_win: PWINDOW;

  procedure print_in_middle(win: PWINDOW; starty, startx, Width: cint; s: PChar; color: chtype);
  var
    y, x: longint;
    len, temp: SizeInt;
  begin
    if win = nil then begin
      win := stdscr;
    end;
    getyx(win, y, x);
    if startx <> 0 then begin
      x := startx;
    end;
    if starty <> 0 then begin
      y := starty;
    end;
    if Width = 0 then begin
      Width := 80;
    end;
    len := StrLen(s);
    temp := (Width - len) div 2;
    x := startx + temp;
    wattron(win, color);
    mvwprintw(win, y, x, '%s', s);
    wattroff(win, color);
    refresh;
  end;

begin
  initscr;
  start_color;
  cbreak;
  noecho;
  keypad(stdscr, True);
  init_pair(1, COLOR_RED, COLOR_BLACK);

  n_choices := Length(choices);
  SetLength(my_items, n_choices + 1);

  for i := 0 to n_choices - 1 do begin
    my_items[i] := new_item(choices[i], choices[i]);
  end;

  my_items[n_choices] := nil;
  my_menu := new_menu(ppITEM(my_items));

  my_menu_win := newwin(10, 40, 4, 4);
  keypad(my_menu_win, True);

  set_menu_win(my_menu, my_menu_win);
  set_menu_sub(my_menu, derwin(my_menu_win, 6, 38, 3, 1));

  set_menu_mark(my_menu, ' * ');

  box(my_menu_win, 0, 0);

  print_in_middle(my_menu_win, 1, 0, 40, 'My Menu', COLOR_PAIR(1));

mvwaddch(my_menu_win, 2, 0, ACS_LTEE);
  mvwhline(my_menu_win, 2, 1, ACS_HLINE, 38);
  mvwaddch(my_menu_win, 2, 39, ACS_RTEE);
  mvprintw(Lines - 2, 0, 'F10 to Exit');
  refresh;

  post_menu(my_menu);
  wrefresh(my_menu_win);

  while c <> KEY_F(10) do begin
    c := wgetch(my_menu_win);
    case c of
      KEY_DOWN: begin
        menu_driver(my_menu, REQ_DOWN_ITEM);
      end;
      KEY_UP: begin
        menu_driver(my_menu, REQ_UP_ITEM);
      end;
    end;
  end;

  free_menu(my_menu);
  for i := 0 to n_choices - 1 do begin
    free_item(my_items[i]);
  end;
  endwin;
end.
