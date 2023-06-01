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

begin
  initscr;
  cbreak;
  noecho;
  keypad(stdscr, True);

  n_choices := Length(choices);
  SetLength(my_items, n_choices + 1);

  for i := 0 to n_choices - 1 do begin
    my_items[i] := new_item(choices[i], choices[i]);
  end;
  my_items[n_choices] := nil;
  my_menu := new_menu(ppITEM(my_items));
  mvprintw(Lines - 2, 0, 'F10 to Exit');

  post_menu(my_menu);
  refresh;

  while c <> KEY_F(10) do begin
    c := getch;
    case c of
    KEY_DOWN:menu_driver(my_menu,REQ_DOWN_ITEM);
    KEY_UP:menu_driver(my_menu,REQ_UP_ITEM);
    end;
  end;


  free_item(my_items[0]);
  free_item(my_items[1]);
  free_menu(my_menu);
  endwin;
end.
