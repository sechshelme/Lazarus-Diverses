program Project1;

uses
  GTK2,SysUtils;

Var
 Btn,Window:pGTKWidget;

begin
  GTK_Init(@argc,@argv);
  Btn := gtk_button_new_with_label('Button');
  Window := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  GTK_Container_Add(GTK_Container(Window),Btn);
  GTK_Widget_Show(Window);
  GTK_Widget_Show(Btn);
  GTK_Main;
end.
