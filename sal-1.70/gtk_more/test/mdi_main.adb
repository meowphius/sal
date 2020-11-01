with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Gtk.Main;
with MDI_Main_Window;
procedure MDI_Main
is
   Main_Window : MDI_Main_Window.Gtk_Window;
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   MDI_Main_Window.Gtk_New (Main_Window);

   MDI_Main_Window.Show (Main_Window);
   Gtk.Main.Main;
exception
when E : others =>
   Ada.Text_IO.Put_Line
     ("Unhandled exception " & Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end MDI_Main;
