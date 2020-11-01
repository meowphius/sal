--  Abstract :
--
--  Debug Gtk.Gen_Enumeral_Combo
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Gtk.Box;
with Gtk.Label;
with Gtk.Main;
with Gtk.Window.Signal;
with Test_Gen_Enumeral_Combo_Aux; use Test_Gen_Enumeral_Combo_Aux;
procedure Debug_Gen_Enumeral_Combo
is
   Enum_Combo  : Enum_Combos.Gtk_Enumeral;
   Vbox        : Gtk.Box.Gtk_Box;
   Hbox        : Gtk.Box.Gtk_Box;
   Label       : Gtk.Label.Gtk_Label;
   Main_Window : Gtk.Window.Gtk_Window;
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Gtk.Window.Gtk_New (Main_Window);
   Gtk.Window.Show (Main_Window);

   Gtk.Box.Gtk_New_Vbox (Vbox);
   Gtk.Box.Show (Vbox);
   Gtk.Window.Add (Main_Window, Vbox);

   Gtk.Label.Gtk_New (Label, "Just a label to give the window a definite size");
   Gtk.Label.Show (Label);
   Gtk.Box.Pack_Start (Vbox, Label);

   --  Enum combo
   Gtk.Box.Gtk_New_Hbox (Hbox);
   Gtk.Box.Show (Hbox);
   Gtk.Box.Pack_Start (Vbox, Hbox);

   --  Label in hbox with combo to allow combo to resize
   Gtk.Label.Gtk_New (Label, "enum");
   Gtk.Label.Show (Label);
   Gtk.Box.Pack_Start (Hbox, Label);

   Enum_Combos.Gtk_New (Enum_Combo);
   Enum_Combos.Show (Enum_Combo);
   Gtk.Box.Pack_End (Hbox, Enum_Combo, Expand => False);

   Enum_Combos.Connect_Selection_Changed (Enum_Combo, On_Enum_Combo_Selection_Changed'Access);

   Gtk.Window.Signal.Connect_Destroy (Main_Window, On_Main_Window_Destroy'Access);

   Put_Line ("Entering Main");
   Gtk.Main.Main;
   Put_Line ("Exited Main");

exception
when E : others =>
   Put_Line (Ada.Exceptions.Exception_Name (E) & " : " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Debug_Gen_Enumeral_Combo;
