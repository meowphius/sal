--  Abstract :
--
--  See spec.
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

with Ada.Text_IO;
with Gtk.Main;
package body Test_Enumeral_Combo_Aux is

   procedure On_Main_Window_Destroy (Object : access Gtk.Object.Gtk_Object_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Main_Quit;
   end On_Main_Window_Destroy;

   procedure On_Enum_Combo_Selection_Changed (Combo : access Enum_Combos.Gtk_Enumeral_Record'Class)
   is begin
      if Debug then
         Ada.Text_IO.Put_Line ("selection => " & Enum'Image (Enum_Combos.Get (Combo)));
      end if;

      Selected := Enum_Combos.Get (Combo);

   end On_Enum_Combo_Selection_Changed;

end Test_Enumeral_Combo_Aux;
