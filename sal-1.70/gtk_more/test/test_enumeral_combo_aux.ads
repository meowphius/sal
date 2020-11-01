--  Abstract :
--
--  Library-level declarations for testing Gtk.Gen_Enumeral_Combo.
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

with Gtk.Gen_Enumeral_Combo;
with Gtk.Object;
package Test_Enumeral_Combo_Aux is

   type Enum is (One, Two, Three);
   package Enum_Combos is new Gtk.Gen_Enumeral_Combo (Enum);

   procedure On_Main_Window_Destroy (Object : access Gtk.Object.Gtk_Object_Record'Class);
   --  Exit gtk main loop

   Debug    : Boolean := False;
   Selected : Enum;

   procedure On_Enum_Combo_Selection_Changed (Combo : access Enum_Combos.Gtk_Enumeral_Record'Class);
   --  Set Selected to new selection. If Debug, also put a message to Text_IO.current_output

end Test_Enumeral_Combo_Aux;
