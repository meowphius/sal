--  Abstract :
--
--  See spec
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
--

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Gtk.Main;
with Gtk.Message_Box;
package body Test_Gtk_More.Message_Box is

   procedure Test_Ok_Cancel_Box (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Gtk.Message_Box;

      Result : Result_Type;

   begin
      Gtk.Main.Set_Locale;
      Gtk.Main.Init;

      Result := Ok_Cancel_Box ("Test_Ok_Cancel", "Please hit Ok");
      AUnit.Assertions.Assert (Result = Ok, "did not get Ok");

      Result := Ok_Cancel_Box ("Test_Ok_Cancel", "Please hit Cancel");
      AUnit.Assertions.Assert (Result = Cancel, "did not get Cancel");
   end Test_Ok_Cancel_Box;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("message_box");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Ok_Cancel_Box'Access, "Test_Ok_Cancel_Box");
   end Register_Tests;

end Test_Gtk_More.Message_Box;
