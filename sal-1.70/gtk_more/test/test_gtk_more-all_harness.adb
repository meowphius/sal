--  Abstract :
--
--  Run all tests of gtk_more stuff.
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

with AUnit.Test_Results.Text_Reporter;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Test_Gtk_More.Enumeral_Combo;
with Test_Gtk_More.Message_Box;
procedure Test_Gtk_More.All_Harness
is
   Suite  : constant Access_Test_Suite := new Test_Suite;
   Result : AUnit.Test_Results.Result;

begin
   --  WORKAROUND: gtkada 2.2.0 can't handle Enumeral_Combo first;
   --  some sort of tasking issue?
   Add_Test (Suite, new Test_Gtk_More.Message_Box.Test_Case (Debug_Level => 0));
   Add_Test (Suite, new Test_Gtk_More.Enumeral_Combo.Test_Case (Debug_Level => 0));

   Run (Suite.all, Result);
   AUnit.Test_Results.Text_Reporter.Report (Result);
end Test_Gtk_More.All_Harness;
