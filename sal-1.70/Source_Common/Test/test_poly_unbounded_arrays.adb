-- Abstract :
--
-- test SAL.Poly.Unbounded_Arrays
--
-- Copyright (C) 1999, 2002 Stephen Leake.  All Rights Reserved.
--
-- This program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2, or (at
-- your option) any later version. This program is distributed in the
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even
-- the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE. See the GNU General Public License for more details. You
-- should have received a copy of the GNU General Public License
-- distributed with this program; see file COPYING. If not, write to
-- the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
-- MA 02111-1307, USA.
--
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Poly_Unbounded_Arrays_Aux;
with Test_Storage_Pools;
procedure Test_Poly_Unbounded_Arrays is

   use Test_Poly_Unbounded_Arrays_Aux;
   use Test_Poly_Unbounded_Arrays_Aux.Integers.Arrays;
   use Test_Poly_Unbounded_Arrays_Aux.Integers.Arrays_Test;

   Integer_Array : Array_Type;
   Debug_Level : Integer := 0;
begin

   case Ada.Command_Line.Argument_Count is
   when 0 =>
      -- use defaults
      null;

   when 1 =>
      Debug_Level := Integer'Value (Ada.Command_Line.Argument (1));

   when others =>
      Put_Line ("arguments : <debug_level>");
      Put_Line ("defaults  : " & Integer'Image (Debug_Level));
      return;
   end case;

   Test_Storage_Pools.Set_Debug (Array_Storage_Pool, Debug_Level > 0);
   Put_Line ("Creating Append null array");
   Create
      (Array_Obj => Integer_Array,
       Space => 0,
       Growth => Append,
       First => 10);
   Put (Integer_Array);
   -- Array_Storage_Pool contains just the array bounds at this point.

   Put_Line ("Testing for Constraint_Errors");
   begin
      Set (Integer_Array, 1, 100);
      Put_Line ("oops, did NOT raise Constraint_Error");
   exception
   when Constraint_Error =>
      Put_Line ("ok, got Constraint_Error");
   end;

   begin
      Set (Integer_Array, 11, 100);
      Put_Line ("oops, did NOT raise Constraint_Error");
   exception
   when Constraint_Error =>
      Put_Line ("ok, got Constraint_Error");
   end;

   begin
      Delete_First (Integer_Array);
      Put_Line ("oops, did NOT raise Constraint_Error");
   exception
   when Constraint_Error =>
      Put_Line ("ok, got Constraint_Error");
   end;

   begin
      Delete_Last (Integer_Array);
      Put_Line ("oops, did NOT raise Constraint_Error");
   exception
   when Constraint_Error =>
      Put_Line ("ok, got Constraint_Error");
   end;

   Put_Line ("Adding First, Last");
   begin
      -- can't Add_First to 'append' array
      Add_First (Integer_Array, -101);
      Put_Line ("oops, did NOT raise Constraint_Error");
   exception
   when Constraint_Error =>
      Put_Line ("ok, got Constraint_Error");
   end;

   Add_Last (Integer_Array, 101);
   Put (Integer_Array);

   Put_Line ("Set_Grow");
   begin
      Set_Grow (Integer_Array, 8, -102);
      Put_Line ("oops, did NOT raise Constraint_Error");
   exception
   when Constraint_Error =>
      Put_Line ("ok, got Constraint_Error");
   end;
   Set_Grow (Integer_Array, 11, 102);
   Put (Integer_Array);

   Put_Line ("Delete first, last");
   Delete_First (Integer_Array);
   New_Line;
   Put (Integer_Array);
   Delete_Last (Integer_Array);
   New_Line;
   Put (Integer_Array);
   -- shrink on delete further tested below

   Finalize (Integer_Array);
   Test_Storage_Pools.Check_Deallocated (Array_Storage_Pool);
   Test_Storage_Pools.Reset_Counts (Array_Storage_Pool);

   --------------
   -- Prepend array

   New_Line (2);

   Put_Line ("Creating Prepend null array");
   Create
      (Array_Obj => Integer_Array,
       Space => 0,
       Growth => Prepend,
       Last => 10);
   Put (Integer_Array);

   Put_Line ("Adding First, Last");
   Add_First (Integer_Array, -101);
   begin
      -- can't Add_Last to 'prepend' array
      Add_Last (Integer_Array, 101);
      Put_Line ("oops, did NOT raise Constraint_Error");
   exception
   when Constraint_Error =>
      Put_Line ("ok, got Constraint_Error");
   end;
   Put (Integer_Array);

   Put_Line ("Set_Grow");
   -- set item 2 less than current First, to check resize
   Set_Grow (Integer_Array, 8, -103);
   Set_Grow (Integer_Array, 9, -102);
   begin
      Set_Grow (Integer_Array, 11, 102);
      Put_Line ("oops, did NOT raise Constraint_Error");
   exception
   when Constraint_Error =>
      Put_Line ("ok, got Constraint_Error");
   end;
   Put (Integer_Array);

   Put_Line ("add enough to grow, then delete enough to shrink");
   for I in reverse 0 .. First (Integer_Array) - 1 loop
      Add_First (Integer_Array, Get (Integer_Array, I + 1) - 1);
   end loop;
   Put (Integer_Array);
   for I in 0 .. 9 loop
      Delete_First (Integer_Array);
   end loop;
   New_Line;
   Put (Integer_Array);

   Finalize (Integer_Array);
   Test_Storage_Pools.Check_Deallocated (Array_Storage_Pool);
   Test_Storage_Pools.Reset_Counts (Array_Storage_Pool);

   --------------
   -- Double ended array

   New_Line (2);

   Put_Line ("Creating Both null array");
   Create
      (Array_Obj => Integer_Array,
       Space => 0,
       Growth => Both,
       First => 10);
   Put (Integer_Array);

   Put_Line ("Adding First, Last");
   Add_First (Integer_Array, -101);
   Add_Last (Integer_Array, 101);
   Put (Integer_Array);

   Put_Line ("Set_Grow");
   Set_Grow (Integer_Array, 8, -102);
   Set_Grow (Integer_Array, 11, 102);
   Put (Integer_Array);

   Put_Line ("set prepend, then delete last");
   -- use Set_Grow to test adding a lot at once
   for I in 0 .. First (Integer_Array) - 1 loop
      Set_Grow (Integer_Array, I, -110 + I);
   end loop;
   Put (Integer_Array);
   for I in 0 .. 9 loop
      Delete_Last (Integer_Array);
   end loop;
   New_Line;
   Put (Integer_Array);

   Put_Line ("add last, then delete first");
   for I in Last (Integer_Array) + 1 .. Last (Integer_Array) + 10 loop
      Add_Last (Integer_Array, Get (Integer_Array, I - 1) + 1);
   end loop;
   Put (Integer_Array);
   for I in 0 .. 9 loop
      Delete_First (Integer_Array);
   end loop;
   New_Line;
   Put (Integer_Array);

   Finalize (Integer_Array);
   Test_Storage_Pools.Check_Deallocated (Array_Storage_Pool);

end Test_Poly_Unbounded_Arrays;
