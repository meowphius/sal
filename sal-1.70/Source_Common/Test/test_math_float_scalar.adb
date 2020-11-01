--  Abstract:
--
--  Test each subprogram in the SAL.Gen_Math.Gen_Scalar package, using the
--  Float instantiation.
--
--  Copyright (C) 2001 - 2003 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;
with Ada.Numerics;
with Ada.Text_IO;                   use  Ada.Text_IO;
with Boolean_Text_IO;               use Boolean_Text_IO;
with SAL.Math_Float.Scalar;         use SAL.Math_Float.Scalar;
with SAL.Math_Float.Scalar.Text_IO; use SAL.Math_Float.Scalar.Text_IO;
with SAL.Math_Float.Text_IO;        use SAL.Math_Float.Text_IO;
procedure Test_Math_Float_Scalar
is
   use SAL.Math_Float; -- for Float_Type
begin
   Num_Text_IO.Default_Exp := 0;

   New_Line;
   Test_Mod :
   begin
      Put ("Mod (10.0, 5.0) : "); Put ("Mod" (10.0, 5.0)); New_Line;
      Put ("Mod (11.0, 5.0) : "); Put ("Mod" (11.0, 5.0)); New_Line;
      Put ("Mod (14.0, 5.0) : "); Put ("Mod" (14.0, 5.0)); New_Line;
      Put ("Mod (10.0, -5.0) : "); Put ("Mod" (10.0, -5.0)); New_Line;
      Put ("Mod (11.0, -5.0) : "); Put ("Mod" (11.0, -5.0)); New_Line;
      Put ("Mod (14.0, -5.0) : "); Put ("Mod" (14.0, -5.0)); New_Line;
      Put ("Mod (-10.0, 5.0) : "); Put ("Mod" (-10.0, 5.0)); New_Line;
      Put ("Mod (-11.0, 5.0) : "); Put ("Mod" (-11.0, 5.0)); New_Line;
      Put ("Mod (-14.0, 5.0) : "); Put ("Mod" (-14.0, 5.0)); New_Line;
      Put ("Mod (-10.0, -5.0) : "); Put ("Mod" (-10.0, -5.0)); New_Line;
      Put ("Mod (-11.0, -5.0) : "); Put ("Mod" (-11.0, -5.0)); New_Line;
      Put ("Mod (-14.0, -5.0) : "); Put ("Mod" (-14.0, -5.0)); New_Line;

      -- We don't test for Constraint_Error on mod (n, 0), because we
      -- compile the release version with constraint checks disabled.
--       begin
--          Put ("Mod (10.0, 0.0) : "); Put ("Mod" (10.0, 0.0)); New_Line;
--       exception
--       when Constraint_Error =>
--          Put_Line ("ok, Constraint_Error raised");
--       end;

      Put ("Mod (0.0, 2.0) : "); Put ("Mod" (0.0, 2.0)); New_Line;
   end Test_Mod;

   New_Line;
   Test_Dead_Band :
   begin
      Put ("Dead_Band (10.0, 2.0) : "); Put (Dead_Band (10.0, 2.0)); New_Line;
      Put ("Dead_Band (1.0, 2.0) : "); Put (Dead_Band (1.0, 2.0)); New_Line;
      Put ("Dead_Band (-10.0, 2.0) : "); Put (Dead_Band (-10.0, 2.0)); New_Line;
      Put ("Dead_Band (-1.0, 2.0) : "); Put (Dead_Band (-1.0, 2.0)); New_Line;
   end Test_Dead_Band;

   New_Line;
   Test_Detent :
   begin
      Put ("Detent (-1.0, 2.0, 4.0): "); Put (Detent (-1.0, 2.0, 4.0)); New_Line;
      Put ("Detent (1.0, 2.0, 4.0): "); Put (Detent (1.0, 2.0, 4.0)); New_Line;
      Put ("Detent (3.0, 2.0, 4.0): "); Put (Detent (3.0, 2.0, 4.0)); New_Line;
      Put ("Detent (-3.0, 2.0, 4.0): "); Put (Detent (-3.0, 2.0, 4.0)); New_Line;
      Put ("Detent (5.0, 2.0, 4.0): "); Put (Detent (5.0, 2.0, 4.0)); New_Line;
      Put ("Detent (-5.0, 2.0, 4.0): "); Put (Detent (-5.0, 2.0, 4.0)); New_Line;
   end Test_Detent;

   New_Line;
   Test_Limit :
   declare
      A_Limit : constant Limit_Type := To_Limit (-1.0, 1.0);
   begin
      Put ("To_Limit (-1.0, 1.0): "); Put (A_Limit); New_Line;
      begin
         Put ("To_Limit (1.0, -1.0): "); Put (To_Limit (1.0, -1.0)); New_Line;
         Put_Line ("Invalid_Limit NOT raised");
      exception
      when SAL.Invalid_Limit =>
         Put_Line ("ok, Invalid_Limit raised");
      end;
      Put ("Low  ((-1.0, 1.0)): "); Put (Low (A_Limit)); New_Line;
      Put ("High ((-1.0, 1.0)): "); Put (High (A_Limit)); New_Line;
      Put ("(-1.0, 1.0) and (-0.5, 0.5): "); Put (To_Limit (-1.0, 1.0) and To_Limit (-0.5, 0.5)); New_Line;
      Put ("(-1.0, 1.0) and (-0.5, 1.5): "); Put (To_Limit (-1.0, 1.0) and To_Limit (-0.5, 1.5)); New_Line;
      Put ("(-1.0, 1.0) and (-1.5, 0.5): "); Put (To_Limit (-1.0, 1.0) and To_Limit (-1.5, 0.5)); New_Line;
      begin
         Put ("(-1.0, 1.0) and (1.5, 2.5): "); Put (To_Limit (-1.0, 1.0) and To_Limit (1.5, 2.5)); New_Line;
         Put_Line ("Invalid_Limit NOT raised");
      exception
      when SAL.Invalid_Limit =>
         Put_Line ("ok, Invalid_Limit raised");
      end;
      begin
         Put ("(-1.0, 1.0) and (-2.5, -1.5): "); Put (To_Limit (-1.0, 1.0) and To_Limit (-2.5, -1.5)); New_Line;
         Put_Line ("Invalid_Limit NOT raised");
      exception
      when SAL.Invalid_Limit =>
         Put_Line ("ok, Invalid_Limit raised");
      end;
   end Test_Limit;

   New_Line;
   Test_Clip :
   declare

      procedure Test_Clip
         (Item  : in Real_Type;
          Limit : in Limit_Type)
      is
         Temp_Item : Real_Type := Item;
         Clipped   : Boolean;
      begin
         Clip (Temp_Item, Limit, Clipped);
         Put (Item); Put (" ");
         Put (Limit); Put (" ");
         Put (Temp_Item); Put (" ");
         Put (Clipped);
         New_Line;
      end Test_Clip;
   begin
      Put_Line ("   Item           Limit            Result  Clipped");
      Test_Clip (-2.0, To_Limit (-1.0, 1.0));
      Test_Clip (-1.0, To_Limit (-1.0, 1.0));
      Test_Clip (0.0, To_Limit (-1.0, 1.0));
      Test_Clip (1.0, To_Limit (-1.0, 1.0));
      Test_Clip (2.0, To_Limit (-1.0, 1.0));
   end Test_Clip;

   New_Line;
   Test_Less_Equal :
   begin
      Put ("-3.0 <= (-2.0, 2.0) => "); Put (-3.0 <= To_Limit (-2.0, 2.0)); New_Line;
      Put ("-2.0 <= (-2.0, 2.0) => "); Put (2.0 <= To_Limit (-2.0, 2.0)); New_Line;
      Put (" 0.0 <= (-2.0, 2.0) => "); Put (0.0 <= To_Limit (-2.0, 2.0)); New_Line;
      Put (" 2.0 <= (-2.0, 2.0) => "); Put (2.0 <= To_Limit (-2.0, 2.0)); New_Line;
      Put (" 3.0 <= (-2.0, 2.0) => "); Put (3.0 <= To_Limit (-2.0, 2.0)); New_Line;
   end Test_Less_Equal;

   New_Line;
   Test_Sin_Cos :
   declare

      procedure Test_Angle (I : in Integer)
      is
         Trig : constant Trig_Pair_Type := Sin_Cos (Pi / 8.0 * Real_Type (I));
      begin
         Put (I, Width => 4); Put (" ");
         Put (Trig);
         Put (Atan2 (Trig));
         New_Line;
      end Test_Angle;
   begin
      -- test every octant in range -2 PI .. +2 PI, since some algorithms have switches on the octant.
      Put_Line ("  PI/8        Sin_Cos         Atan2");
      for I in -16 .. 16
      loop
         Test_Angle (I);
      end loop;

      New_Line;
      begin
         Put ("Atan2 ((0.0, 0.0)) : ");
         Put (Atan2 (Unchecked_Trig_Pair (0.0, 0.0)));
         New_Line;
      exception
      when Ada.Numerics.Argument_Error =>
         Put_Line ("ok, Argument_Error raised");
      end;

      New_Line;
      Put ("To_Trig_Pair (0.0, 1.0) : "); Put (To_Trig_Pair (0.0, 1.0)); New_Line;
      Put ("To_Trig_Pair (4.0, 3.0) : "); Put (To_Trig_Pair (4.0, 3.0)); New_Line;
      begin
         Put ("To_Trig_Pair (0.0, 0.0) : "); Put (To_Trig_Pair (0.0, 0.0)); New_Line;
      exception
      when SAL.Non_Normalizable_Trig_Pair =>
         Put_Line ("ok, Non_Normalizable_Trig_Pair raised");
      end;

   end Test_Sin_Cos;

   New_Line;
   Test_Trig_Sum_Diff :
   declare
      Small              : constant Real_Type      := First_Order_Trig / 2.0;
      Sin_Cos_Small      : constant Trig_Pair_Type := Sin_Cos (Small);
      Half_Sin_Cos_Small : constant Trig_Pair_Type := Half_Trig (Sin_Cos_Small);
   begin
      Put ("Small = "); Put (Small, Exp => 3); New_Line;
      Put ("Half_Trig (Sin_Cos (0.0)) = ");
      Put (Half_Trig (Sin_Cos (0.0))); New_Line;
      Put ("Half_Trig (Sin_Cos (small)) = ");
      -- this tests Sin, Cos
      Put (Sin (Half_Sin_Cos_Small), Exp => 3); Put (Cos (Half_Sin_Cos_Small), Exp => 3); New_Line;

      Put ("Half_Trig (Sin_Cos (PI/2.0)) = ");
      Put (Half_Trig (Sin_Cos (Pi / 2.0))); New_Line;
      Put ("Half_Trig (Sin_Cos (-PI/4.0)) = ");
      Put (Half_Trig (Sin_Cos (-Pi/4.0))); New_Line;

      Put ("Sin_Cos ((PI-small)) = "); Put (Sin_Cos (Pi - Small)); New_Line;
      Put ("Sin_Cos ((PI-small)/2) = "); Put (Sin_Cos ((Pi - Small) / 2.0)); New_Line;
      Put ("Half_Trig (Sin_Cos (PI - small)) = "); Put (Half_Trig (Sin_Cos (Pi - Small))); New_Line;

      Put ("Double_Trig (Sin_Cos (0.0)) = ");
      Put (Double_Trig (Sin_Cos (0.0))); New_Line;
      Put ("Double_Trig (Sin_Cos (-0.2)) = ");
      Put (Double_Trig (Sin_Cos (-0.2))); New_Line;
      Put ("Double_Trig (Sin_Cos (Pi/4.0)) = ");
      Put (Double_Trig (Sin_Cos (Pi / 4.0))); New_Line;

      Put ("Sin_Cos (0.1) + Sin_Cos (0.2) = ");
      Put (Sin_Cos (0.1) + Sin_Cos (0.2)); New_Line;
      Put ("Sin_Cos (0.1) - Sin_Cos (0.2) = ");
      Put (Sin_Cos (0.1) - Sin_Cos (0.2)); New_Line;
   end Test_Trig_Sum_Diff;

end Test_Math_Float_Scalar;
