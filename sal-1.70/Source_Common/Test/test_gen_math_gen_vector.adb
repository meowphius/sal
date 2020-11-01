-- Abstract :
--
-- Test SAL.Gen_Math.Gen_Vector
--
-- Design :
--
-- Only need to verify that Math.Scalar functions are called correctly.
--
-- Copyright (C) 2001, 2002, 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
with Boolean_Text_IO; use Boolean_Text_IO;
with SAL.Gen_Math.Gen_Vector;
with SAL.Gen_Array_Text_IO;
with SAL.Math_Float.Elementary;
with SAL.Math_Float.Scalar.Text_IO; use SAL.Math_Float.Scalar.Text_IO;
with SAL.Math_Float.Text_IO; use SAL.Math_Float.Text_IO;
procedure Test_Gen_Math_Gen_Vector
is
   use SAL.Math_Float;
   use SAL.Math_Float.Scalar;

   type Index_Type is (A, B, C);

   type Index_Array_Boolean_Type is array (Index_Type) of Boolean;
   type Index_Array_Real_Type is array (Index_Type) of Real_Type;
   type Index_Array_Limit_Type is array (Index_Type) of Limit_Type;

   package Vector_Math is new SAL.Math_Float.Gen_Vector
      (Elementary               => SAL.Math_Float.Elementary,
       Math_Scalar              => SAL.Math_Float.Scalar,
       Index_Type               => Index_Type,
       Index_Array_Boolean_Type => Index_Array_Boolean_Type,
       Index_Array_Real_Type    => Index_Array_Real_Type,
       Index_Array_Limit_Type   => Index_Array_Limit_Type);
   use Vector_Math;

   package Index_Array_Boolean_IO is new SAL.Gen_Array_Text_IO.Enumeration_1D
      (Element_Type             => Boolean,
       Index_Type               => Index_Type,
       Index_Array_Element_Type => Index_Array_Boolean_Type,
       Element_Put              => Boolean_Text_IO.Put,
       Element_Get              => Boolean_Text_IO.Get);
   use Index_Array_Boolean_IO;

   package Index_Array_Float_IO is new SAL.Gen_Array_Text_IO.Float_1D
      (Element_Type             => Real_Type,
       Index_Type               => Index_Type,
       Index_Array_Element_Type => Index_Array_Real_Type,
       Element_Put              => SAL.Math_Float.Text_IO.Put,
       Element_Get              => SAL.Math_Float.Text_IO.Get,
       Init_Default_Fore        => 3,
       Init_Default_Aft         => 5,
       Init_Default_Exp         => 0);
   use Index_Array_Float_IO;

   package Index_Array_Limit_IO is new SAL.Gen_Array_Text_IO.Private_1D
      (Element_Type               => SAL.Math_Float.Scalar.Limit_Type,
       Index_Type                 => Index_Type,
       Index_Array_Element_Type   => Index_Array_Limit_Type,
       Element_Put                => SAL.Math_Float.Scalar.Text_IO.Put_Item,
       Element_Get                => SAL.Math_Float.Scalar.Text_IO.Get_Item);
   use Index_Array_Limit_IO;

begin
   SAL.Math_Float.Text_IO.Num_Text_IO.Default_Fore := 3;
   SAL.Math_Float.Text_IO.Num_Text_IO.Default_Aft  := 5;
   SAL.Math_Float.Text_IO.Num_Text_IO.Default_Exp  := 0;

   Put_Line ("Testing Gen_Math.Gen_Vector");
   New_Line;
   Put ("Any ((True, False, False))  => "); Put (Any ((True, False, False))); New_Line;
   Put ("Any ((False, False, False)) => "); Put (Any ((False, False, False))); New_Line;
   Put ("Any ((True, True, True))    => "); Put (Any ((True, True, True))); New_Line;
   New_Line;

   Put ("- (1.0, 2.0, 3.0) => "); Put (-(1.0, 2.0, 3.0)); New_Line;
   Put ("abs (-1.0, 2.0, -3.0) => "); Put (abs (-1.0, 2.0, -3.0)); New_Line;

   Put ("(-1.0, 2.0, -3.0) + (-1.0, 2.0, -3.0) => ");
   Put ((-1.0, 2.0, -3.0) + (-1.0, 2.0, -3.0)); New_Line;
   Put ("(-1.0, 2.0, -3.0) - (-1.0, 2.0, -3.0) => ");
   Put ((-1.0, 2.0, -3.0) - (-1.0, 2.0, -3.0)); New_Line;
   Put ("(-1.0, 2.0, -3.0) * (-1.0, 2.0, -3.0) => ");
   Put (Index_Array_Real_Type'((-1.0, 2.0, -3.0) * (-1.0, 2.0, -3.0))); New_Line;
   Put ("(-1.0, 2.0, -3.0) / (-1.0, 2.0, -3.0) => ");
   Put ((-1.0, 2.0, -3.0) / (-1.0, 2.0, -3.0)); New_Line;

   Put ("(-1.0, 2.0, -3.0) + 3.0  => "); Put ((-1.0, 2.0, -3.0) + 3.0); New_Line;
   Put ("(-1.0, 2.0, -3.0) - 3.0  => "); Put ((-1.0, 2.0, -3.0) - 3.0); New_Line;
   Put ("(-1.0, 2.0, -3.0) * 3.0  => "); Put ((-1.0, 2.0, -3.0) * 3.0); New_Line;
   Put ("(-1.0, 2.0, -3.0) / 3.0  => "); Put ((-1.0, 2.0, -3.0) / 3.0); New_Line;

   Put ("3.0 + (-1.0, 2.0, -3.0) => "); Put (3.0 + (-1.0, 2.0, -3.0)); New_Line;
   Put ("3.0 - (-1.0, 2.0, -3.0) => "); Put (3.0 - (-1.0, 2.0, -3.0)); New_Line;
   Put ("3.0 * (-1.0, 2.0, -3.0) => "); Put (3.0 * (-1.0, 2.0, -3.0)); New_Line;
   Put ("3.0 / (-1.0, 2.0, -3.0) => "); Put (3.0 / (-1.0, 2.0, -3.0)); New_Line;

   Put ("(-1.0, 2.0, -3.0) * (-1.0, 2.0, -3.0) => ");
   Put (Real_Type'((-1.0, 2.0, -3.0) * (-1.0, 2.0, -3.0)));
   New_Line (2);

   Put ("Mask ((-1.0,  2.0, -3.0), (True, False, True)) => ");
   Put (Mask ((-1.0,  2.0, -3.0), (True, False, True)));
   New_Line (2);

   Put_Line ("Dead_Band");
   Put_Line ("    (Item =>        (-1.0,  2.0, -3.0),");
   Put_Line ("     Lower_Limit => ( 2.0,  0.5,  0.5)) => ");
   Put
      (Dead_Band
       (Item =>        (-1.0,  2.0, -3.0),
        Lower_Limit => (2.0,  0.5,  0.5)));
   New_Line (2);

   Put_Line ("Detent");
   Put_Line ("    (Item =>        (-1.0,  2.0, -3.0),");
   Put_Line ("     Dead_Band   => ( 2.0,  0.5,  0.5),");
   Put_Line ("     Upper_Limit => ( 3.0,  2.5,  2.5));");
   Put
      (Detent
       (Item =>        (-1.0,  2.0, -3.0),
        Dead_Band =>   (2.0,  0.5,  0.5),
        Upper_Limit => (3.0,  2.5,  2.5)));
   New_Line (2);

   Put_Line ("To_Limit");
   Put_Line ("    (Low  =>  (-2.0, -2.0, -1.0),");
   Put_Line ("     High =>  ( 1.0,  2.0,  3.0));");
   Put
      (To_Limit
       (Low =>   (-2.0, -2.0, -1.0),
        High =>  (1.0, 2.0, 3.0)));
   New_Line (2);

   Put_Line ("and");
   Put_Line ("    (Left =>  ((-1.0, 1.0), (-2.0, 2.0), (-3.0, 3.0)),");
   Put_Line ("     Right => ((-2.0, 1.0), (-2.0, 3.0), (-1.0, 1.0))));");
   Put (Index_Array_Limit_Type'
      (Vector_Math."and"
       (Left  => (To_Limit (-1.0, 1.0), To_Limit (-2.0, 2.0), To_Limit (-3.0, 3.0)),
        Right => (To_Limit (-2.0, 1.0), To_Limit (-2.0, 3.0), To_Limit (-1.0, 1.0)))));
   New_Line (2);

   Put_Line ("Clip");
   declare
      Item    : Index_Array_Real_Type           := (-1.0,  2.0, -3.0);
      Limit   : constant Index_Array_Limit_Type := (To_Limit (-1.0, 1.0), To_Limit (-1.0, 1.0), To_Limit (-1.0, 1.0));
      Clipped : Index_Array_Boolean_Type;
   begin
      Put ("    (Item  => "); Put (Item); New_Line;
      Put ("     Limit => "); Put (Limit); New_Line;
      Clip (Item, Limit, Clipped);
      Put ("Item    => "); Put (Item); New_Line;
      Put ("Clipped => "); Put (Clipped);
      New_Line (2);
   end;

   Put_Line (" <=");
   declare
      Item  : constant Index_Array_Real_Type  := (-1.0,  2.0, -3.0);
      Limit : constant Index_Array_Limit_Type := (To_Limit (-1.0, 1.0), To_Limit (-1.0, 1.0), To_Limit (-1.0, 1.0));
   begin
      Put (Item); Put (" <= "); Put (Limit);
      Put (" => "); Put (Item <= Limit);
      New_Line (2);
   end;

   Put_Line ("Scale_Limit");
   declare

      procedure Test
         (Item  : in Index_Array_Real_Type;
          Limit : in Real_Type)
      is
         Scaled_Item : Index_Array_Real_Type := Item;
         Scaled      : Boolean;
      begin
         Put ("Item  => "); Put (Item); New_Line;
         Put ("Limit => "); Put (Limit); New_Line;

         Scale_Limit (Scaled_Item, Limit, Scaled);

         Put ("Item   => "); Put (Scaled_Item); New_Line;
         Put ("Scaled => "); Put (Scaled); New_Line (2);
      end Test;

   begin
      Test ((-1.0,  2.0, -3.0), -1.0);
      Test ((-1.0,  2.0, -3.0),  4.0);
   end;

end Test_Gen_Math_Gen_Vector;
