--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

with AUnit.Assertions;
package body SAL.Gen_Math.Gen_DOF_3.Gen_Aunit is

   procedure Check
     (Message   : in String;
      Computed  : in Cart_Vector_Type;
      Expected  : in Cart_Vector_Type;
      Threshold : in Real_Type        := Math_AUnit.Default_Threshold)
   is begin
      Math_AUnit.Check (Message & " (X)", Computed (X), Expected (X), Threshold);
      Math_AUnit.Check (Message & " (Y)", Computed (Y), Expected (Y), Threshold);
      Math_AUnit.Check (Message & " (Z)", Computed (Z), Expected (Z), Threshold);
   end Check;

   procedure Check
     (Message    : in String;
      Computed   : in Unit_Vector_Type;
      Expected_X : in Real_Type;
      Expected_Y : in Real_Type;
      Expected_Z : in Real_Type;
      Threshold  : in Real_Type        := Math_AUnit.Default_Threshold)
   is
      Passed : constant Boolean :=
        abs (X (Computed) - Expected_X) < Threshold and
        abs (Y (Computed) - Expected_Y) < Threshold and
        abs (Z (Computed) - Expected_Z) < Threshold;
   begin
      AUnit.Assertions.Assert (Passed, Message & " failed");
   end Check;

   procedure Check
     (Message      : in String;
      Computed     : in Mag_Axis_Type;
      Expected_Mag : in Real_Type;
      Expected_X   : in Real_Type;
      Expected_Y   : in Real_Type;
      Expected_Z   : in Real_Type;
      Threshold    : in Real_Type     := Math_AUnit.Default_Threshold)
   is
      Passed : constant Boolean :=
        abs (Computed.Mag - Expected_Mag) < Threshold and
        abs (X (Computed.Axis) - Expected_X) < Threshold and
        abs (Y (Computed.Axis) - Expected_Y) < Threshold and
        abs (Z (Computed.Axis) - Expected_Z) < Threshold;
   begin
      AUnit.Assertions.Assert (Passed, Message & " failed");
   end Check;

   procedure Check
     (Message   : in String;
      Computed  : in Rot_Matrix_Type;
      Expected  : in Cart_Array_Cart_Vector_Type;
      Threshold : in Real_Type                   := Math_AUnit.Default_Threshold)
   is
      Computed_CACV : constant Cart_Array_Cart_Vector_Type := To_CACV (Computed);
      Passed        : Boolean                              := True;
   begin
      for I in Cart_Axis_Type loop
         for J in Cart_Axis_Type loop
            Passed := Passed and abs (Computed_CACV (I)(J) - Expected (I)(J)) < Threshold;
         end loop;
      end loop;
      AUnit.Assertions.Assert (Passed, Message & " failed");
   end Check;

   procedure Check
     (Message   : in String;
      Computed  : in Cart_Array_Cart_Vector_Type;
      Expected  : in Cart_Array_Cart_Vector_Type;
      Threshold : in Real_Type                   := Math_AUnit.Default_Threshold)
   is
      Passed        : Boolean                              := True;
   begin
      for I in Cart_Axis_Type loop
         for J in Cart_Axis_Type loop
            Passed := Passed and abs (Computed (I)(J) - Expected (I)(J)) < Threshold;
         end loop;
      end loop;
      AUnit.Assertions.Assert (Passed, Message & " failed");
   end Check;

   procedure Check
     (Message    : in String;
      Computed   : in Unit_Quaternion_Type;
      Expected_X : in Real_Type;
      Expected_Y : in Real_Type;
      Expected_Z : in Real_Type;
      Expected_S : in Real_Type;
      Threshold  : in Real_Type            := Math_AUnit.Default_Threshold)
   is
      Passed : constant Boolean :=
        abs (X (Computed) - Expected_X) < Threshold and
        abs (Y (Computed) - Expected_Y) < Threshold and
        abs (Z (Computed) - Expected_Z) < Threshold and
        abs (S (Computed) - Expected_S) < Threshold;
   begin
      AUnit.Assertions.Assert (Passed, Message & " failed");
   end Check;

   procedure Check
     (Message   : in String;
      Computed  : in Unit_Quaternion_Type;
      Expected  : in Unit_Quaternion_Type;
      Threshold : in Real_Type            := Math_AUnit.Default_Threshold)
   is begin
      Check
        (Message,
         Computed,
         Expected_X => Expected.X,
         Expected_Y => Expected.Y,
         Expected_Z => Expected.Z,
         Expected_S => Expected.S,
         Threshold  => Threshold);
   end Check;

   procedure Check
     (Message   : in String;
      Computed  : in Inertia_Type;
      Expected  : in Inertia_Type;
      Threshold : in Real_Type    := Math_AUnit.Default_Threshold)
   is
      Passed : Boolean := True;
   begin
      for I in Computed'Range loop
         Passed := Passed and abs (Computed (I) - Expected (I)) < Threshold;
      end loop;
      AUnit.Assertions.Assert (Passed, Message & " failed");
   end Check;

   procedure Check
     (Message    : in String;
      Computed   : in ZYX_Euler_Type;
      Expected_Z : in Real_Type;
      Expected_Y : in Real_Type;
      Expected_X : in Real_Type;
      Threshold  : in Real_Type      := Math_AUnit.Default_Threshold)
   is
      Passed : constant Boolean :=
        abs (Computed.Theta_X - Expected_X) < Threshold and
        abs (Computed.Theta_Y - Expected_Y) < Threshold and
        abs (Computed.Theta_Z - Expected_Z) < Threshold;
   begin
      AUnit.Assertions.Assert (Passed, Message & " failed");
   end Check;

   procedure Check
     (Message      : in String;
      Computed     : in Celestial_Coordinate_Type;
      Expected_RA  : in Real_Type;
      Expected_Dec : in Real_Type;
      Threshold    : in Real_Type                 := Math_AUnit.Default_Threshold)
   is
      Passed : constant Boolean :=
        abs (Computed.Right_Ascension - Expected_RA) < Threshold and
        abs (Computed.Declination - Expected_Dec) < Threshold;
   begin
      AUnit.Assertions.Assert (Passed, Message & " failed");
   end Check;

   function Fold_Pi (R : in Mag_Axis_Type) return Mag_Axis_Type
   is
      use Parent_Math_Scalar;
   begin
      if abs R.Mag = Pi then
         if X (R.Axis) /= 0.0 then
            if X (R.Axis) < 0.0 then
               return (Pi, -R.Axis);
            else
               return (Pi, R.Axis);
            end if;
         elsif Y (R.Axis) /= 0.0 then
            if Y (R.Axis) < 0.0 then
               return (Pi, -R.Axis);
            else
               return (Pi, R.Axis);
            end if;
         else
            if Z (R.Axis) < 0.0 then
               return (Pi, -R.Axis);
            else
               return (Pi, R.Axis);
            end if;
         end if;
      else
         return R;
      end if;
   end Fold_Pi;

   function Fold_Pi (R : in Cart_Vector_Type) return Cart_Vector_Type
   is
      use Cart_Vector_Ops;
      use Parent_Math_Scalar;
   begin
      if Mag (R) = Pi then
         if R (X) /= 0.0 then
            if R (X) < 0.0 then
               return -R;
            else
               return R;
            end if;
         elsif R (Y) /= 0.0 then
            if R (Y) < 0.0 then
               return -R;
            else
               return R;
            end if;
         else
            if R (Z) < 0.0 then
               return -R;
            else
               return R;
            end if;
         end if;
      else
         return R;
      end if;
   end Fold_Pi;

end SAL.Gen_Math.Gen_DOF_3.Gen_Aunit;
