--  Abstract :
--
--  Utilities for AUnit tests of Gen_DOF_3 stuff.
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

pragma License (Modified_GPL);

with SAL.Gen_Math.Gen_AUnit;

generic
   with package Math_AUnit is new SAL.Gen_Math.Gen_AUnit;
package SAL.Gen_Math.Gen_DOF_3.Gen_Aunit is
   pragma Elaborate_Body; --  Parent is.

   procedure Check
     (Message   : in String;
      Computed  : in Cart_Vector_Type;
      Expected  : in Cart_Vector_Type;
      Threshold : in Real_Type        := Math_AUnit.Default_Threshold);

   procedure Check
     (Message    : in String;
      Computed   : in Unit_Vector_Type;
      Expected_X : in Real_Type;
      Expected_Y : in Real_Type;
      Expected_Z : in Real_Type;
      Threshold  : in Real_Type        := Math_AUnit.Default_Threshold);

   procedure Check
     (Message      : in String;
      Computed     : in Mag_Axis_Type;
      Expected_Mag : in Real_Type;
      Expected_X   : in Real_Type;
      Expected_Y   : in Real_Type;
      Expected_Z   : in Real_Type;
      Threshold    : in Real_Type     := Math_AUnit.Default_Threshold);

   procedure Check
     (Message   : in String;
      Computed  : in Rot_Matrix_Type;
      Expected  : in Cart_Array_Cart_Vector_Type;
      Threshold : in Real_Type                   := Math_AUnit.Default_Threshold);

   procedure Check
     (Message   : in String;
      Computed  : in Cart_Array_Cart_Vector_Type;
      Expected  : in Cart_Array_Cart_Vector_Type;
      Threshold : in Real_Type                   := Math_AUnit.Default_Threshold);

   procedure Check
     (Message    : in String;
      Computed   : in Unit_Quaternion_Type;
      Expected_X : in Real_Type;
      Expected_Y : in Real_Type;
      Expected_Z : in Real_Type;
      Expected_S : in Real_Type;
      Threshold  : in Real_Type            := Math_AUnit.Default_Threshold);

   procedure Check
     (Message   : in String;
      Computed  : in Unit_Quaternion_Type;
      Expected  : in Unit_Quaternion_Type;
      Threshold : in Real_Type            := Math_AUnit.Default_Threshold);

   procedure Check
     (Message   : in String;
      Computed  : in Inertia_Type;
      Expected  : in Inertia_Type;
      Threshold : in Real_Type    := Math_AUnit.Default_Threshold);

   procedure Check
     (Message    : in String;
      Computed   : in ZYX_Euler_Type;
      Expected_Z : in Real_Type;
      Expected_Y : in Real_Type;
      Expected_X : in Real_Type;
      Threshold  : in Real_Type      := Math_AUnit.Default_Threshold);

   procedure Check
     (Message      : in String;
      Computed     : in Celestial_Coordinate_Type;
      Expected_RA  : in Real_Type;
      Expected_Dec : in Real_Type;
      Threshold    : in Real_Type                 := Math_AUnit.Default_Threshold);

   function Fold_Pi (R : in Mag_Axis_Type) return Mag_Axis_Type;
   function Fold_Pi (R : in Cart_Vector_Type) return Cart_Vector_Type;
   --  Normalize to +Pi, +Axis.X. This is done because rotations of
   --  magnitude Pi are the same if the axis is negated; it often is
   --  by round-off noise.

end SAL.Gen_Math.Gen_DOF_3.Gen_Aunit;
