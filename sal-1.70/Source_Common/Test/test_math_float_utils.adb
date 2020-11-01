--  Abstract :
--
--  see specs
--
--  Copyright (C) 2003 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.

with AUnit.Assertions;
with SAL.Math_Float.DOF_3;
package body Test_Math_Float_Utils is

   procedure Check
     (Message   : in String;
      Computed  : in SAL.Math_Float.Real_Type;
      Expected  : in SAL.Math_Float.Real_Type;
      Threshold : in SAL.Math_Float.Real_Type := Default_Threshold)
   is begin
      AUnit.Assertions.Assert
        (abs (Computed - Expected) < Threshold,
         Message &
           " failed; expected " & SAL.Math_Float.Real_Type'Image (Expected) &
           " got " & SAL.Math_Float.Real_Type'Image (Computed));
   end Check;

   procedure Check
     (Message   : in String;
      Computed  : in SAL.Math_Float.DOF_6.Dual_Cart_Vector_Type;
      Expected  : in SAL.Math_Float.DOF_6.Dual_Cart_Vector_Type;
      Threshold : in SAL.Math_Float.Real_Type := Default_Threshold)
   is begin
      for I in Expected'Range loop
         Check (Message, Computed (I), Expected (I), Threshold);
      end loop;
   end Check;

   procedure Check
     (Message   : in String;
      Computed  : in SAL.Math_Float.DOF_6.Pose_Type;
      Expected  : in SAL.Math_Float.DOF_6.Pose_Type;
      Threshold : in SAL.Math_Float.Real_Type := Default_Threshold)
   is
      use SAL.Math_Float.DOF_3;
   begin
      for I in Expected.Translation'Range loop
         Check (Message, Computed.Translation (I), Expected.Translation (I), Threshold);
      end loop;
      Check (Message, X (Computed.Rotation), X (Expected.Rotation), Threshold);
      Check (Message, Y (Computed.Rotation), Y (Expected.Rotation), Threshold);
      Check (Message, Z (Computed.Rotation), Z (Expected.Rotation), Threshold);
      Check (Message, S (Computed.Rotation), S (Expected.Rotation), Threshold);
   end Check;

   procedure Check
     (Message   : in String;
      Computed  : in SAL.Math_Float.DOF_6.Rate_Transform_Type;
      Expected  : in SAL.Math_Float.DOF_6.Rate_Transform_Type;
      Threshold : in SAL.Math_Float.Real_Type := Default_Threshold)
   is
      use SAL.Math_Float.DOF_6;
      Computed_DCV : constant DC_Array_DCV_Type := To_DC_Array_DCV (Computed);
      Expected_DCV : constant DC_Array_DCV_Type := To_DC_Array_DCV (Expected);
   begin
      for I in Expected_DCV'Range loop
         for J in Expected_DCV (I)'Range loop
            Check (Message, Computed_DCV (I)(J), Expected_DCV (I)(J), Threshold);
         end loop;
      end loop;
   end Check;

   procedure Check
     (Message   : in String;
      Computed  : in SAL.Math_Float.DOF_6.Wrench_Transform_Type;
      Expected  : in SAL.Math_Float.DOF_6.Wrench_Transform_Type;
      Threshold : in SAL.Math_Float.Real_Type := Default_Threshold)
   is
      use SAL.Math_Float.DOF_6;
      Computed_DCV : constant DC_Array_DCV_Type := To_DC_Array_DCV (Computed);
      Expected_DCV : constant DC_Array_DCV_Type := To_DC_Array_DCV (Expected);
   begin
      for I in Expected_DCV'Range loop
         for J in Expected_DCV (I)'Range loop
            Check (Message, Computed_DCV (I)(J), Expected_DCV (I)(J), Threshold);
         end loop;
      end loop;
   end Check;

end Test_Math_Float_Utils;
