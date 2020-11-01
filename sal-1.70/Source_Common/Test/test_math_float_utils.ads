--  Abstract :
--
--  Utilities for Math_Float tests.
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

with SAL.Math_Float.DOF_6;
package Test_Math_Float_Utils is
   pragma Elaborate_Body; --  DOF_6 is.

   Default_Threshold : constant SAL.Math_Float.Real_Type := 10.0e-5;

   procedure Check
     (Message   : in String;
      Computed  : in SAL.Math_Float.Real_Type;
      Expected  : in SAL.Math_Float.Real_Type;
      Threshold : in SAL.Math_Float.Real_Type := Default_Threshold);
   --  Do AUnit.Assertions.Assert if Computed is not within Threshold
   --  of Expected.

   procedure Check
     (Message   : in String;
      Computed  : in SAL.Math_Float.DOF_6.Dual_Cart_Vector_Type;
      Expected  : in SAL.Math_Float.DOF_6.Dual_Cart_Vector_Type;
      Threshold : in SAL.Math_Float.Real_Type := Default_Threshold);

   procedure Check
     (Message   : in String;
      Computed  : in SAL.Math_Float.DOF_6.Pose_Type;
      Expected  : in SAL.Math_Float.DOF_6.Pose_Type;
      Threshold : in SAL.Math_Float.Real_Type := Default_Threshold);

   procedure Check
     (Message   : in String;
      Computed  : in SAL.Math_Float.DOF_6.Rate_Transform_Type;
      Expected  : in SAL.Math_Float.DOF_6.Rate_Transform_Type;
      Threshold : in SAL.Math_Float.Real_Type := Default_Threshold);

   procedure Check
     (Message   : in String;
      Computed  : in SAL.Math_Float.DOF_6.Wrench_Transform_Type;
      Expected  : in SAL.Math_Float.DOF_6.Wrench_Transform_Type;
      Threshold : in SAL.Math_Float.Real_Type := Default_Threshold);

end Test_Math_Float_Utils;
