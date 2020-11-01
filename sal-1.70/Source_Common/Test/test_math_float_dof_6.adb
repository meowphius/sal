-- Abstract :
--
-- Test all subprograms in the SAL.Math_Float.DOF_6 package, that are
-- not generic instantiations. In general, this means verifying that
-- SAL.Math_Float.DOF_3 operations are called correctly; here we
-- assume SAL.Math_Float.DOF_3 is correct.
--
-- Copyright (C) 2001 - 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;                   use Ada.Text_IO;
with Boolean_Text_IO;               use Boolean_Text_IO;
with SAL.Math_Float.DOF_3.Text_IO;  use SAL.Math_Float.DOF_3.Text_IO;
with SAL.Math_Float.DOF_6.Text_IO;  use SAL.Math_Float.DOF_6.Text_IO;
with SAL.Math_Float.Scalar.Text_IO; use SAL.Math_Float.Scalar.Text_IO;
with SAL.Math_Float.Text_IO;        use SAL.Math_Float.Text_IO;
procedure Test_Math_Float_DOF_6
is
   use SAL.Math_Float, SAL.Math_Float.DOF_3, SAL.Math_Float.DOF_6;
begin

   SAL.Math_Float.Text_IO.Num_Text_IO.Default_Exp := 0;

   Test_Dual_Real_Type :
   declare
      use Dual_Real_Ops;
   begin
      SAL.Math_Float.DOF_6.Text_IO.Dual_Real_Text_IO.Default_Exp := 0;

      Put_Line ("Testing Dual_Real_Type");
      Put ("(1.0, 1.0) <= (1.0, 1.0): "); Put (Dual_Real_Type'(1.0, 1.0) <= Dual_Real_Type'(1.0, 1.0)); New_Line;
      Put ("(1.0, 1.0) <= (1.1, 1.1): "); Put (Dual_Real_Type'(1.0, 1.0) <= Dual_Real_Type'(1.1, 1.1)); New_Line;
      Put ("(1.1, 1.1) <= (1.0, 1.0): "); Put (Dual_Real_Type'(1.1, 1.1) <= Dual_Real_Type'(1.0, 1.0)); New_Line;
      Put ("(1.1, 1.0) <= (1.0, 1.1): "); Put (Dual_Real_Type'(1.1, 1.0) <= Dual_Real_Type'(1.0, 1.1)); New_Line;
      Put ("(1.0, 1.1) <= (1.1, 1.0): "); Put (Dual_Real_Type'(1.0, 1.1) <= Dual_Real_Type'(1.1, 1.0)); New_Line;
      New_Line;

      Put ("2.0 * (3.0, 4.0) : "); Put (2.0 * Dual_Real_Type'(3.0, 4.0)); New_Line;
      Put ("(3.0, 4.0) * 2.0 : "); Put (2.0 * Dual_Real_Type'(3.0, 4.0)); New_Line;
      New_Line;
   end Test_Dual_Real_Type;

   Test_Dual_Cart_Vector :
   declare
      A_Dual_Cart_Vector : constant Dual_Cart_Vector_Type := (1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
      A_Dual_Real        : constant Dual_Real_Type       := (7.0, 8.0);
      A_Quaternion       : constant Unit_Quaternion_Type  := To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit));
   begin
      SAL.Math_Float.DOF_6.Text_IO.Dual_Cart_Vector_Text_IO.Default_Exp := 0;
      SAL.Math_Float.DOF_3.Text_IO.Cart_Vector_Text_IO.Default_Exp      := 0;

      Put_Line (" Testing Dual_Cart_Vector_Type non-generic operations");
      Put ("DCV         : "); Put (A_Dual_Cart_Vector); New_Line;
      Put ("Translation : "); Put (Translation (A_Dual_Cart_Vector)); New_Line;
      Put ("Rotation    : "); Put (Rotation (A_Dual_Cart_Vector)); New_Line;
      Put ("Trans & Rot : "); Put (Dual_Cart_Vector_Type'(Cart_Vector_Type'(1.0, 2.0, 3.0) &
                                                          Cart_Vector_Type'(4.0, 5.0, 6.0)));
      New_Line;
      Put ("Mag         : "); Put (Mag (A_Dual_Cart_Vector)); New_Line;
      New_Line;

      Put ("DCV             : "); Put (A_Dual_Cart_Vector); New_Line;
      Put ("Dual_Real       : "); Put (A_Dual_Real); New_Line;
      Put ("Dual_Real * DCV : "); Put (A_Dual_Real * A_Dual_Cart_Vector); New_Line;
      Put ("DCV * Dual_Real : "); Put (A_Dual_Cart_Vector * A_Dual_Real); New_Line;
      Put ("DCV / Dual_Real : "); Put (A_Dual_Cart_Vector / A_Dual_Real); New_Line;
      New_Line;

      -- only need to verify that SAL.Math_Float.DOF_3 is called correctly.
      Put ("Quat                      : "); Put (A_Quaternion); New_Line;
      Put ("DCV                       : "); Put (A_Dual_Cart_Vector); New_Line;
      Put ("Quat * DCV                : "); Put (A_Quaternion * A_Dual_Cart_Vector); New_Line;
      Put ("Inverse(Quat) * DCV       : "); Put (Inverse (A_Quaternion) * A_Dual_Cart_Vector); New_Line;
      Put ("Inverse_Times (Quat, DCV) : "); Put (Inverse_Times (A_Quaternion, A_Dual_Cart_Vector)); New_Line;
      New_Line;
   end Test_Dual_Cart_Vector;

   Test_Pose :
   declare
      A_Quaternion : constant Unit_Quaternion_Type := To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit));
      A_Pose       : constant Pose_Type            := ((1.0, 2.0, 3.0), A_Quaternion);

      procedure Test_Mult (Left, Right : in Pose_Type)
      is
      begin
         Put ("Left                        : "); Put (Left); New_Line;
         Put ("Right                       : "); Put (Right); New_Line;
         Put ("Left * Right                : "); Put (Left * Right); New_Line;
         Put ("Left.Rot * Right            : "); Put (Left.Rotation * Right); New_Line;
         Put ("Left * Right.Rot            : "); Put (Left * Right.Rotation); New_Line;
         Put ("(Left * Right.Tran).tran    : "); Put (Cart_Vector_Type'(Left * Right.Translation)); New_Line;
         Put ("Inverse (Left) * Right      : "); Put (Inverse (Left) * Right); New_Line;
         Put ("Inverse_Times (Left, Right) : "); Put (Inverse_Times (Left, Right)); New_Line;
      end Test_Mult;

      procedure Test_Add_Subtract (Base_T_One, Base_T_Two : in Pose_Type)
      is
         One_T_Two : constant Dual_Cart_Vector_Type := Base_T_Two - Base_T_One;
      begin
         Put ("Base_T_One              : "); Put (Base_T_One); New_Line;
         Put ("Base_T_Two              : "); Put (Base_T_Two); New_Line;
         Put ("Base_T_Two - Base_T_One : "); Put (One_T_Two); New_Line;
         Put ("Base_T_One + One_T_Two  : "); Put (Base_T_One + One_T_Two); New_Line;
         Put ("Base_T_Two - One_T_Two  : "); Put (Base_T_Two - One_T_Two); New_Line;
         Put ("Base_T_One + One_T_Two.Tran : "); Put (Pose_Type'(Base_T_One + Translation (One_T_Two))); New_Line;
         Put ("One_T_Two + Two_T_Base      : "); Put (One_T_Two + Inverse (Base_T_Two)); New_Line;
         Put ("One_T_Base                  : "); Put (Inverse (Base_T_One)); New_Line;
         Put ("One_T_Two.Tran + Two_T_Base : "); Put (Translation (One_T_Two) + Inverse (Base_T_Two)); New_Line;
      end Test_Add_Subtract;

   begin
      Put_Line (" Testing Pose_Type");

      Put ("A_Pose              : "); Put (A_Pose); New_Line;
      Put ("To_Dual_Cart_Vector : "); Put (To_Dual_Cart_Vector (A_Pose)); New_Line;
      Put (" and back           : "); Put (To_Pose (To_Dual_Cart_Vector (A_Pose))); New_Line;
      Put ("Mag                 : "); Put (Mag (A_Pose)); New_Line;
      Put ("Inverse             : "); Put (Inverse (A_Pose)); New_Line;
      Put ("Pose * Inverse      : "); Put (A_Pose * Inverse (A_Pose)); New_Line;
      New_Line;

      Test_Mult (A_Pose, A_Pose);
      New_Line;

      Test_Add_Subtract (Pose_Type'((0.0, 0.0, 0.0), To_Unit_Quaternion (0.0, 0.0, 0.0, 1.0)),
                         Pose_Type'((0.0, 2.0, 0.0), To_Unit_Quaternion (1.0, 0.0, 0.0, 1.0)));
      Test_Add_Subtract (A_Pose, A_Pose);
      New_Line;

   end Test_Pose;

   -- dc_array_dcv ops are all generic instantiations

   Test_Rate_Wrench_Transform :
   declare
      use DC_Array_DCV_Ops;

      X_Quaternion : constant Unit_Quaternion_Type  := To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit));
      A_T_B        : constant Pose_Type             := ((1.0, 0.0, 0.0), X_Quaternion);
      A_TR_B       : constant Rate_Transform_Type   := To_Rate_Transform (A_T_B);
      A_TW_B       : constant Wrench_Transform_Type := To_Wrench_Transform (A_T_B);
      Y_Quaternion : constant Unit_Quaternion_Type  := To_Unit_Quaternion (Mag_Axis_Type'(0.1, Y_Unit));
      B_T_C        : constant Pose_Type             := ((0.0, 1.0, 0.0), Y_Quaternion);
      B_TR_C       : constant Rate_Transform_Type   := To_Rate_Transform (B_T_C);
      B_TW_C       : constant Wrench_Transform_Type := To_Wrench_Transform (B_T_C);

      procedure Test_Transform (Xform : in Pose_Type)
      is
         Rate_Transform   : constant Rate_Transform_Type   := To_Rate_Transform (Xform);
         Wrench_Transform : constant Wrench_Transform_Type := To_Wrench_Transform (Xform);
         Temp             : DC_Array_DCV_Type;

         function Transpose (Item : in DC_Array_DCV_Type) return DC_Array_DCV_Type
            -- make output more readable
         is
            Result : DC_Array_DCV_Type;
         begin
            for I in Dual_Cart_Axis_Type
            loop
               for J in Dual_Cart_Axis_Type
               loop
                  Result (J) (I) := Item (I) (J);
               end loop;
            end loop;
            return Result;
         end Transpose;
      begin
         Put ("Xform : "); Put (Xform); New_Line;
         Put_Line ("To_Rate_Transform (Xform) : ");
         Put (Rate_Transform); New_Line;

         Put_Line ("matrix from Rate_Transform * DCV : ");
         Temp :=
            (Rate_Transform * (1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
             Rate_Transform * (0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
             Rate_Transform * (0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
             Rate_Transform * (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
             Rate_Transform * (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
             Rate_Transform * (0.0, 0.0, 0.0, 0.0, 0.0, 1.0));
         Put (Transpose (Temp), Single_Line_Array => False); New_Line;

         Put_Line ("matrix from Transform_Rate (Xform, DCV) : ");
         Temp :=
            (Transform_Rate (Xform, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
             Transform_Rate (Xform, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0)));
         Put (Transpose (Temp), Single_Line_Array => False); New_Line;

         Put_Line ("To_Rate_Transform (Xform.Translation, Zero_Unit_Quaternion) : ");
         Put (To_Rate_Transform ((Xform.Translation, Zero_Unit_Quaternion)));
         New_Line;

         Put_Line ("matrix from Transform_Rate (Xform.Tran, DCV) : ");
         Temp :=
            (Transform_Rate (Xform.Translation, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform.Translation, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform.Translation, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
             Transform_Rate (Xform.Translation, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
             Transform_Rate (Xform.Translation, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
             Transform_Rate (Xform.Translation, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0)));
         Put (Transpose (Temp), Single_Line_Array => False); New_Line;

         Put_Line ("To_Wrench_Transform (Xform) : ");
         Put (To_Wrench_Transform (Xform)); New_Line;

         Put_Line ("matrix from Wrench_Transform * DCV : ");
         Temp :=
            (Wrench_Transform * (1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
             Wrench_Transform * (0.0, 0.0, 0.0, 0.0, 0.0, 1.0));
         Put (Transpose (Temp), Single_Line_Array => False); New_Line;

         Put_Line ("matrix from Transform_Wrench (Xform, DCV) : ");
         Temp :=
            (Transform_Wrench (Xform, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
             Transform_Wrench (Xform, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0)));
         Put (Transpose (Temp), Single_Line_Array => False); New_Line;

         Put_Line ("To_Wrench_Transform (Xform.Translation, Zero_Unit_Quaternion) : ");
         Put (To_Wrench_Transform ((Xform.Translation, Zero_Unit_Quaternion)));
         New_Line;

         Put_Line ("matrix from Transform_Wrench (Xform.Tran, DCV) : ");
         Temp :=
            (Transform_Wrench (Xform.Translation, (1.0, 0.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform.Translation, (0.0, 1.0, 0.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform.Translation, (0.0, 0.0, 1.0, 0.0, 0.0, 0.0)),
             Transform_Wrench (Xform.Translation, (0.0, 0.0, 0.0, 1.0, 0.0, 0.0)),
             Transform_Wrench (Xform.Translation, (0.0, 0.0, 0.0, 0.0, 1.0, 0.0)),
             Transform_Wrench (Xform.Translation, (0.0, 0.0, 0.0, 0.0, 0.0, 1.0)));
         Put (Transpose (Temp), Single_Line_Array => False); New_Line;

         Put_Line ("matrix from Transform_Force (Xform.Tran, DCV.Tran) : ");
         Temp :=
            (Transform_Force (Xform.Translation, (1.0, 0.0, 0.0)),
             Transform_Force (Xform.Translation, (0.0, 1.0, 0.0)),
             Transform_Force (Xform.Translation, (0.0, 0.0, 1.0)),
             Transform_Force (Xform.Translation, (0.0, 0.0, 0.0)),
             Transform_Force (Xform.Translation, (0.0, 0.0, 0.0)),
             Transform_Force (Xform.Translation, (0.0, 0.0, 0.0)));
         Put (Transpose (Temp), Single_Line_Array => False); New_Line;

      end Test_Transform;
   begin
      Put_Line (" Testing transform_type");
      Put ("A_T_B :"); Put (A_T_B); New_Line;
      Put_Line ("A_TR_B : "); Put (A_TR_B); New_Line;
      Put_Line ("A_TW_B : "); Put (A_TW_B); New_Line;

      Put_Line ("To_Rate_Transform (Tran, Rot_Matrix) : ");
      Put (To_Rate_Transform (A_T_B.Translation, To_Rot_Matrix (A_T_B.Rotation))); New_Line;

      Put_Line ("To_Wrench_Transform (Tran, Rot_Matrix) : ");
      Put (To_Wrench_Transform (A_T_B.Translation, To_Rot_Matrix (A_T_B.Rotation))); New_Line;

      -- To_DC_Array_DCV used in IO package

      Put_Line ("Inverse_Transpose (A_TR_B) : "); Put (Inverse_Transpose (A_TR_B)); New_Line;
      Put_Line ("Inverse_Transpose (A_TW_B) : "); Put (Inverse_Transpose (A_TW_B)); New_Line;

      Put_Line ("A_TR_B * B_TR_C : "); Put (A_TR_B * B_TR_C); New_Line;
      Put_Line ("To_Rate_Transform (A_T_B * B_T_C) : "); Put (To_Rate_Transform (B_T_C * A_T_B)); New_Line;
      Put_Line ("DC_Array_DCV result : ");
      Put (To_DC_Array_DCV (A_TR_B) * To_DC_Array_DCV (B_TR_C), Single_Line_Array => False); New_Line;
      Put_Line ("To_DC_Array_DCV (A_TR_B) * B_TR_C : ");
      Put (To_DC_Array_DCV (A_TR_B) * B_TR_C, Single_Line_Array => False); New_Line;
      Put_Line ("A_TR_B * To_DC_Array_DCV (B_TR_C) : ");
      Put (A_TR_B * To_DC_Array_DCV (B_TR_C), Single_Line_Array => False); New_Line;

      Put_Line ("A_TW_B * B_TW_C : "); Put (A_TW_B * B_TW_C); New_Line;
      Put_Line ("Wrench_Transform (A_T_B * B_T_C) : "); Put (To_Wrench_Transform (B_T_C * A_T_B)); New_Line;
      Put_Line ("DC_Array_DCV result : ");
      Put (To_DC_Array_DCV (A_TW_B) * To_DC_Array_DCV (B_TW_C), Single_Line_Array => False); New_Line;
      Put_Line ("To_DC_Array_DCV (A_TW_B) * B_TW_C : ");
      Put (To_DC_Array_DCV (A_TW_B) * B_TW_C, Single_Line_Array => False); New_Line;
      Put_Line ("A_TW_B * To_DC_Array_DCV (B_TW_C) : ");
      Put (A_TW_B * To_DC_Array_DCV (B_TW_C), Single_Line_Array => False); New_Line;
      New_Line;

      Put_Line ("Testing Transform_Rate, Transform_Wrench");
      --  Just verify the SAL.Math_Float.DOF_3 stuff is called properly.
      Test_Transform (((1.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit))));
      Test_Transform (((1.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, Y_Unit))));
   end Test_Rate_Wrench_Transform;

   Test_Dual_Mag_Axis :
   declare
      A_Dual_Mag_Axis : constant Dual_Mag_Axis_Type := ((4.0, X_Unit), (2.0, Y_Unit));
      A_Dual_Float    : constant Dual_Real_Type    := (5.0, 6.0);
      A_Float         : constant Real_Type         := 5.0;

   begin
      Put_Line (" Testing Dual_Mag_Axis_Type");

      Put ("Dual_Mag_Axis    : "); Put (A_Dual_Mag_Axis); New_Line;
      Put ("Mag              : "); Put (Mag (A_Dual_Mag_Axis)); New_Line;
      Put ("Dual_Cart_Vector : "); Put (To_Dual_Cart_Vector (A_Dual_Mag_Axis)); New_Line;
      Put (" and back        : "); Put (To_Dual_Mag_Axis (To_Dual_Cart_Vector (A_Dual_Mag_Axis))); New_Line;
      Put ("Pose             : "); Put (To_Pose (A_Dual_Mag_Axis)); New_Line;
      Put (" and back        : "); Put (To_Dual_Mag_Axis (To_Pose (A_Dual_Mag_Axis))); New_Line;
      Put ("- Dual_Mag_Axis  : "); Put (-A_Dual_Mag_Axis); New_Line;
      New_Line;

      Put ("Dual_Float                 : "); Put (A_Dual_Float); New_Line;
      Put ("Dual_Float * Dual_Mag_Axis : "); Put (A_Dual_Float * A_Dual_Mag_Axis); New_Line;
      Put ("Dual_Mag_Axis * Dual_Float : "); Put (A_Dual_Mag_Axis * A_Dual_Float); New_Line;
      Put ("Dual_Mag_Axis / Dual_Float : "); Put (A_Dual_Mag_Axis / A_Dual_Float); New_Line;
      New_Line;

      Put ("Float                 : "); Put (A_Float); New_Line;
      Put ("Float * Dual_Mag_Axis : "); Put (A_Float * A_Dual_Mag_Axis); New_Line;
      Put ("Dual_Mag_Axis * Float : "); Put (A_Dual_Mag_Axis * A_Float); New_Line;
      Put ("Dual_Mag_Axis / Float : "); Put (A_Dual_Mag_Axis / A_Float); New_Line;
      New_Line;
   end Test_Dual_Mag_Axis;

   Test_Mass_Type :
   declare
      A_Mass : constant Mass_Type := To_Mass
         (Total          => 2.0,
          Center         => (0.0, 3.0, 4.0),
          Center_Inertia => (1.0, 2.0, 3.0, 4.0, 5.0, 6.0));

      Another_Mass : constant Mass_Type := To_Mass
         (Total          => 2.0,
          Center         => (5.0, 0.0, 0.0),
          Center_Inertia => (2.0, 3.0, 4.0, 0.0, 0.0, 0.0));

      procedure Test_Change_Frame (Current_T_New : in Pose_Type; Current_Mass : in Mass_Type)
      is
         New_Mass : constant Mass_Type := Current_T_New * Current_Mass;
      begin
         Put ("Current_Mass  : "); Put (Current_Mass); New_Line;
         Put ("  Inertia     : "); Put (Inertia (Current_Mass)); New_Line;
         Put ("Current_T_New : "); Put (Current_T_New); New_Line;
         Put ("New Mass      : "); Put (New_Mass); New_Line;
         Put ("  Inertia     : "); Put (Inertia (New_Mass)); New_Line;
         New_Line;
      end Test_Change_Frame;

      procedure Test_Add (Left, Right : in Mass_Type; Left_T_Right : in Pose_Type)
      is
         Sum : constant Mass_Type := Add (Left, Right, Left_T_Right);
      begin
         Put ("Left         : "); Put (Left); New_Line;
         Put ("Right        : "); Put (Right); New_Line;
         Put ("Left_T_Right : "); Put (Left_T_Right); New_Line;
         Put ("Sum          : "); Put (Sum); New_Line;
         Put ("Sum - Right  : "); Put (Subtract (Sum, Right, Left_T_Right)); New_Line;
         New_Line;
      end Test_Add;

      procedure Test_Times (Mass : in Mass_Type; Velocity : in Dual_Cart_Vector_Type)
      is
         Momentum : constant Dual_Cart_Vector_Type := Mass * Velocity;
      begin
         Put ("Mass     : "); Put (Mass); New_Line;
         Put ("Velocity : "); Put (Velocity); New_Line;
         Put ("Momentum : "); Put (Momentum); New_Line;
         Put ("Velocity : "); Put (Inverse_Times (Mass, Momentum)); New_Line;
         New_Line;
      end Test_Times;

   begin
      SAL.Math_Float.DOF_3.Text_IO.Inertia_Text_IO.Default_Exp := 0;

      Put_Line (" Testing MASS_TYPE");
      Put_Line ("A_Mass         :"); Put (A_Mass); New_Line;
      Put      ("Total          : "); Put (Total (A_Mass)); New_Line;
      Put      ("Center         : "); Put (Center (A_Mass)); New_Line;
      Put      ("Center_Inertia : "); Put (Center_Inertia (A_Mass)); New_Line;
      Put      ("Inertia        : "); Put (Inertia (A_Mass)); New_Line;
      New_Line;

      Test_Change_Frame
         (Current_T_New => Zero_Pose,
          Current_Mass  => Another_Mass);

      Test_Change_Frame
         (Current_T_New => ((1.0, 2.0, 0.0), Zero_Unit_Quaternion),
          Current_Mass  => Another_Mass);

      Test_Change_Frame
         (Current_T_New => ((0.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, Z_Unit))),
          Current_Mass  => Another_Mass);

      Test_Change_Frame
         (Current_T_New => ((1.0, 2.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, Z_Unit))),
          Current_Mass  => Another_Mass);

      Test_Add
         (Left         => To_Mass (2.0, (1.0, 0.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
          Right        => To_Mass (1.0, (0.0, 1.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
          Left_T_Right => Zero_Pose);

      Test_Add
         (Left         => To_Mass (2.0, (1.0, 0.0, 0.0), (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
          Right        => To_Mass (1.0, (0.0, 1.0, 0.0), (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
          Left_T_Right => Zero_Pose);

      Test_Add
         (Left         => To_Mass (2.0, (1.0, 0.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
          Right        => To_Mass (1.0, (0.0, 1.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
          Left_T_Right => ((1.0, 2.0, 0.0), Zero_Unit_Quaternion));

      Test_Add
         (Left         => To_Mass (2.0, (1.0, 0.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
          Right        => To_Mass (1.0, (0.0, 1.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
          Left_T_Right => ((0.0, 0.0, 0.0), To_Unit_Quaternion (Mag_Axis_Type'(0.1, X_Unit))));

      Test_Times
         (Mass     => To_Mass (2.0, (1.0, 0.0, 0.0), (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
          Velocity => (1.0, 1.0, 1.0, 0.0, 0.0, 0.0));

      Test_Times
         (Mass     => To_Mass (2.5, (0.0, 0.0, 0.0), (3.0, 4.0, 5.0, 0.0, 0.0, 0.0)),
          Velocity => (1.0, 0.0, 0.0, 0.0, 0.0, 2.0));

      Test_Times
         (Mass     => To_Mass (2.5, (0.0, 1.5, 0.0), (3.0, 4.0, 5.0, 0.0, 0.0, 0.0)),
          Velocity => (1.0, 0.0, 0.0, 0.0, 0.0, 2.0));

   end Test_Mass_Type;

   Test_CM_Mass_Type :
   declare

      procedure Test_Times (Mass : in CM_Mass_Type; Velocity : in Dual_Cart_Vector_Type)
      is
         Momentum : constant Dual_Cart_Vector_Type := Mass * Velocity;
      begin
         Put ("Mass     : "); Put (Mass); New_Line;
         Put ("Velocity : "); Put (Velocity); New_Line;
         Put ("Momentum : "); Put (Momentum); New_Line;
         Put ("Velocity : "); Put (Inverse_Times (Mass, Momentum)); New_Line;
         New_Line;
      end Test_Times;

   begin
      Put_Line (" Testing CM_Mass_Type");

      Test_Times
         (Mass     => (2.0, (1.0, 1.0, 1.0, 0.0, 0.0, 0.0)),
          Velocity => (1.0, 1.0, 1.0, 0.0, 0.0, 0.0));

      Test_Times
         (Mass     => (2.5, (3.0, 4.0, 5.0, 0.0, 0.0, 0.0)),
          Velocity => (1.0, 0.0, 0.0, 0.0, 0.0, 2.0));

   end Test_CM_Mass_Type;

end Test_Math_Float_DOF_6;
