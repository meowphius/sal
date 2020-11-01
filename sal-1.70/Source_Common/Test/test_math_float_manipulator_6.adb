--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003, 2004 Stephen Leake.  All Rights Reserved.
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
--

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with SAL.Math_Float.Scalar; use SAL.Math_Float.Scalar;
with SAL.Math_Float.DOF_3;
with SAL.Math_Float.DOF_6;
with SAL.Math_Float_Kraft_HC_Nominal;
with SAL.Math_Float_Manipulator_6;
package body Test_Math_Float_Manipulator_6 is

   use SAL.Math_Float;
   use SAL.Math_Float.DOF_3;
   use SAL.Math_Float_Manipulator_6.Math;
   use SAL.Math_Float_Manipulator_6.Math.Joint_Array_Real_Ops;

   Threshold : constant := 10.0e-5;

   Geometry         : constant Joint_Array_Den_Hart_Type      := SAL.Math_Float_Kraft_HC_Nominal.Geometry;
   Nominal_Position : constant Joint_Array_Real_Type          := (0.0, -Pi/2.0, Pi / 2.0, 0.0, -Pi/2.0, 0.0);
   Tlast_T_Tp       : constant SAL.Math_Float.DOF_6.Pose_Type := ((0.0, 0.0,  0.1), Zero_Unit_Quaternion);
   Tp_T_Obj         : constant SAL.Math_Float.DOF_6.Pose_Type := ((0.0, 0.0, -0.1), Zero_Unit_Quaternion);

   procedure Check (Message : in String; Computed, Expected : in Jacobian_Type)
   is begin
      for I in Computed'Range loop
         for J in Computed (I)'Range loop
            AUnit.Assertions.Assert (abs (Computed (I)(J) - Expected(I)(J)) < Threshold, Message & " failed");
         end loop;
      end loop;
   end Check;

   procedure Check (Message : in String; Computed, Expected : in Joint_Array_Pose_Type)
   is begin
      for I in Computed'Range loop
         for J in Cart_Axis_Type loop
            AUnit.Assertions.Assert
              (abs (Computed (I).Translation (J) - Expected (I).Translation (J)) < Threshold,
               Message & " failed");
         end loop;

         AUnit.Assertions.Assert
           (abs Mag (Inverse_Times (Computed (I).Rotation, Expected(I).Rotation)) < Threshold,
            Message & " failed");
      end loop;
   end Check;

   procedure Jacobian (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      --  Inverse is just a call to an instantiation of
      --  Generic_Inverse_Array_Math, so we don't need to test it
      --  here.

      procedure Test_Jacobian
        (Message    : in String;
         Joints     : in Joint_Array_Real_Type;
         Tlast_T_TP : in SAL.Math_Float.DOF_6.Pose_Type := SAL.Math_Float.DOF_6.Zero_Pose;
         TP_T_Obj   : in SAL.Math_Float.DOF_6.Pose_Type := SAL.Math_Float.DOF_6.Zero_Pose;
         Expected   : in Jacobian_Type)
      is
         Ti_T_Obj : Joint_Array_Pose_Type;
         T0_T_Obj : SAL.Math_Float.DOF_6.Pose_Type;
      begin
         Slow_Ti_T_Obj (Joints, Geometry, Tlast_T_TP, TP_T_Obj, Ti_T_Obj, T0_T_Obj);
         Check (Message, Slow_Jacobian (Ti_T_Obj), Expected);
      end Test_Jacobian;

   begin
      Test_Jacobian
        (Message  => "Nominal",
         Joints   => Nominal_Position,
         Expected =>
           (( -0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
            ( -0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
            ( -0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
            (  1.00000,  -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
            ( -0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
            (  0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message    => "Nominal + Tlast_T_Tp",
         Joints     => Nominal_Position,
         Tlast_T_Tp => Tlast_T_Tp,
         Expected   =>
           (( -0.00000,  -0.30500,  -0.30500,   0.00000,  -0.10000,   0.00000),
            ( -0.30500,   0.00000,   0.00000,   0.10000,   0.00000,   0.00000),
            ( -0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
            (  1.00000,  -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
            ( -0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
            (  0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message    => "Nominal + Tlast_T_Tp + Tp_T_Obj",
         Joints     => Nominal_Position,
         Tlast_T_Tp => Tlast_T_Tp,
         Tp_T_Obj   => Tp_T_Obj,
         Expected   =>
           (( -0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
            ( -0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
            ( -0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
            (  1.00000,  -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
            ( -0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
            (  0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J6",
         Joints   => Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.0, 0.1),
         Expected =>
           (( -0.02047,  -0.20398,  -0.20398,   0.00000,   0.00000,   0.00000),
            ( -0.20398,   0.02047,   0.02047,   0.00000,   0.00000,   0.00000),
            ( -0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
            (  0.99500,  -0.09983,  -0.09983,  -0.99500,  -0.09983,   0.00000),
            ( -0.09983,  -0.99500,  -0.99500,   0.09983,  -0.99500,   0.00000),
            (  0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J5",
         Joints   => Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.1, 0.0),
         Expected =>
           (( -0.01348,  -0.19449,  -0.21246,   0.00050,   0.00000,   0.00000),
            ( -0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
            ( -0.13433,   0.11499,  -0.06411,   0.00498,   0.00000,   0.00000),
            (  0.99500,  -0.00000,   0.00000,  -0.99500,  -0.00000,   0.00000),
            ( -0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
            ( -0.09983,   0.00000,   0.00000,   0.09983,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J4",
         Joints   => Nominal_Position + (0.0, 0.0, 0.0, 0.1, 0.0, 0.0),
         Expected =>
            (( -0.00000,  -0.20550,  -0.20550,   0.00000,   0.00000,   0.00000),
             ( -0.19100,  -0.00948,   0.00849,   0.00000,   0.00000,   0.00000),
             ( -0.15482,   0.09453,  -0.08458,   0.00500,   0.00000,   0.00000),
             (  1.00000,  -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
             (  0.00000,  -0.99500,  -0.99500,   0.00000,  -1.00000,   0.00000),
             (  0.00000,  -0.09983,  -0.09983,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J3",
         Joints   => Nominal_Position + (0.0, 0.0, 0.1, 0.0, 0.0, 0.0),
         Expected =>
            (( -0.01348,  -0.18703,  -0.20500,   0.00000,   0.00000,   0.00000),
             ( -0.19549,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
             ( -0.13433,   0.09410,  -0.08500,   0.00500,   0.00000,   0.00000),
             (  0.99500,  -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
             ( -0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
             ( -0.09983,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J2",
         Joints   => Nominal_Position + (0.0, 0.1, 0.0, 0.0, 0.0, 0.0),
         Expected =>
            (( -0.01348,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
             ( -0.21346,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
             ( -0.13433,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
             (  0.99500,  -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
             ( -0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
             ( -0.09983,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Jacobian
        (Message  => "Nominal + 0.1 J1",
         Joints   => Nominal_Position + (0.1, 0.0, 0.0, 0.0, 0.0, 0.0),
         Expected =>
            (( -0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
             ( -0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
             ( -0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
             (  1.00000,  -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
             ( -0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
             (  0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

   end Jacobian;

   procedure Jacobian_Change_Frame (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Ti_T_Obj : Joint_Array_Pose_Type;
      T0_T_Obj : SAL.Math_Float.DOF_6.Pose_Type;
      Jacobian : Jacobian_Type;

      procedure Test_Change_Frame
        (Message       : in String;
         Jacobian      : in Jacobian_Type;
         Current_T_New : in SAL.Math_Float.DOF_6.Pose_Type;
         Expected      : in Jacobian_Type)
      is
         New_Jacob_1 : constant Jacobian_Type := Transform_Jacobian (Current_T_New, Jacobian);
         New_Jacob_2 : constant Jacobian_Type := SAL.Math_Float.DOF_6.To_Rate_Transform (Current_T_New) * Jacobian;
      begin
         Check (Message & "Transform_Jacobian", New_Jacob_1, Expected);
         Check (Message & "To_Rate_Transform", New_Jacob_2, Expected);
      end Test_Change_Frame;

   begin
      Slow_Ti_T_Obj (Nominal_Position, Geometry, Tlast_T_Tp, Tp_T_Obj, Ti_T_Obj, T0_T_Obj);
      Jacobian := Slow_Jacobian (Ti_T_Obj);

      Test_Change_Frame
        ("Zero",   Jacobian, (Zero_Cart_Vector, Zero_Unit_Quaternion),
         (( -0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
          ( -0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
          ( -0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
          (  1.00000,  -0.00000,   0.00000,  -1.00000,  -0.00000,   0.00000),
          ( -0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
          (  0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Change_Frame
        ("0.1 TX", Jacobian, ((0.1, 0.0, 0.0), Zero_Unit_Quaternion),
        (( -0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
         ( -0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.10000),
         ( -0.13500,   0.19500,   0.01500,   0.00500,   0.10000,   0.00000),
         (  1.00000,   0.00000,   0.00000,  -1.00000,   0.00000,   0.00000),
         (  0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
         (  0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Change_Frame
        ("0.1 TY", Jacobian, ((0.0, 0.1, 0.0), Zero_Unit_Quaternion),
        (( -0.00000,  -0.20500,  -0.20500,  -0.00000,  -0.00000,  -0.10000),
         ( -0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
         ( -0.03500,   0.09500,  -0.08500,  -0.09500,   0.00000,   0.00000),
         (  1.00000,   0.00000,   0.00000,  -1.00000,   0.00000,   0.00000),
         (  0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
         (  0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Change_Frame
        ("0.1 TZ", Jacobian, ((0.0, 0.0, 0.1), Zero_Unit_Quaternion),
        (( -0.00000,  -0.30500,  -0.30500,   0.00000,  -0.10000,   0.00000),
         ( -0.30500,   0.00000,   0.00000,   0.10000,   0.00000,   0.00000),
         ( -0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
         (  1.00000,   0.00000,   0.00000,  -1.00000,   0.00000,   0.00000),
         (  0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
         (  0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

      Test_Change_Frame
        ("0.1 RX", Jacobian, (Zero_Cart_Vector, To_Unit_Quaternion (0.1, X)),
        (( -0.00000,  -0.20500,  -0.20500,   0.00000,   0.00000,   0.00000),
         ( -0.21745,   0.00948,  -0.00849,   0.00050,   0.00000,   0.00000),
         ( -0.11386,   0.09453,  -0.08458,   0.00498,   0.00000,   0.00000),
         (  1.00000,   0.00000,   0.00000,  -1.00000,   0.00000,   0.00000),
         (  0.00000,  -0.99500,  -0.99500,   0.00000,  -0.99500,   0.09983),
         (  0.00000,   0.09983,   0.09983,   0.00000,   0.09983,   0.99500)));

      Test_Change_Frame
        ("0.1 RY", Jacobian, (Zero_Cart_Vector, To_Unit_Quaternion (0.1, Y)),
        ((  0.01348,  -0.21346,  -0.19549,  -0.00050,   0.00000,   0.00000),
         ( -0.20500,   0.00000,   0.00000,   0.00000,   0.00000,   0.00000),
         ( -0.13433,   0.07406,  -0.10504,   0.00498,   0.00000,   0.00000),
         (  0.99500,  -0.00000,  -0.00000,  -0.99500,  -0.00000,  -0.09983),
         (  0.00000,  -1.00000,  -1.00000,   0.00000,  -1.00000,   0.00000),
         (  0.09983,   0.00000,   0.00000,  -0.09983,   0.00000,   0.99500)));

      Test_Change_Frame
        ("0.1 RZ", Jacobian, (Zero_Cart_Vector, To_Unit_Quaternion (0.1, Z)),
        (( -0.02047,  -0.20398,  -0.20398,   0.00000,   0.00000,   0.00000),
         ( -0.20398,   0.02047,   0.02047,  -0.00000,   0.00000,   0.00000),
         ( -0.13500,   0.09500,  -0.08500,   0.00500,   0.00000,   0.00000),
         (  0.99500,  -0.09983,  -0.09983,  -0.99500,  -0.09983,   0.00000),
         ( -0.09983,  -0.99500,  -0.99500,   0.09983,  -0.99500,   0.00000),
         (  0.00000,   0.00000,   0.00000,   0.00000,   0.00000,   1.00000)));

   end Jacobian_Change_Frame;

   procedure T0_T_Ti_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("Nominal", Slow_T0_T_Ti (Nominal_Position, Geometry),
         ( 1 =>
             (Translation => (  0.00000,   0.00000,  -0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,  -0.00000,   0.00000),
                     (  0.00000,  -1.00000,   0.00000),
                     ( -0.00000,   0.00000,  -1.00000))))),
           2 =>
             (Translation => (  0.00000,  -0.07500,   0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   1.00000,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     ( -1.00000,   0.00000,   0.00000))))),
           3 =>
             (Translation => (  0.00000,  -0.13000,  -0.18000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,   0.00000,  -0.00000),
                     ( -0.00000,   0.00000,  -1.00000),
                     (  0.00000,   1.00000,   0.00000))))),
           4 =>
             (Translation => (  0.20500,  -0.13000,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,   0.00000,   0.00000),
                     (  0.00000,   1.00000,   0.00000),
                     (  0.00000,   0.00000,   1.00000))))),
           5 =>
             (Translation => (  0.20500,  -0.13500,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   1.00000,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     ( -1.00000,   0.00000,   0.00000))))),
           6 =>
             (Translation => (  0.20500,  -0.13500,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   0.00000,   1.00000),
                     ( -0.00000,   1.00000,   0.00000),
                     ( -1.00000,  -0.00000,   0.00000)))))));

   end T0_T_Ti_1;

   procedure T0_T_Ti_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("Nominal + 0.1 J1", Slow_T0_T_Ti (Nominal_Position + (0.1, 0.0, 0.0, 0.0, 0.0, 0.0), Geometry),
         ( 1 =>
             (Translation => (  0.00000,   0.00000,  -0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.99500,  -0.09983,   0.00000),
                     ( -0.09983,  -0.99500,   0.00000),
                     ( -0.00000,   0.00000,  -1.00000))))),
           2 =>
             (Translation => ( -0.00749,  -0.07463,   0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   0.99500,  -0.09983),
                     (  0.00000,  -0.09983,  -0.99500),
                     ( -1.00000,   0.00000,   0.00000))))),
           3 =>
             (Translation => ( -0.01298,  -0.12935,  -0.18000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.99500,   0.00000,  -0.09983),
                     ( -0.09983,   0.00000,  -0.99500),
                     (  0.00000,   1.00000,   0.00000))))),
           4 =>
             (Translation => (  0.19100,  -0.14982,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.99500,   0.09983,   0.00000),
                     ( -0.09983,   0.99500,   0.00000),
                     (  0.00000,   0.00000,   1.00000))))),
           5 =>
             (Translation => (  0.19050,  -0.15479,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   0.99500,  -0.09983),
                     (  0.00000,  -0.09983,  -0.99500),
                     ( -1.00000,   0.00000,   0.00000))))),
           6 =>
             (Translation => (  0.19050,  -0.15479,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   0.09983,   0.99500),
                     (  0.00000,   0.99500,  -0.09983),
                     ( -1.00000,   0.00000,   0.00000)))))));

   end T0_T_Ti_2;

   procedure T0_T_Ti_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("Nominal + 0.1 J2", Slow_T0_T_Ti (Nominal_Position + (0.0, 0.1, 0.0, 0.0, 0.0, 0.0), Geometry),
         ( 1 =>
             (Translation => (  0.00000,   0.00000,  -0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,  -0.00000,   0.00000),
                     (  0.00000,  -1.00000,   0.00000),
                     ( -0.00000,   0.00000,  -1.00000))))),
           2 =>
             (Translation => (  0.00000,  -0.07500,   0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.09983,   0.99500,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     ( -0.99500,   0.09983,   0.00000))))),
           3 =>
             (Translation => (  0.01797,  -0.13000,  -0.17910),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.99500,  -0.09983,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     (  0.09983,   0.99500,   0.00000))))),
           4 =>
             (Translation => (  0.21346,  -0.13000,  -0.07406),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.99500,   0.00000,  -0.09983),
                     (  0.00000,   1.00000,   0.00000),
                     (  0.09983,   0.00000,   0.99500))))),
           5 =>
             (Translation => (  0.21346,  -0.13500,  -0.07406),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.09983,   0.99500,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     ( -0.99500,   0.09983,   0.00000))))),
           6 =>
             (Translation => (  0.21346,  -0.13500,  -0.07406),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.09983,   0.00000,   0.99500),
                     ( -0.00000,   1.00000,   0.00000),
                     ( -0.99500,  -0.00000,   0.09983)))))));

   end T0_T_Ti_3;

   procedure T0_T_Ti_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("Nominal + 0.1 J3", Slow_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.1, 0.0, 0.0, 0.0), Geometry),
         ( 1 =>
             (Translation => (  0.00000,   0.00000,  -0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,  -0.00000,   0.00000),
                     (  0.00000,  -1.00000,   0.00000),
                     ( -0.00000,   0.00000,  -1.00000))))),
           2 =>
             (Translation => (  0.00000,  -0.07500,   0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   1.00000,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     ( -1.00000,   0.00000,   0.00000))))),
           3 =>
             (Translation => (  0.00000,  -0.13000,  -0.18000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.99500,  -0.09983,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     (  0.09983,   0.99500,   0.00000))))),
           4 =>
             (Translation => (  0.19549,  -0.13000,  -0.07496),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.99500,   0.00000,  -0.09983),
                     (  0.00000,   1.00000,   0.00000),
                     (  0.09983,   0.00000,   0.99500))))),
           5 =>
             (Translation => (  0.19549,  -0.13500,  -0.07496),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.09983,   0.99500,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     ( -0.99500,   0.09983,   0.00000))))),
           6 =>
             (Translation => (  0.19549,  -0.13500,  -0.07496),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.09983,   0.00000,   0.99500),
                     ( -0.00000,   1.00000,   0.00000),
                     ( -0.99500,  -0.00000,   0.09983)))))));

   end T0_T_Ti_4;

   procedure T0_T_Ti_5 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("Nominal + 0.1 J4", Slow_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.0, 0.1, 0.0, 0.0), Geometry),
         ( 1 =>
             (Translation => (  0.00000,   0.00000,  -0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,  -0.00000,   0.00000),
                     (  0.00000,  -1.00000,   0.00000),
                     ( -0.00000,   0.00000,  -1.00000))))),
           2 =>
             (Translation => (  0.00000,  -0.07500,   0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   1.00000,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     ( -1.00000,   0.00000,   0.00000))))),
           3 =>
             (Translation => (  0.00000,  -0.13000,  -0.18000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,   0.00000,  -0.00000),
                     ( -0.00000,   0.00000,  -1.00000),
                     (  0.00000,   1.00000,   0.00000))))),
           4 =>
             (Translation => (  0.20500,  -0.13000,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.99500,  -0.09983,  -0.00000),
                     (  0.09983,   0.99500,   0.00000),
                     (  0.00000,  -0.00000,   1.00000))))),
           5 =>
             (Translation => (  0.20550,  -0.13498,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   0.99500,   0.09983),
                     (  0.00000,   0.09983,  -0.99500),
                     ( -1.00000,   0.00000,   0.00000))))),
           6 =>
             (Translation => (  0.20550,  -0.13498,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,  -0.09983,   0.99500),
                     (  0.00000,   0.99500,   0.09983),
                     ( -1.00000,   0.00000,   0.00000)))))));

   end T0_T_Ti_5;

   procedure T0_T_Ti_6 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("Nominal + 0.1 J5", Slow_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.1, 0.0), Geometry),
         ( 1 =>
             (Translation => (  0.00000,   0.00000,  -0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,  -0.00000,   0.00000),
                     (  0.00000,  -1.00000,   0.00000),
                     ( -0.00000,   0.00000,  -1.00000))))),
           2 =>
             (Translation => (  0.00000,  -0.07500,   0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   1.00000,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     ( -1.00000,   0.00000,   0.00000))))),
           3 =>
             (Translation => (  0.00000,  -0.13000,  -0.18000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,   0.00000,  -0.00000),
                     ( -0.00000,   0.00000,  -1.00000),
                     (  0.00000,   1.00000,   0.00000))))),
           4 =>
             (Translation => (  0.20500,  -0.13000,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,   0.00000,   0.00000),
                     (  0.00000,   1.00000,   0.00000),
                     (  0.00000,   0.00000,   1.00000))))),
           5 =>
             (Translation => (  0.20500,  -0.13500,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.09983,   0.99500,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     ( -0.99500,   0.09983,   0.00000))))),
           6 =>
             (Translation => (  0.20500,  -0.13500,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.09983,   0.00000,   0.99500),
                     ( -0.00000,   1.00000,   0.00000),
                     ( -0.99500,  -0.00000,   0.09983)))))));

   end T0_T_Ti_6;

   procedure T0_T_Ti_7 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      --  One check per routine, to allow GNAT to do stack checking.
      pragma Unreferenced (T);
   begin
      Check
        ("Nominal + 0.1 J6", Slow_T0_T_Ti (Nominal_Position + (0.0, 0.0, 0.0, 0.0, 0.0, 0.1), Geometry),
         ( 1 =>
             (Translation => (  0.00000,   0.00000,  -0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,  -0.00000,   0.00000),
                     (  0.00000,  -1.00000,   0.00000),
                     ( -0.00000,   0.00000,  -1.00000))))),
           2 =>
             (Translation => (  0.00000,  -0.07500,   0.00000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   1.00000,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     ( -1.00000,   0.00000,   0.00000))))),
           3 =>
             (Translation => (  0.00000,  -0.13000,  -0.18000),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,   0.00000,  -0.00000),
                     ( -0.00000,   0.00000,  -1.00000),
                     (  0.00000,   1.00000,   0.00000))))),
           4 =>
             (Translation => (  0.20500,  -0.13000,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  1.00000,   0.00000,   0.00000),
                     (  0.00000,   1.00000,   0.00000),
                     (  0.00000,   0.00000,   1.00000))))),
           5 =>
             (Translation => (  0.20500,  -0.13500,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   1.00000,   0.00000),
                     (  0.00000,   0.00000,  -1.00000),
                     ( -1.00000,   0.00000,   0.00000))))),
           6 =>
             (Translation => (  0.20500,  -0.13500,  -0.09500),
              Rotation => To_Unit_Quaternion
                (Unchecked_Rot_Matrix
                   (((  0.00000,   0.00000,   1.00000),
                     (  0.09983,   0.99500,   0.00000),
                     ( -0.99500,   0.09983,   0.00000)))))));
   end T0_T_Ti_7;

   ----------
   --  Public routines

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test SAL.Math_Float.Manipulator_6");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Jacobian'Access, "Jacobian");
      Register_Routine (T, Jacobian_Change_Frame'Access, "Jacobian_Change_Frame");
      Register_Routine (T, T0_T_Ti_1'Access, "T0_T_Ti_1");
      Register_Routine (T, T0_T_Ti_2'Access, "T0_T_Ti_2");
      Register_Routine (T, T0_T_Ti_3'Access, "T0_T_Ti_3");
      Register_Routine (T, T0_T_Ti_4'Access, "T0_T_Ti_4");
      Register_Routine (T, T0_T_Ti_5'Access, "T0_T_Ti_5");
      Register_Routine (T, T0_T_Ti_6'Access, "T0_T_Ti_6");
      Register_Routine (T, T0_T_Ti_7'Access, "T0_T_Ti_7");
   end Register_Tests;

end Test_Math_Float_Manipulator_6;
