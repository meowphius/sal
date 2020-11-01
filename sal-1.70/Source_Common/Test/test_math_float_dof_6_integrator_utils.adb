--  Abstract :
--
--  Test SAL.Gen_Math.Gen_DOF_6.Integrator_Utils, by using them in an
--  integrator. Also demonstrates instantiating Runge_Kutta for a 6
--  DOF system.
--
--  Copyright (C) 2002, 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
with SAL.Gen_Math.Gen_Runge_Kutta_4th;
with SAL.Math_Float.DOF_3;
with SAL.Math_Float.DOF_6.Integrator_Utils; use SAL.Math_Float.DOF_6.Integrator_Utils;
with SAL.Math_Float.DOF_6.Text_IO; use SAL.Math_Float.DOF_6.Text_IO;
-- with SAL.Math_Float.Stats.Image;
-- with SAL.Math_Float.Text_IO;
procedure Test_Math_Float_DOF_6_Integrator_Utils
is
   use SAL.Math_Float.DOF_3;
   use SAL.Math_Float.DOF_6;

   -- common function between tests
   function Zero_Indep_Wrench return Dual_Cart_Vector_Type
   is begin
      return (others => 0.0);
   end Zero_Indep_Wrench;

   function Zero_Dep_Wrench (State : in State_Type) return Dual_Cart_Vector_Type
   is
      pragma Unreferenced (State);
   begin
      return (others => 0.0);
   end Zero_Dep_Wrench;

   function Constant_Wrench (State : in State_Type) return Dual_Cart_Vector_Type
   is
      pragma Unreferenced (State);
   begin
      return (Tx => 0.1, Rz => -0.1, others => 0.0);
   end Constant_Wrench;

   function Zero_Momentum return Cart_Vector_Type
   is
   begin
      return (others => 0.0);
   end Zero_Momentum;

   generic
      with function State_Dep_Wrench (State : in State_Type) return Dual_Cart_Vector_Type;
      with function State_Indep_Wrench return Dual_Cart_Vector_Type;
      with function State_Indep_Momentum return Cart_Vector_Type;
      --  Return wrench on object due to actuators, gravity, etc.
      --  Euler torque will be added internally.
   procedure Gen_State_Integrator
     (State        : in out State_Type;
      Inverse_Mass : in     CM_Inverse_Mass_Type;
      Time_Step    : in     Float);

   procedure Gen_State_Integrator
     (State        : in out State_Type;
      Inverse_Mass : in     CM_Inverse_Mass_Type;
      Time_Step    : in     Float)
   is
      Cached_State_Indep_Wrench : constant Dual_Cart_Vector_Type := State_Indep_Wrench;

      function Compute_Derivative (State : in State_Type) return State_Dot_Type
      is
         use DCV_Ops;
      begin
         return Derivative
           (State, Inverse_Mass, Cached_State_Indep_Wrench + State_Dep_Wrench (State), State_Indep_Momentum);
      end Compute_Derivative;

      procedure Runge_Kutta is new SAL.Math_Float.Gen_Runge_Kutta_4th
        (State_Type         => State_Type,
         Derivative_Type    => State_Dot_Type,
         Compute_Derivative => Compute_Derivative,
         "+"                => State_Plus_State,
         "*"                => Derivative_Times_Time);
   begin
      Runge_Kutta (State, Time_Step);
   end Gen_State_Integrator;

begin
   Put_Line ("Test with zero wrench; constant output.");
   declare

      procedure Zero_Force_Integrator is new Gen_State_Integrator
        (State_Dep_Wrench     => Zero_Dep_Wrench,
         State_Indep_Wrench   => Zero_Indep_Wrench,
         State_Indep_Momentum => Zero_Momentum);

      State : State_Type :=
        (Pose     => Zero_Pose,
         Momentum => (Tx | Ty | Tz => 0.0, Rx | Ry => 0.0, Rz => 0.2));

      Mass : constant CM_Mass_Type :=
           (Total          => 5.0,
            Center_Inertia => ((1.0, 1.0, 2.0, 0.0, 0.0, 0.0)));

      Inverse_Mass : constant CM_Inverse_Mass_Type := Inverse (Mass);
   begin
      Dual_Cart_Vector_Text_IO.Default_Fore := 2;
      Dual_Cart_Vector_Text_IO.Default_Aft  := 2;
      Dual_Cart_Vector_Text_IO.Default_Exp  := 0;

      Put_Line ("    Pose                                   |            Momentum");
      Put_Line ("  tx     ty     tz     rx     ry     rz        tx     ty     tz     rx     ry     rz");
      for I in 1 .. 10 loop
         Put (To_Dual_Cart_Vector (State.Pose));
         Put (" | ");
         Put (State.Momentum);
         New_Line;
         Zero_Force_Integrator (State, Inverse_Mass, 0.1);
      end loop;
   end;

   ----------
   New_Line;
   Put_Line ("Test with constant wrench; parabolic output.");
   declare
      procedure Constant_Force_Integrator is new Gen_State_Integrator
        (State_Dep_Wrench     => Constant_Wrench,
         State_Indep_Wrench   => Zero_Indep_Wrench,
         State_Indep_Momentum => Zero_Momentum);

      State : State_Type :=
        (Pose     => Zero_Pose,
         Momentum => (Tx | Ty | Tz => 0.0, Rx | Ry => 0.0, Rz => 0.1));

      Mass : constant CM_Mass_Type :=
           (Total          => 5.0,
            Center_Inertia => ((1.0, 1.0, 2.0, 0.0, 0.0, 0.0)));

      Inverse_Mass : constant CM_Inverse_Mass_Type := Inverse (Mass);
   begin
      Dual_Cart_Vector_Text_IO.Default_Fore := 2;
      Dual_Cart_Vector_Text_IO.Default_Aft  := 3;
      Dual_Cart_Vector_Text_IO.Default_Exp  := 0;

      Put_Line ("    Pose                                         |            Momentum");
      Put_Line ("  tx      ty      tz      rx      ry      rz         tx      ty      tz      rx      ry      rz");
      for I in 1 .. 10 loop
         Put (To_Dual_Cart_Vector (State.Pose));
         Put (" | ");
         Put (State.Momentum);
         New_Line;
         Constant_Force_Integrator (State, Inverse_Mass, 0.1);
      end loop;
   end;

   ----------
   New_Line;
   Put_Line ("Test with child rotation momentum, constant force");
   declare
      function Child_Momentum return Cart_Vector_Type
      is begin
         return (X => 0.5, others => 0.0);
      end Child_Momentum;

      procedure Integrator is new Gen_State_Integrator
        (State_Dep_Wrench     => Constant_Wrench,
         State_Indep_Wrench   => Zero_Indep_Wrench,
         State_Indep_Momentum => Child_Momentum);

      State : State_Type :=
        (Pose     => Zero_Pose,
         Momentum => (Tx | Ty | Tz => 0.0, Rx | Ry => 0.0, Rz => 0.1)); --  In parent frame

      Mass : constant CM_Mass_Type :=
           (Total          => 5.0,
            Center_Inertia => ((1.0, 1.0, 2.0, 0.0, 0.0, 0.0)));

      Inverse_Mass : constant CM_Inverse_Mass_Type := Inverse (Mass);

   begin
      Dual_Cart_Vector_Text_IO.Default_Fore := 2;
      Dual_Cart_Vector_Text_IO.Default_Aft  := 3;
      Dual_Cart_Vector_Text_IO.Default_Exp  := 0;

      Put_Line ("    Pose                                         |            Momentum");
      Put_Line ("  tx      ty      tz      rx      ry      rz         tx      ty      tz      rx      ry      rz");
      for I in 1 .. 10 loop
         Put (To_Dual_Cart_Vector (State.Pose));
         Put (" | ");
         Put (State.Momentum);
         New_Line;
         Integrator (State, Inverse_Mass, 0.1);
      end loop;
   end;

end Test_Math_Float_DOF_6_Integrator_Utils;
