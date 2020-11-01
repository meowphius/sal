--  Abstract :
--
--  Examples of various rotation equations, using SAL and Wertz conventions.
--
--  Copyright (C) 2003, 2004 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Math_Double.DOF_3; use SAL.Math_Double.DOF_3;
with SAL.Math_Double.DOF_3.Text_IO; use SAL.Math_Double.DOF_3.Text_IO;
procedure Examples
is
   --  All rotations are passive.

   Earth_R_Sc     : constant Mag_Axis_Type    := (1.5, X_Unit);
   --  Current attitude of the spacecraft.

   Sc_R_Tracker   : constant Mag_Axis_Type    := (2.0, Y_Unit);
   --  Alignment of the star tracker on the spacecraft, in the spacecraft frame.

   Tracker_N_Star : constant Unit_Vector_Type := To_Unit_Vector (0.01, 0.02, 0.9);
   --  Unit vector pointing from the tracker to a star, in the tracker frame.

begin
   Put ("Earth_R_Sc      => "); Put (Earth_R_Sc); New_Line;
   Put ("Sc_R_Tracker    => "); Put (Sc_R_Tracker); New_Line;
   Put ("Tracker_N_Star  => "); Put (Tracker_N_Star); New_Line;
   New_Line;

   SAL_Convention_Quat :
   declare
      Earth_Q_Sc      : constant Unit_Quaternion_Type := To_Unit_Quaternion (Earth_R_Sc);
      Sc_Q_Tracker    : constant Unit_Quaternion_Type := To_Unit_Quaternion (Sc_R_Tracker);
      Earth_Q_Tracker : constant Unit_Quaternion_Type := Earth_Q_Sc * Sc_Q_Tracker;
      Earth_N_Star    : constant Unit_Vector_Type     := Earth_Q_Tracker * Tracker_N_Star;
   begin
      Put_Line ("SAL Convention quaternions");
      Put ("Earth_Q_Sc      => "); Put (Earth_Q_Sc); New_Line;
      Put ("Sc_Q_Tracker    => "); Put (Sc_Q_Tracker); New_Line;
      Put ("Earth_Q_Tracker => "); Put (Earth_Q_Tracker); New_Line;
      Put ("Earth_N_Star    => "); Put (Earth_N_Star); New_Line;
   end SAL_Convention_Quat;

   New_Line;
   Wertz_Convention_Quat :
   declare
      Earth_Q_Sc      : constant Unit_Quaternion_Type := To_Unit_Quaternion (-Earth_R_Sc);
      Sc_Q_Tracker    : constant Unit_Quaternion_Type := To_Unit_Quaternion (-Sc_R_Tracker);
      Earth_Q_Tracker : constant Unit_Quaternion_Type := Sc_Q_Tracker * Earth_Q_Sc;
      Earth_N_Star    : constant Unit_Vector_Type     := Tracker_N_Star * Earth_Q_Tracker;
   begin
      Put_Line ("Wertz convention quaternions");
      Put ("Earth_Q_Sc      => "); Put (Earth_Q_Sc); New_Line;
      Put ("Sc_Q_Tracker    => "); Put (Sc_Q_Tracker); New_Line;
      Put ("Earth_Q_Tracker => "); Put (Earth_Q_Tracker); New_Line;
      Put ("Earth_N_Star    => "); Put (Earth_N_Star); New_Line;
   end Wertz_Convention_Quat;

   New_Line;
   SAL_Convention_Mat :
   declare
      Earth_M_Sc      : constant Rot_Matrix_Type      := To_Rot_Matrix (Earth_R_Sc);
      Earth_Q_Sc      : constant Unit_Quaternion_Type := To_Unit_Quaternion (Earth_M_Sc);
      Sc_M_Tracker    : constant Rot_Matrix_Type      := To_Rot_Matrix (Sc_R_Tracker);
      Sc_Q_Tracker    : constant Unit_Quaternion_Type := To_Unit_Quaternion (Sc_R_Tracker);
      Earth_M_Tracker : constant Rot_Matrix_Type      := Earth_M_Sc * Sc_M_Tracker;
      Earth_Q_Tracker : constant Unit_Quaternion_Type := To_Unit_Quaternion (Earth_M_Tracker);
      Earth_N_Star    : constant Unit_Vector_Type     := Earth_M_Tracker * Tracker_N_Star;
   begin
      Put_Line ("SAL Convention matrices");
      Put_Line ("Earth_M_Sc      => ");
      Put (Earth_M_Sc); New_Line;
      Put ("Earth_Q_Sc      => "); Put (Earth_Q_Sc); New_Line;
      Put_Line ("Sc_M_Tracker    => ");
      Put (Sc_M_Tracker); New_Line;
      Put ("Sc_Q_Tracker    => "); Put (Sc_Q_Tracker); New_Line;
      Put_Line ("Earth_M_Tracker => ");
      Put (Earth_M_Tracker); New_Line;
      Put ("Earth_Q_Tracker => "); Put (Earth_Q_Tracker); New_Line;
      Put ("Earth_N_Star    => "); Put (Earth_N_Star); New_Line;
   end SAL_Convention_Mat;

   New_Line;
   Wertz_Convention_Mat :
   declare
      Earth_M_Sc      : constant Rot_Matrix_Type      := To_Rot_Matrix (Earth_R_Sc);
      Earth_Q_Sc      : constant Unit_Quaternion_Type := Rot_Matrix_To_Unit_Quaternion_Left_Right (Earth_M_Sc);
      Sc_Q_Tracker    : constant Unit_Quaternion_Type := To_Unit_Quaternion (-Sc_R_Tracker);
      Sc_M_Tracker    : constant Rot_Matrix_Type      := Unit_Quaternion_To_Rot_Matrix_Left_Right (Sc_Q_Tracker);
      Earth_M_Tracker : constant Rot_Matrix_Type      := Earth_M_Sc * Sc_M_Tracker;
      Earth_Q_Tracker : constant Unit_Quaternion_Type := Rot_Matrix_To_Unit_Quaternion_Left_Right (Earth_M_Tracker);
      Earth_N_Star    : constant Unit_Vector_Type     := Earth_M_Tracker * Tracker_N_Star;
   begin
      Put_Line ("Wertz Convention matrices");
      Put_Line ("Earth_M_Sc      => "); Put (Earth_M_Sc); New_Line;
      Put ("Earth_Q_Sc      => "); Put (Earth_Q_Sc); New_Line;
      Put_Line ("Sc_M_Tracker    => "); Put (Sc_M_Tracker); New_Line;
      Put ("Sc_Q_Tracker    => "); Put (Sc_Q_Tracker); New_Line;
      Put_Line ("Earth_M_Tracker => "); Put (Earth_M_Tracker); New_Line;
      Put ("Earth_Q_Tracker => "); Put (Earth_Q_Tracker); New_Line;
      Put ("Earth_N_Star    => "); Put (Earth_N_Star); New_Line;
   end Wertz_Convention_Mat;

end Examples;
