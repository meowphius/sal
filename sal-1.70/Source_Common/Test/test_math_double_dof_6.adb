--  Abstract :
--
--  Test SAL.Math_Double.DOF_6. Generic is tested in
--  test_math_float_dof_6; just ensure that Math_Double.DOF_6 and
--  Math_Double.DOF_6.Text_IO are instantiated properly.
--
--  Copyright (C) 2001, 2003 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;                   use Ada.Text_IO;
with SAL.Math_Double.DOF_6.Text_IO; use SAL.Math_Double.DOF_6.Text_IO;
procedure Test_Math_Double_DOF_6
is
   use SAL.Math_Double.DOF_6;
   A_Dual_Cart_Vector : constant Dual_Cart_Vector_Type := (1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
begin
   SAL.Math_Double.DOF_6.Text_IO.Dual_Cart_Vector_Text_IO.Default_Fore := 3;
   SAL.Math_Double.DOF_6.Text_IO.Dual_Cart_Vector_Text_IO.Default_Aft  := 5;
   SAL.Math_Double.DOF_6.Text_IO.Dual_Cart_Vector_Text_IO.Default_Exp  := 0;

   Put ("DCV => "); Put (A_Dual_Cart_Vector); New_Line;

   Put_Line ("Math_Double.DOF_6 is instantiated properly.");

end Test_Math_Double_DOF_6;
