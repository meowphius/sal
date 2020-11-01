--  Abstract :
--
--  Run all AUnit tests for SAL. See Build/*/Makefile for non-AUnit
--  tests.
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

with AUnit.Test_Results.Text_Reporter;
with AUnit.Test_Suites;
with SAL.File_Names.Test;
with Test.Config_Files.All_Suite;
with Test_Gen_Queues_Bounded_Nonlimited;
with Test_Gen_Sets;
with Test_Math_Float_DOF_3;
with Test_Math_Float_Den_Hart;
with Test_Math_Float_Gen_Runge_Kutta_4th;
with Test_Math_Float_Manipulator_6;
with Test_Math_Float_Manipulator_7;
with Test_Math_Float_Polynomials;
with Test_Math_Float_Stats;
with Test_Network_Order;
with Test_Time_Conversions;
procedure Test_All_Harness
is
   use AUnit.Test_Suites;

   Suite  : constant Access_Test_Suite := new Test_Suite;
   -- Either allocate or put at library level.

   Result : AUnit.Test_Results.Result;

begin
   Add_Test (Suite, Test.Config_Files.All_Suite);

   Add_Test (Suite, new SAL.File_Names.Test.Test_Case);
   Add_Test (Suite, new Test_Gen_Queues_Bounded_Nonlimited.Test_Case);
   Add_Test (Suite, new Test_Gen_Sets.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Den_Hart.Test_Case);
   Add_Test (Suite, new Test_Math_Float_DOF_3.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Gen_Runge_Kutta_4th.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Manipulator_6.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Manipulator_7.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Polynomials.Test_Case);
   Add_Test (Suite, new Test_Math_Float_Stats.Test_Case);
   Add_Test (Suite, new Test_Network_Order.Test_Case);
   Add_Test (Suite, new Test_Time_Conversions.Test_Case);

   Run (Suite.all, Result);
   AUnit.Test_Results.Text_Reporter.Report (Result);

end Test_All_Harness;
