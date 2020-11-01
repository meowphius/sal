--  Abstract :
--
--  Run one test, while working on it.
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
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Test_Network_Order;
procedure Test_One_Harness
is
   Suite  : constant Access_Test_Suite := new Test_Suite;
   -- Either allocate or put at library level.

   Result : AUnit.Test_Results.Result;

begin
   Add_Test (Suite, new Test_Network_Order.Test_Case);

   Run (Suite.all, Result);

   AUnit.Test_Results.Text_Reporter.Report (Result);

end Test_One_Harness;
