------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           A U N I T . T E S T S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--                Copyright (C) 2000, 2002, 2004 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Finalization;
use Ada.Finalization;

with AUnit.Test_Results;
use AUnit.Test_Results;

--  Base Test or Test Suite.
package AUnit.Tests is
   -- pragma Elaborate_Body; --  AUnit.Test_Results is, but we have no body

   type Test is abstract new Controlled with private;

   type Test_Access is access all Test'Class;

   --  Run a test case or suite
   procedure Run (T : in out Test; R : in out Result) is abstract;

private
   type Test is abstract new Controlled with null record;
end AUnit.Tests;
