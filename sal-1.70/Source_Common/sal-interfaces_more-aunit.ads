--  Abstract :
--
--  Utilities for AUnit checks of types in Interfaces, Interfaces_More.
--  See SAL.AUnit for types in Standard.
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

with Interfaces.C;
with SAL.AUnit;
package SAL.Interfaces_More.AUnit is
   pragma Elaborate_Body; -- Standard.AUnit.Assertions

   procedure Check is new SAL.AUnit.Gen_Check_Discrete (Interfaces.Unsigned_8);
   procedure Check is new SAL.AUnit.Gen_Check_Discrete (Interfaces.Unsigned_16);
   procedure Check is new SAL.AUnit.Gen_Check_Discrete (Interfaces.Unsigned_32);

   procedure Check_Binary
     (Message   : in String;
      Computed  : in Interfaces.Unsigned_8;
      Expected  : in Interfaces.Unsigned_8);

   procedure Check is new SAL.AUnit.Gen_Check_Discrete (Interfaces.C.int);

end SAL.Interfaces_More.AUnit;
