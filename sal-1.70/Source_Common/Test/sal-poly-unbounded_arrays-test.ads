-- Abstract :
--
-- utilities for testing SAL.Poly.Unbounded_Arrays
--
-- Copyright (C) 1999, 2001 Stephen Leake.  All Rights Reserved.
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
--
generic
   with function Image (Item : in Item_Node_Type) return String;
package SAL.Poly.Unbounded_Arrays.Test is
   pragma Elaborate_Body; -- body depends on text_IO

   procedure Put (Item : in Array_Type);
   -- Put all the details to Standard_Output.

end SAL.Poly.Unbounded_Arrays.Test;
