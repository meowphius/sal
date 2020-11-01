-- Abstract :
--
-- see spec
--
-- Copyright (C) 1999, 2001, 2003 Stephen Leake.  All Rights Reserved.
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
with Ada.Text_IO; use Ada.Text_IO;
package body SAL.Poly.Unbounded_Arrays.Test is

   procedure Put (Item : in Array_Type)
   is begin
      Put_Line ("Growth => " & Growth_Type'Image (Item.Growth) &
                ", Initial_Space => " & Index_Type'Image (Item.Initial_Space) &
                ", Max_Space => " & Index_Type'Image (Item.Max_Space));
      Put_Line ("First, Last           => " & Index_Type'Image (Item.First) & ", " & Index_Type'Image (Item.Last));
      Put_Line ("Base'First, Base'Last => " & Index_Type'Image (Item.Base'First) & ", " &
                Index_Type'Image (Item.Base'Last));
      Put_Line ("Length => " & Index_Type'Image (Length (Item)));
      Put ("Items => ");
      for I in Item.First .. Item.Last loop
         Put (Image (Item.Base (I)) & " ");
      end loop;
      New_Line (2);
   end Put;

end SAL.Poly.Unbounded_Arrays.Test;
