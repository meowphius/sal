--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 Stephen Leake.  All Rights Reserved.
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
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Ada.Text_IO; use Ada.Text_IO;
package body SAL.Gen_FIFO.Gen_Text_IO is

   procedure Fill (FIFO : in out FIFO_Type; Item : in Element_Type)
   is begin
      FIFO.Data := (others => Item);
   end Fill;

   procedure Put (Item : in FIFO_Type) is
   begin
      Put_Line ("Next_Get => " & Index_Type'Image (Item.Next_Get));
      Put_Line ("Next_Put => " & Index_Type'Image (Item.Next_Put));
      Put_Line ("Empty    => " & Boolean'Image (Item.Empty));
      for I in Item.Data'Range loop
         Put_Line (Index_Type'Image (I) & " => " & Image (Item.Data (I)));
      end loop;
   end Put;

end SAL.Gen_FIFO.Gen_Text_IO;
