--  Abstract :
--
--  Test SAL.Gen_FIFO
--
--  Copyright (C) 2002 Stephen Leake.  All Rights Reserved.
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
--

with Ada.Text_IO; use Ada.Text_IO;
with SAL.Gen_FIFO.Gen_Text_IO;
procedure Test_Gen_FIFO
is
   package Integer_FIFO is new SAL.Gen_FIFO (Element_Type => Integer, FIFO_Size => 10);
   package Integer_FIFO_Text_IO is new Integer_FIFO.Gen_Text_IO (Integer'Image);
   use Integer_FIFO;
   use Integer_FIFO_Text_IO;

   FIFO : FIFO_Type;
   Item : Integer;
begin
   Fill (FIFO, 100);

   Put_Line ("just declared");
   Put (FIFO);
   Put_Line ("Is_Empty => " & Boolean'Image (Is_Empty (FIFO)));
   Put_Line ("Is_Full  => " & Boolean'Image (Is_Full (FIFO)));
   New_Line;

   Put_Line ("Add 1");
   Put (FIFO, 1);
   Put (FIFO);
   Put_Line ("Is_Empty => " & Boolean'Image (Is_Empty (FIFO)));
   Put_Line ("Is_Full  => " & Boolean'Image (Is_Full (FIFO)));
   New_Line;

   Put_Line ("Fill");
   for I in 2 .. 10 loop
      Put (FIFO, I);
   end loop;
   Put (FIFO);
   New_Line;

   Put_Line ("Get some");
   Get (FIFO, Item);
   Put_Line ("1 => " & Integer'Image (Item));
   Get (FIFO, Item);
   Put_Line ("2 => " & Integer'Image (Item));
   Get (FIFO, Item);
   Put_Line ("3 => " & Integer'Image (Item));
   Put (FIFO);
   New_Line;

   Put_Line ("Fill");
   for I in 21 .. 23 loop
      Put (FIFO, I);
   end loop;
   Put (FIFO);
   New_Line;

end Test_Gen_FIFO;
