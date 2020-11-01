--  Abstract:
--
--  see spec
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

function SAL.Generic_Fixed_Image
   (Item  : in Number_Type;
    Fore : in Natural;
    Aft  : in Natural)
return String
is
   Temp  : Integer := Integer (abs Item * Number_Type (10 ** Aft));
   Digit : Integer;
   Image : String (1 .. Fore + Aft + 1);
begin
   for I in reverse 1 .. Aft loop
      Digit := Temp mod 10;
      Temp  := Temp / 10;

      Image (Fore + I + 1) := Character'Val (Character'Pos ('0') + Digit);
   end loop;

   Image (Fore + 1) := '.';

   for I in reverse 1 .. Fore loop
      Digit := Temp mod 10;
      Temp  := Temp / 10;

      Image (I) := Character'Val (Character'Pos ('0') + Digit);
   end loop;
   return Image;

end Sal.Generic_Fixed_Image;