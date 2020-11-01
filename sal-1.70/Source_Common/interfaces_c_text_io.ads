-- Abstract :
--
-- Instantiation
--
-- Copyright (C) 2002, 2004 Stephen Leake.  All Rights Reserved.
--
-- This library is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2, or (at
-- your option) any later version. This library is distributed in the
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even
-- the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE. See the GNU General Public License for more details. You
-- should have received a copy of the GNU General Public License
-- distributed with this program; see file COPYING. If not, write to
-- the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
-- MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this  unit  does not  by itself cause  the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file  might be covered by the  GNU Public License.
--
with Ada.Text_IO;
with Interfaces.C;
package Interfaces_C_Text_IO is

   package Unsigned_Char_Text_IO is new Ada.Text_IO.Modular_IO (Interfaces.C.unsigned_char);

   procedure Put
      (File  : in Ada.Text_IO.File_Type;
       Item  : in Interfaces.C.unsigned_char;
       Width : in Ada.Text_IO.Field          := Unsigned_Char_Text_IO.Default_Width;
       Base  : in Ada.Text_IO.Number_Base    := Unsigned_Char_Text_IO.Default_Base)
      renames Unsigned_Char_Text_IO.Put;
   procedure Put
      (Item  : in Interfaces.C.unsigned_char;
       Width : in Ada.Text_IO.Field          := Unsigned_Char_Text_IO.Default_Width;
       Base  : in Ada.Text_IO.Number_Base    := Unsigned_Char_Text_IO.Default_Base)
      renames Unsigned_Char_Text_IO.Put;

   procedure Get
      (File  : in     Ada.Text_IO.File_Type;
       Item  :    out Interfaces.C.unsigned_char;
       Width : in     Ada.Text_IO.Field          := 0)
      renames Unsigned_Char_Text_IO.Get;
   procedure Get
      (Item  :    out Interfaces.C.unsigned_char;
       Width : in     Ada.Text_IO.Field          := 0)
      renames Unsigned_Char_Text_IO.Get;

   package Unsigned_Long_Text_IO is new Ada.Text_IO.Modular_IO (Interfaces.C.unsigned_long);

   procedure Put
      (File  : in Ada.Text_IO.File_Type;
       Item  : in Interfaces.C.unsigned_long;
       Width : in Ada.Text_IO.Field          := Unsigned_Long_Text_IO.Default_Width;
       Base  : in Ada.Text_IO.Number_Base    := Unsigned_Long_Text_IO.Default_Base)
      renames Unsigned_Long_Text_IO.Put;
   procedure Put
      (Item  : in Interfaces.C.unsigned_long;
       Width : in Ada.Text_IO.Field          := Unsigned_Long_Text_IO.Default_Width;
       Base  : in Ada.Text_IO.Number_Base    := Unsigned_Long_Text_IO.Default_Base)
      renames Unsigned_Long_Text_IO.Put;

   procedure Get
      (File  : in     Ada.Text_IO.File_Type;
       Item  :    out Interfaces.C.unsigned_long;
       Width : in     Ada.Text_IO.Field          := 0)
      renames Unsigned_Long_Text_IO.Get;
   procedure Get
      (Item  :    out Interfaces.C.unsigned_long;
       Width : in     Ada.Text_IO.Field          := 0)
      renames Unsigned_Long_Text_IO.Get;

   package Long_Text_IO is new Ada.Text_IO.Integer_IO (Interfaces.C.long);

   procedure Put
      (File  : in Ada.Text_IO.File_Type;
       Item  : in Interfaces.C.long;
       Width : in Ada.Text_IO.Field          := Long_Text_IO.Default_Width;
       Base  : in Ada.Text_IO.Number_Base    := Long_Text_IO.Default_Base)
      renames Long_Text_IO.Put;
   procedure Put
      (Item  : in Interfaces.C.long;
       Width : in Ada.Text_IO.Field          := Long_Text_IO.Default_Width;
       Base  : in Ada.Text_IO.Number_Base    := Long_Text_IO.Default_Base)
      renames Long_Text_IO.Put;

   procedure Get
      (File  : in     Ada.Text_IO.File_Type;
       Item  :    out Interfaces.C.long;
       Width : in     Ada.Text_IO.Field          := 0)
      renames Long_Text_IO.Get;
   procedure Get
      (Item  :    out Interfaces.C.long;
       Width : in     Ada.Text_IO.Field          := 0)
      renames Long_Text_IO.Get;

   package Unsigned_Short_Text_IO is new Ada.Text_IO.Modular_IO (Interfaces.C.unsigned_short);

   procedure Put
      (File  : in Ada.Text_IO.File_Type;
       Item  : in Interfaces.C.unsigned_short;
       Width : in Ada.Text_IO.Field          := Unsigned_Short_Text_IO.Default_Width;
       Base  : in Ada.Text_IO.Number_Base    := Unsigned_Short_Text_IO.Default_Base)
      renames Unsigned_Short_Text_IO.Put;
   procedure Put
      (Item  : in Interfaces.C.unsigned_short;
       Width : in Ada.Text_IO.Field          := Unsigned_Short_Text_IO.Default_Width;
       Base  : in Ada.Text_IO.Number_Base    := Unsigned_Short_Text_IO.Default_Base)
      renames Unsigned_Short_Text_IO.Put;

   procedure Get
      (File  : in     Ada.Text_IO.File_Type;
       Item  :    out Interfaces.C.unsigned_short;
       Width : in     Ada.Text_IO.Field          := 0)
      renames Unsigned_Short_Text_IO.Get;
   procedure Get
      (Item  :    out Interfaces.C.unsigned_short;
       Width : in     Ada.Text_IO.Field          := 0)
      renames Unsigned_Short_Text_IO.Get;

end Interfaces_C_Text_IO;
