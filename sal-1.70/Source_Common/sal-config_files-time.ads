--  Abstract :
--
--  Support for Ada.Calendar.Time type.
--
--  Copyright (C) 2003, 2004 Stephen Leake.  All Rights Reserved.
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

with Ada.Calendar;
with SAL.Time_Conversions;
package SAL.Config_Files.Time is
   pragma Elaborate_Body; --  SAL.Config_Files is.

   function Read
     (Config         : in Configuration_Type;
      Key            : in String;
      Default        : in Ada.Calendar.Time;
      Error_Handling : in Error_Handling_Type := Ignore)
     return Ada.Calendar.Time;
   --  Read a time from Config.

   procedure Write
     (Config         : in out Configuration_Type;
      Key            : in     String;
      Value          : in     Ada.Calendar.Time;
      Error_Handling : in     Error_Handling_Type := Ignore);

   function Read is new Read_Fixed (SAL.Time_Conversions.Time_Type);

   procedure Write is new Write_Fixed (SAL.Time_Conversions.Time_Type);

end SAL.Config_Files.Time;
