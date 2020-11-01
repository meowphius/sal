--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003 Stephen Leake.  All Rights Reserved.
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

with SAL.Config_Files.Duration; use SAL.Config_Files.Duration;
with SAL.Config_Files.Integer;  use SAL.Config_Files.Integer;
package body SAL.Config_Files.Time is

   use Ada.Calendar;

   function Read
     (Config         : in Configuration_Type;
      Key            : in String;
      Default        : in Ada.Calendar.Time;
      Error_Handling : in Error_Handling_Type := Ignore)
      return Ada.Calendar.Time
   is
      Default_Year    : Year_Number;
      Default_Month   : Month_Number;
      Default_Day     : Day_Number;
      Default_Seconds : Day_Duration;

      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Default, Default_Year, Default_Month, Default_Day, Default_Seconds);

      Year    := Read (Config, Key & ".Year", Default_Year, Error_Handling);
      Month   := Read (Config, Key & ".Month", Default_Month, Error_Handling);
      Day     := Read (Config, Key & ".Day", Default_Day, Error_Handling);
      Seconds := Read (Config, Key & ".Seconds", Default_Seconds, Error_Handling);

      return Time_Of (Year, Month, Day, Seconds);
   end Read;

   procedure Write
     (Config         : in out Configuration_Type;
      Key            : in     String;
      Value          : in     Ada.Calendar.Time;
      Error_Handling : in     Error_Handling_Type := Ignore)
   is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Value, Year, Month, Day, Seconds);

      Write (Config, Key & ".Year", Year, Error_Handling);
      Write (Config, Key & ".Month", Month, Error_Handling);
      Write (Config, Key & ".Day", Day, Error_Handling);
      Write (Config, Key & ".Seconds", Seconds, Error_Handling);
   end Write;

end SAL.Config_Files.Time;

