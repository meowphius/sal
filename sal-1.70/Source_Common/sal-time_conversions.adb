--  Abstract :
--
--  See spec.
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

with Ada.Exceptions;
with SAL.Generic_Decimal_Image;
with SAL.Generic_Fixed_Image;
package body SAL.Time_Conversions is

   function Image is new SAL.Generic_Decimal_Image (Integer);
   function Image is new SAL.Generic_Fixed_Image (Time_Type);

   function Leap_Year (Year : in Integer) return Boolean
   is begin
      if Year mod 100 = 0 then
         return Year mod 400 = 0;
      else
         return Year mod 4 = 0;
      end if;
   end Leap_Year;

   procedure To_Year_Day_Seconds
      (UTC_Time       : in     Time_Type;
       Year           :    out Integer;
       Day_In_Year    :    out Integer;
       Seconds_In_Day :    out Time_Type)
   is
      Temp_Year : Integer := UTC_Year_Origin;
      Temp_Day : Integer := 1;
      Temp_Seconds : Time_Type := UTC_Time;
   begin
      Find_Year :
      loop
         if Leap_Year (Temp_Year) then
            exit Find_Year when Temp_Seconds < Seconds_Per_Leap_Year;

            Temp_Seconds := Temp_Seconds - Seconds_Per_Leap_Year;
         else
            exit Find_Year when Temp_Seconds < Seconds_Per_Year;

            Temp_Seconds := Temp_Seconds - Seconds_Per_Year;
         end if;

         Temp_Year := Temp_Year + 1;
      end loop Find_Year;

      Year := Temp_Year;

      Find_Day :
      loop
         exit Find_Day when Temp_Seconds < Seconds_Per_Day;

         Temp_Seconds := Temp_Seconds - Seconds_Per_Day;

         Temp_Day := Temp_Day + 1;
      end loop Find_Day;

      Day_In_Year := Temp_Day;

      Seconds_In_Day := Temp_Seconds;

   end To_Year_Day_Seconds;

   function To_UTC_Time
      (Year           : in Integer;
       Day_In_Year    : in Integer;
       Seconds_In_Day : in Time_Type;
       Absolute       : in Boolean)
       return Time_Type
   is
      Result : Time_Type;
   begin
      if Year < UTC_Year_Origin then
         raise Range_Error;
      end if;

      if Absolute then
         -- compute days since UTC origin
         Result := Time_Type (Days_Per_Year * (Year - 1958) + ((Year - 1957) / 4) + (Day_In_Year - 1));
      else
         -- ignore leap days, since we don't know the year origin
         Result := Time_Type (Days_Per_Year * Year + Day_In_Year - 1);
      end if;

      return Result * Seconds_Per_Day + Seconds_In_Day;
   end To_UTC_Time;

   function To_UTC_Time
      (Hours             : in Integer;
       Minutes           : in Integer;
       Seconds_In_Minute : in Time_Type)
       return Time_Type
   is begin
      return Seconds_In_Minute + 60.0 * (Time_Type (Minutes) + 60.0 * Time_Type (Hours));
   end To_UTC_Time;

   procedure To_Hour_Minute_Seconds
      (Seconds           : in     Time_Type;
       Hour              :    out Integer;
       Minute            :    out Integer;
       Seconds_In_Minute :    out Time_Type)
   is
      Temp_Seconds : Time_Type := Seconds;
   begin
      Hour := Integer (Temp_Seconds) / 3600;
      Temp_Seconds := Temp_Seconds - Time_Type (3600 * Hour);
      Minute := Integer (Temp_Seconds) / 60;
      Seconds_In_Minute := Temp_Seconds - Time_Type (60 * Minute);
   end To_Hour_Minute_Seconds;

   ----------
   --  Conversions for counter/timers

   function Floor_Unsigned_16 (Item : in Time_Type) return Interfaces.Unsigned_16
   is
      use Interfaces;
      Temp : Unsigned_16;
   begin
      if not (Item in 0.0 .. Time_Type (Unsigned_16'Last)) then
         raise Range_Error;
      end if;

      Temp := Unsigned_16 (Item);

      if Time_Type (Temp) > Item then
         return Temp - 1;
      else
         return Temp;
      end if;
   end Floor_Unsigned_16;

   function Floor_Unsigned_32 (Item : in Time_Type) return Interfaces.Unsigned_32
   is
      use Interfaces;
      Temp : Unsigned_32;
   begin
      if not (Item in 0.0 .. Time_Type (Unsigned_32'Last)) then
         raise Range_Error;
      end if;

      Temp := Unsigned_32 (Item);

      if Time_Type (Temp) > Item then
         return Temp - 1;
      else
         return Temp;
      end if;
   end Floor_Unsigned_32;

   function To_Time (Microseconds : in Interfaces.Unsigned_16) return SAL.Time_Conversions.Time_Type
   is begin
      return Time_Type (Microseconds) / 1_000_000;
   end To_Time;

   function To_Microseconds (Time : in Time_Type) return Interfaces.Unsigned_16
   is
      use Interfaces;
      Temp : constant Integer_64 := Integer_64 (Time * 1_000_000);
   begin
      if Temp in Integer_64 (Unsigned_16'First) .. Integer_64 (Unsigned_16'Last) then
         return Unsigned_16 (Temp);
      else
         Ada.Exceptions.Raise_Exception
           (Range_Error'Identity,
            Time_Type'Image (Time) & " outside range of Unsigned_16 microseconds");
      end if;
   end To_Microseconds;

   function Checked_Unsigned_16 (Label : in String; Item : in Time_Type) return Interfaces.Unsigned_16
   is begin
      if not (Item in 0.0 .. Time_Type (Interfaces.Unsigned_16'Last)) then
         Ada.Exceptions.Raise_Exception
           (Range_Error'Identity,
            Label & Time_Type 'Image (Item) & " not in range of Unsigned_16");
      else
         return Interfaces.Unsigned_16 (Item);
      end if;
   end Checked_Unsigned_16;

   ----------
   -- ASIST time strings

   function To_UTC_Time_1
     (Time      : in String;
      Absolute  : in Boolean;
      Extended  : in Boolean;
      Day_First : in Integer)
     return Time_Type
   is
      Year    : Integer            := Integer'Value (Time (Time'First .. Day_First - 2));
      Day     : constant Integer   := Integer'Value (Time (Day_First ..  Day_First + 2));
      Hour    : constant Integer   := Integer'Value (Time (Day_First + 4 ..  Day_First + 5));
      Minute  : constant Integer   := Integer'Value (Time (Day_First + 7 .. Day_First + 8));
      Seconds : constant Time_Type := Time_Type'Value (Time (Day_First + 10 .. Day_First + 15));
   begin
      if Absolute and not Extended then
         if Year < 70 then
            Year := 2000 + Year;
         else
            Year := 1900 + Year;
         end if;
      end if;

      return To_UTC_Time (Year, Day, To_UTC_Time (Hour, Minute, Seconds), Absolute);

   end To_UTC_Time_1;

   function To_UTC_Time
      (Time     : in String;
       Absolute : in Boolean)
      return Time_Type
   is begin
      if Time (Time'First + 2) = '-' then
         return To_UTC_Time_1 (Time, Absolute, Extended => False, Day_First => Time'First + 3);
      elsif Time (Time'First + 4) = '-' then
         return To_UTC_Time_1 (Time, Absolute, Extended => True, Day_First => Time'First + 5);
      else
         raise Invalid_Format;
      end if;
   exception
   when Constraint_Error =>
      raise Invalid_Format;
   end To_UTC_Time;

   function To_ASIST_String (Time : in Time_Type) return ASIST_Time_String_Type
   is
      Year    : Integer;
      Day     : Integer;
      Hour    : Integer;
      Minute  : Integer;
      Seconds : Time_Type;
      Result  : ASIST_Time_String_Type;
   begin
      To_Year_Day_Seconds (Time, Year, Day, Seconds);

      Year := Year mod 100;

      To_Hour_Minute_Seconds (Seconds, Hour, Minute, Seconds);

      Result (1 ..  2)  := Image (Item => Year, Width => 2);
      Result (3)        := '-';
      Result (4 ..  6)  := Image (Item => Day, Width => 3);
      Result (7)        := '-';
      Result (8 ..  9)  := Image (Item => Hour, Width => 2);
      Result (10)       := ':';
      Result (11 .. 12) := Image (Item => Minute, Width => 2);
      Result (13)       := ':';
      Result (14 .. 19) := Image (Item => Seconds, Fore => 2, Aft => 3);
      return Result;
   end To_ASIST_String;

   function To_Extended_ASIST_String (Time : in Time_Type) return Extended_ASIST_Time_String_Type
   is
      Year    : Integer;
      Day     : Integer;
      Hour    : Integer;
      Minute  : Integer;
      Seconds : Time_Type;
      Result  : Extended_ASIST_Time_String_Type;
   begin
      To_Year_Day_Seconds (Time, Year, Day, Seconds);

      To_Hour_Minute_Seconds (Seconds, Hour, Minute, Seconds);

      Result (1 ..  4)  := Image (Item => Year, Width => 4);
      Result (5)        := '-';
      Result (6 ..  8)  := Image (Item => Day, Width => 3);
      Result (9)        := '-';
      Result (10 .. 11) := Image (Item => Hour, Width => 2);
      Result (12)       := ':';
      Result (13 .. 14) := Image (Item => Minute, Width => 2);
      Result (15)       := ':';
      Result (16 .. 21) := Image (Item => Seconds, Fore => 2, Aft => 3);
      return Result;
   end To_Extended_ASIST_String;

end SAL.Time_Conversions;
