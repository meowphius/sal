-- Abstract :
--
-- Conversions between several time representations.
--
-- Copyright (C) 2001, 2004 Stephen Leake.  All Rights Reserved.
--
-- SAL is free software; you can redistribute it and/or modify it
-- under terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2, or (at your option) any
-- later version. SAL is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details. You should have received a
-- copy of the GNU General Public License distributed with SAL; see
-- file COPYING. If not, write to the Free Software Foundation, 59
-- Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from
-- SAL, or you link SAL object files with other files to produce
-- an executable, that does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

with Interfaces;
package SAL.Time_Conversions is
   pragma Elaborate_Body; --  Body depends on Ada.Exceptions

   Small : constant := 10.0**(-9);
   type Time_Type is delta Small range -2**63 * Small .. (2**63-1) * Small;
   for Time_Type'Small use Small;
   for Time_Type'Size use 64;
   --  This gives a range of +- 292 years:
   --
   --  10.0**-9 seconds/small * 2**63 smalls = 9.2234e9 seconds
   --  60 seconds/min * 60 min/hr * 24 hrs/day * 365 days/year = 3.153e7 seconds/year

   ----------
   -- Useful constants.

   Seconds_Per_Minute    : constant := 60.0;
   Seconds_Per_Hour      : constant := 3600.0;
   Seconds_Per_Day       : constant := 86_400.0;
   Seconds_Per_Week      : constant := 604_800.0;
   Seconds_Per_Year      : constant := 31_536_000.0;
   Seconds_Per_Leap_Year : constant := 31_622_400.0;
   Days_Per_Week         : constant := 7;
   Days_Per_Year         : constant := 365;
   Days_Per_Leap_Year    : constant := 366;

   UTC_Year_Origin : constant := 1958;

   ----------
   -- Operations

   function Leap_Year (Year : in Integer) return Boolean;
   --  Return TRUE if Year is a leap year.

   procedure To_Year_Day_Seconds
     (UTC_Time       : in     Time_Type;
      Year           :    out Integer;
      Day_In_Year    :    out Integer;
      Seconds_In_Day :    out Time_Type);

   function To_UTC_Time
     (Year           : in Integer;
      Day_In_Year    : in Integer;
      Seconds_In_Day : in Time_Type;
      Absolute       : in Boolean)
     return Time_Type;
   -- If not Absolute, leap days are ignored. Year must be > UTC_Year_Origin.

   function To_UTC_Time
     (Hours             : in Integer;
      Minutes           : in Integer;
      Seconds_In_Minute : in Time_Type)
     return Time_Type;

   procedure To_Hour_Minute_Seconds
     (Seconds           : in     Time_Type;
      Hour              :    out Integer;
      Minute            :    out Integer;
      Seconds_In_Minute :    out Time_Type);

   ----------
   --  Conversions for counter/timers

   function Floor_Unsigned_16 (Item : in Time_Type) return Interfaces.Unsigned_16;
   --  Return smallest integral value not less than Item.
   --
   --  Raises SAL.Range_Error (with no message) if Item is outside
   --  range of Unsigned_16.

   function Floor_Unsigned_32 (Item : in Time_Type) return Interfaces.Unsigned_32;
   --  Return smallest integral value not less than Item.
   --
   --  Raises SAL.Range_Error (with no message) if Item is outside
   --  range of Unsigned_32.

   function To_Time (Microseconds : in Interfaces.Unsigned_16) return Time_Type;
   function To_Microseconds (Time : in Time_Type) return Interfaces.Unsigned_16;

   function Checked_Unsigned_16 (Label : in String; Item : in Time_Type) return Interfaces.Unsigned_16;
   --  If Item is in range of Unsigned_16, return Unsigned_16 (Item).
   --  Else raise Range_Error with Label, value of Item in message.

   ----------
   --  The NASA Goddard ASIST package defines a string time representation:
   --
   --  YY-DDD-HH:MM:SS.LLL
   --  1234567890123456789
   --
   --  For absolute times, if YY < 70, year = 2000 + YY, else year = 1900 + YY.
   --
   --  No fields of the ASIST time string may be omitted.
   --
   --  To allow dates outside the range 1970 .. 2069, we extend the
   --  ASIST representation to permit 4 digit years.
   --
   --  The to_* (Time : in String) functions raise SAL.Invalid_Format
   --  if the string is not a valid format.

   subtype ASIST_Time_String_Type is String (1 .. 19);
   subtype Extended_ASIST_Time_String_Type is String (1 .. 21);

   function To_UTC_Time
      (Time     : in String;
       Absolute : in Boolean)
      return Time_Type;
   --  Time must be ASIST format or extended ASIST format. If
   --  absolute, result is relative to UTC origin.

   function To_ASIST_String (Time : in Time_Type) return ASIST_Time_String_Type;

   function To_Extended_ASIST_String (Time : in Time_Type) return Extended_ASIST_Time_String_Type;

end SAL.Time_Conversions;
