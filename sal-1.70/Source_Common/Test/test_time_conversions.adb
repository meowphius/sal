--  Abstract :
--
--  See spec
--
--  Copyright (C) 2001, 2004 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Interfaces;
with SAL.AUnit;                  use SAL.AUnit;
with SAL.Interfaces_More.AUnit;
with SAL.Time_Conversions.AUnit; use SAL.Time_Conversions.AUnit;
with SAL.Time_Conversions;       use SAL.Time_Conversions;
package body Test_Time_Conversions is

   ----------
   --  Test procedures

   procedure Test_Leap_Year (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Check ("Leap_Year (1)", Leap_Year (1), False);
      Check ("Leap_Year (2)", Leap_Year (2), False);
      Check ("Leap_Year (3)", Leap_Year (3), False);
      Check ("Leap_Year (4)", Leap_Year (4), True);
      Check ("Leap_Year (5)", Leap_Year (5), False);
      Check ("Leap_Year (6)", Leap_Year (6), False);
      Check ("Leap_Year (7)", Leap_Year (7), False);
      Check ("Leap_Year (8)", Leap_Year (8), True);
      Check ("Leap_Year (9)", Leap_Year (9), False);
   end Test_Leap_Year;

   procedure Test_Year_Day_Seconds (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (Message      : in String;
         Year         : in Integer;
         Day          : in Integer;
         Seconds      : in Time_Type;
         Expected_UTC : in Time_Type)
      is
         UTC_Time    : constant Time_Type := To_UTC_Time (Year, Day, Seconds, Absolute => True);
         Out_Year    : Integer;
         Out_Day     : Integer;
         Out_Seconds : Time_Type;
      begin
         Check (Message & " UTC", UTC_Time, Expected_UTC);

         To_Year_Day_Seconds (UTC_Time, Out_Year, Out_Day, Out_Seconds);

         Check (Message & ".year", Out_Year, Year);
         Check (Message & ".day", Out_Day, Day);
         Check (Message & ".Seconds", Out_Seconds, Seconds);

      end Check;
   begin

      Check
        ("1",
         Year         => UTC_Year_Origin + 0,
         Day          => 1,
         Seconds      => 0.0,
         Expected_UTC => 0.00);

      Check
        ("2",
         Year         => UTC_Year_Origin + 0,
         Day          => 20,
         Seconds      => 10.01,
         Expected_UTC => 1641610.01);

      Check
        ("3",
         Year         => UTC_Year_Origin + 5,
         Day          => 120,
         Seconds      => 20.01,
         Expected_UTC => 168048020.01);

   end Test_Year_Day_Seconds;

   procedure Test_Hour_Minute_Seconds (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (Message          : in String;
         Hour             : in Integer;
         Minute           : in Integer;
         Seconds          : in Time_Type;
         Expected_UTC     : in Time_Type;
         Expected_Hour    : in Integer;
         Expected_Minute  : in Integer;
         Expected_Seconds : in Time_Type)
      is
         UTC_Time    : constant Time_Type := To_UTC_Time (Hour, Minute, Seconds);
         Out_Hour    : Integer;
         Out_Minute  : Integer;
         Out_Seconds : Time_Type;
      begin
         Check (Message & " UTC", UTC_Time, Expected_UTC);

         To_Hour_Minute_Seconds (UTC_Time, Out_Hour, Out_Minute, Out_Seconds);

         Check (Message & ".Hour", Out_Hour, Expected_Hour);
         Check (Message & ".Minute", Out_Minute, Expected_Minute);
         Check (Message & ".Seconds", Out_Seconds, Expected_Seconds);

      end Check;
   begin

      Check
        ("1",
         Hour             => 0,
         Minute           => 0,
         Seconds          => 0.0,
         Expected_UTC     => 0.0,
         Expected_Hour    => 0,
         Expected_Minute  => 0,
         Expected_Seconds => 0.0);

      Check
        ("2",
         Hour             => 0,
         Minute           => 20,
         Seconds          => 10.01,
         Expected_UTC     => 1210.01,
         Expected_Hour    => 0,
         Expected_Minute  => 20,
         Expected_Seconds => 10.01);

      Check
        ("3",
         Hour             => 5,
         Minute           => 120,
         Seconds          => 20.01,
         Expected_UTC     => 25220.01,
         Expected_Hour    => 7,
         Expected_Minute  => 0,
         Expected_Seconds => 20.01);
   end Test_Hour_Minute_Seconds;

   procedure Test_String_UTC (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (ASIST        : in String;
         Expected_UTC : in Time_Type)
      is
         UTC_Time : constant Time_Type := To_UTC_Time (ASIST, Absolute => True);
      begin
         Check (ASIST & " UTC", UTC_Time, Expected_UTC);

         Check (ASIST & " ASIST", To_ASIST_String (UTC_Time), ASIST);
      end Check;
   begin

      Check ("70-001-00:00:00.000", 378691200.00);
      Check ("71-001-00:20:10.010", 410228410.01);
      Check ("69-001-05:40:20.010", 3502935620.01);
      Check ("00-021-05:40:20.010", 1327124420.01);
      Check ("10-021-05:40:20.010", 1642743620.01);
   end Test_String_UTC;

   procedure Test_Extended_String_UTC (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check
        (ASIST        : in String;
         Expected_UTC : in Time_Type)
      is
         UTC_Time : constant Time_Type := To_UTC_Time (ASIST, Absolute => True);
      begin
         Check (ASIST & " UTC", UTC_Time, Expected_UTC);

         Check (ASIST & " ASIST", To_Extended_ASIST_String (UTC_Time), ASIST);
      end Check;
   begin

      Check ("1970-001-00:00:00.000", 378691200.00);
      Check ("1971-001-00:20:10.010", 410228410.01);
      Check ("2069-001-05:40:20.010", 3502935620.01);
      Check ("2000-021-05:40:20.010", 1327124420.01);
      Check ("2010-021-05:40:20.010", 1642743620.01);
   end Test_Extended_String_UTC;

   procedure Test_Floor_Unsigned_16 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.Interfaces_More.AUnit, Interfaces, AUnit.Assertions;
   begin
      Check ("1.0", Floor_Unsigned_16 (1.0), 1);
      Check ("1.1", Floor_Unsigned_16 (1.1), 1);
      Check ("0.000_000_001", Floor_Unsigned_16 (0.000_000_001), 0);
      Check ("1.000_000_001", Floor_Unsigned_16 (1.000_000_001), 1);
      Check ("~Unsigned_16'last", Floor_Unsigned_16 (65534.1), 65534);

      declare
         Temp : Unsigned_16;
         --  WORKAROUND: GNAT 3.15 and 5.02 have different semantics
         --  for 'pragma Unreferenced', so use this instead.
         pragma Warnings (Off, Temp);
      begin
         Temp := Floor_Unsigned_16 (65535.01);
         Assert (False, "> Unsigned_16'last did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

      declare
         Temp : Unsigned_16;
         --  WORKAROUND: GNAT 3.15 and 5.02 have different semantics
         --  for 'pragma Unreferenced', so use this instead.
         pragma Warnings (Off, Temp);
      begin
         Temp := Floor_Unsigned_16 (-0.01);
         Assert (False, "< 0 did not get range_error");
      exception
      when SAL.Range_Error =>
         null;
      end;

   end Test_Floor_Unsigned_16;

   ----------
   --  Public subprograms

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Leap_Year'Access, "Test_Leap_Year");
      Register_Routine (T, Test_Year_Day_Seconds'Access, "Test_Year_Day_Seconds");
      Register_Routine (T, Test_Hour_Minute_Seconds'Access, "Test_Hour_Minute_Seconds");
      Register_Routine (T, Test_String_UTC'Access, "Test_String_UTC");
      Register_Routine (T, Test_Extended_String_UTC'Access, "Test_Extended_String_UTC");
      Register_Routine (T, Test_Floor_Unsigned_16'Access, "Test_Floor_Unsigned_16");
   end Register_Tests;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Time_Conversions");
   end Name;

end Test_Time_Conversions;
