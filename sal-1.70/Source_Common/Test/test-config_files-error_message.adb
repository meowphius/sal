--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
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

with Ada.Exceptions; use Ada.Exceptions;
with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with GNAT.OS_Lib;
with SAL.Config_Files.Boolean; use SAL.Config_Files.Boolean;
with SAL.Config_Files.Duration; use SAL.Config_Files.Duration;
with SAL.Config_Files.Integer; use SAL.Config_Files.Integer;
with SAL.Config_Files; use SAL.Config_Files;
package body Test.Config_Files.Error_Message is

   type Modular_Type is mod 8;
   function Read is new Read_Modular (Modular_Type);

   function Read is new Read_Float (Float);

   Config    : Configuration_Type;
   File_Name : constant String := "test-config_files-error_message.config";

   ----------
   --  Local subprograms

   procedure Invalid_Enumeral (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Data : Boolean;
      --  WORKAROUND: GNAT 3.15p and GNAT 5.02a have different
      --  semantics for 'pragma Unreferenced'. So we suppress the
      --  unreferenced warning in the project file.
      --  pragma Unreferenced (Data);
   begin
      Data := Read (Config, "Enumeral");
      Assert (False, "Enumeral: no exception");
   exception
   when E : Constraint_Error =>
      Assert
        (Exception_Message (E) = File_Name & ":1:10: invalid enumeral",
         "enumeral: got " & Exception_Message (E));
   end Invalid_Enumeral;

   procedure Invalid_Integer (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Data : Integer;
      --  WORKAROUND: see above
      --  pragma Unreferenced (Data);
   begin
      Data := Read (Config, "Integer");
      Assert (False, "Integer: no exception");
   exception
   when E : Constraint_Error =>
      Assert
        (Exception_Message (E) = File_Name & ":2:9: invalid integer syntax or range",
         "integer: got " & Exception_Message (E));

   end Invalid_Integer;

   procedure Invalid_Modular (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Data : Modular_Type;
      --  WORKAROUND: see above
      --  pragma Unreferenced (Data);
   begin
      Data := Read (Config, "Modular");
      Assert (False, "Modular: no exception");
   exception
   when E : Constraint_Error =>
      Assert
        (Exception_Message (E) = File_Name & ":3:9: invalid modular integer syntax or range",
         "Modular: got " & Exception_Message (E));

   end Invalid_Modular;

   procedure Invalid_Float (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Data : Float;
      --  WORKAROUND: see above
      --  pragma Unreferenced (Data);
   begin
      Data := Read (Config, "Float");
      Assert (False, "Float: no exception");
   exception
   when E : Constraint_Error =>
      Assert
        (Exception_Message (E) = File_Name & ":4:7: invalid floating point syntax or range",
         "Float: got " & Exception_Message (E));

   end Invalid_Float;

   procedure Invalid_Fixed (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Assertions;

      Data : Duration;
      --  WORKAROUND: see above
      --  pragma Unreferenced (Data);
   begin
      Data := Read (Config, "Fixed");
      Assert (False, "Fixed: no exception");
   exception
   when E : Constraint_Error =>
      Assert
        (Exception_Message (E) = File_Name & ":5:7: invalid fixed point syntax or range",
         "Float: got " & Exception_Message (E));

   end Invalid_Fixed;

   ----------
   --  Public subprograms

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Invalid_Enumeral'Access, "Invalid_Enumeral");
      Register_Routine (T, Invalid_Integer'Access, "Invalid_Integer");
      Register_Routine (T, Invalid_Modular'Access, "Invalid_Modular");
      Register_Routine (T, Invalid_Float'Access, "Invalid_Float");
      Register_Routine (T, Invalid_Fixed'Access, "Invalid_Fixed");
   end Register_Tests;

   procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);

      --  Delete File_Name if it exists, to erase previous tests.

      use GNAT.OS_Lib;
      File    : String_Access := Locate_Regular_File (File_Name, ".");
      Success : Boolean;
   begin
      if File /= null then
         Delete_File (File.all, Success);
         Free (File);
      end if;

      Open (Config, File_Name, Error_Handling => Ignore, Read_Only => False);
      Write (Config, "Enumeral", "in"); -- bad spelling for Enumeral_Type
      Write (Config, "Integer", "one"); -- bad syntax for integer
      Write (Config, "Modular", -1);    -- bad syntax for modular
      Write (Config, "Float", "float"); -- bad syntax for float
      Write (Config, "Fixed", "fixed"); -- bad syntax for fixed

      --  Close and reopen, so line, column are set properly.
      Close (Config);
      Open (Config, File_Name, Error_Handling => Raise_Exception, Read_Only => False);
   end Set_Up_Case;

   procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      --  Don't delete file here, in case we want to look at it.
      Close (Config);
   end Tear_Down_Case;

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Config_Files.Error_Message");
   end Name;

end Test.Config_Files.Error_Message;
