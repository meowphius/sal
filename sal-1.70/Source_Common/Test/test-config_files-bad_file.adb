--  Abstract :
--
--  See spec
--
--  Copyright (C) 2003, 2004 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with SAL.Config_Files; use SAL.Config_Files;
package body Test.Config_Files.Bad_File is

   Config    : Configuration_Type;
   File_Name : constant String := "test-config_files-bogus.config";

   ----------
   --  Local subprogram declarations

   procedure Test_Open_Error (T : in out AUnit.Test_Cases.Test_Case'Class);

   ----------
   --  Subprogram bodies (alphabetical order)

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Config_Files.Bad_File");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Open_Error'Access, "Test_Open_Error");
   end Register_Tests;

   procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
      use Ada.Text_IO;

      File : File_Type;
   begin

      begin
         --  Delete the file if it currently exists, to erase previous
         --  tests.
         Open (File, In_File, File_Name);
         Delete (File);
      exception
      when Ada.IO_Exceptions.Name_Error =>
         --  File did not exist.
         null;
      end;

      --  Write a bogus file, so we can test failures.
      Create (File, Out_File, File_Name);

      --  But also write valid comments, so we can test that as well.
      Put_Line (File, "# a comment");
      Put_Line (File, "! another comment");
      Put_Line (File, "[Geometry]");
      Put_Line (File, "Left_Top=( 10,  10)");

      Close (File);
   end Set_Up_Case;

   procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, In_File, File_Name);
      Delete (File);
   end Tear_Down_Case;

   procedure Test_Open_Error (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Raised_Exception : Boolean := False;
      Expected_Message : constant String := "test-config_files-bogus.config:3:0: missing '='";
   begin
      begin
         Open (Config, File_Name, Error_Handling => Raise_Exception);
      exception
      when E : SAL.Config_File_Error =>

         AUnit.Assertions.Assert
           (Ada.Exceptions.Exception_Message (E) = Expected_Message,
            "expecting invalid syntax error, got '" & Ada.Exceptions.Exception_Message (E) & "'");

         Raised_Exception := True;

      when E : others =>
         AUnit.Assertions.Assert
           (False,
            "Open raised " & Ada.Exceptions.Exception_Name (E) &
              ":" &
              Ada.Exceptions.Exception_Message (E));
      end;

      AUnit.Assertions.Assert (Raised_Exception, "Open did not raise exception");
   end Test_Open_Error;

end Test.Config_Files.Bad_File;
