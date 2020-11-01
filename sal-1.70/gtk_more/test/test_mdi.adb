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

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Exceptions;
with Glib;
with Gdk.Test_Events; use Gdk.Test_Events;
with Gtk.Gen_Background_Window;
with Gtk.Window;
with MDI_Main_Window;
package body Test_MDI is

   Create_Param : MDI_Main_Window.Create_Parameters_Type;

   procedure Create (Window : out Gtk.Window.Gtk_Window)
   is begin
      MDI_Main_Window.Gtk_New (MDI_Main_Window.Gtk_Window (Window), Create_Param);
   end Create;

   package Background is new Gtk.Gen_Background_Window (Create);

   use Background;

   procedure Test_Resize_Opaque (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Glib;
      Main_Window : MDI_Main_Window.Gtk_Window;

      Resize_Start : constant Point_Type := (270,  105); --  Bottom edge of child window
   begin
      Create_Param := (Opaque_Resize => True, Opaque_Move => False);

      Background_Task.Create_Window (Gtk.Window.Gtk_Window (Main_Window));

      Background_Task.Run;

      if Debug_Level < 4 then
         delay Test_Delay_Time;

         --  Mouse selections
         Move_To (Resize_Start);

         Mouse_Button (1, Button_Up => False);

         for I in Gint'(1) .. 10 loop
            Move_To (Resize_Start + (0, 4 * I));
            delay Test_Delay_Time;
         end loop;

         Mouse_Button (1, Button_Up => True);

         Close;

         -- else let user run window
      end if;

      Background_Task.Wait_Shutdown;
   exception
   when Assertion_Error =>
      Close;
      Background_Task.Wait_Shutdown;
      raise;

   when E : others =>
      Close;
      Background_Task.Wait_Shutdown;
      Assert (False, "exception " & Ada.Exceptions.Exception_Name (E));
   end Test_Resize_Opaque;

   procedure Test_Resize_Transparent (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Glib;

      Main_Window : MDI_Main_Window.Gtk_Window;

      Resize_Start : constant Point_Type := (270,  105); --  Bottom edge of child window
   begin

      Create_Param := (Opaque_Resize => False, Opaque_Move => False);

      Background_Task.Create_Window (Gtk.Window.Gtk_Window (Main_Window));

      Background_Task.Run;

      if Debug_Level < 4 then
         delay Test_Delay_Time;

         --  Mouse selections
         Move_To (Resize_Start);

         Mouse_Button (1, Button_Up => False);

         for I in Gint'(1) .. 10 loop
            Move_To (Resize_Start + (0, 4 * I));
            delay Test_Delay_Time;
         end loop;

         Mouse_Button (1, Button_Up => True);

         Close;

         -- else let user run window
      end if;

      Background_Task.Wait_Shutdown;
   exception
   when Assertion_Error =>
      Close;
      Background_Task.Wait_Shutdown;
      raise;

   when E : others =>
      Close;
      Background_Task.Wait_Shutdown;
      Assert (False, "exception " & Ada.Exceptions.Exception_Name (E));
   end Test_Resize_Transparent;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_MDI");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Resize_Opaque'Access, "Test_Resize_Opaque");
      Register_Routine (T, Test_Resize_Transparent'Access, "Test_Resize_Transparent");
   end Register_Tests;

   procedure Set_Up_Case (Test : in out Test_Case)
   is begin
      Debug_Level := Test.Debug_Level;

      case Test.Debug_Level is
      when 0 =>
         null;

      when 1 =>
         Test_Delay_Time := 1.0;
         Default_Delay   := Test_Delay_Time;

      when others =>
         null;
      end case;

      Background_Task.Init;

   end Set_Up_Case;

end Test_MDI;
