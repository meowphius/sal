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

with Gdk.Test_Events; use Gdk.Test_Events;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Exceptions;
with Gtk.Box;
with Gtk.Gen_Background_Window;
with Gtk.Label;
with Gtk.Window.Signal;
with Test_Enumeral_Combo_Aux; use Test_Enumeral_Combo_Aux;
package body Test_Gtk_More.Enumeral_Combo is


   procedure Create (Window : out Gtk.Window.Gtk_Window)
   is
      Enum_Combo : Enum_Combos.Gtk_Enumeral;
      Vbox       : Gtk.Box.Gtk_Box;
      Hbox       : Gtk.Box.Gtk_Box;
      Label      : Gtk.Label.Gtk_Label;
   begin
      Gtk.Window.Gtk_New (Window);

      Gtk.Box.Gtk_New_Vbox (Vbox);
      Gtk.Box.Show (Vbox);
      Gtk.Window.Add (Window, Vbox);

      Gtk.Label.Gtk_New (Label, "Just a label to give the window a definite size");
      Gtk.Label.Show (Label);
      Gtk.Box.Pack_Start (Vbox, Label);

      --  Enum combo
      Gtk.Box.Gtk_New_Hbox (Hbox);
      Gtk.Box.Show (Hbox);
      Gtk.Box.Pack_Start (Vbox, Hbox);

      --  Label in hbox with combo to allow combo to resize
      Gtk.Label.Gtk_New (Label, "enum");
      Gtk.Label.Show (Label);
      Gtk.Box.Pack_Start (Hbox, Label);

      Enum_Combos.Gtk_New (Enum_Combo);
      Enum_Combos.Show (Enum_Combo);
      Gtk.Box.Pack_End (Hbox, Enum_Combo, Expand => False);

      Enum_Combos.Connect_Selection_Changed (Enum_Combo, On_Enum_Combo_Selection_Changed'Access);

      Gtk.Window.Signal.Connect_Destroy (Window, On_Main_Window_Destroy'Access);

   end Create;

   package Background is new Gtk.Gen_Background_Window (Create);

   use Background;

   procedure Test_Hi_Bye (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      Main_Window : Gtk.Window.Gtk_Window;
   begin
      Debug_Level     := Test.Debug_Level;

      case Test.Debug_Level is
      when 0 =>
         null;

      when 1 =>
         Debug           := True;
         Test_Delay_Time := 1.0;
         Default_Delay   := Test_Delay_Time;

      when others =>
         null;
      end case;

      Background_Task.Init;

      Background_Task.Create_Window (Main_Window);

      Background_Task.Run;
      delay Test_Delay_Time;

      --  Mouse selections
      Move_To ((200, 25)); --  Enum_combo drop arrow
      Click_Left;
      Move_To ((195, 60)); --  Two
      Click_Left;

      Assert (Selected = Two, "mouse Two");

      Move_To ((200, 25)); --  Enum_combo drop arrow
      Click_Left;
      Move_To ((195, 85)); --  Three
      Click_Left;

      Assert (Selected = Three, "mouse Three");

      --  Keyboard selections
      Move_To ((195, 25)); --  Enum_combo text
      Click_Left;
      Key_Stroke (Up); --  Two
      Assert (Selected = Two, "keyboard Two");
      Key_Stroke (Up); --  One
      Assert (Selected = One, "keyboard One");

      Key_Stroke (Enter); --  Drops list, highlights One
      Key_Stroke (Down);  --  highlights Two
      Key_Stroke (Down);  --  highlights Three
      Key_Stroke (Enter); --  Three
      Assert (Selected = Three, "keyboard Three");

      Close;

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
   end Test_Hi_Bye;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("message_box");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Hi_Bye'Access, "Test_Hi_Bye");
   end Register_Tests;

end Test_Gtk_More.Enumeral_Combo;
