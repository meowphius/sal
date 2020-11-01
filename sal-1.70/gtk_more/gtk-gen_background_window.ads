--  Abstract :
--
--  Generic window in a background task, with utilities for simulating
--  user input events, for testing application windows.
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

with Gdk.Event;
with Gdk.Test_Events;
with Gtk.Window;
generic
   with procedure Create_User_Window (Window : out Gtk.Window.Gtk_Window);
   --  Creat the GUI to be tested.
package Gtk.Gen_Background_Window is

   Debug_Level     : Natural  := 0;
   Test_Delay_Time : Duration := 0.1; -- time required to let background task process a typical event

   Command_Line_Usage : constant String := "[<debug_level> [<view_delay_time>]]";

   procedure Process_Command_Line_Args (Next_Arg : in out Natural);
   --  Set Debug_Level, Test_Delay_Time from Ada.Command_Line.Argument
   --  (Next_Arg), Next_Arg + 1, according to Command_Line_Usage, if present.
   --  Leaves default values if command line arguments not present.

   ----------

   Application_Died : Boolean := False;

   task Background_Task is

      entry Init;
      --  Task waits for Init, then runs Gtk.Main.Set_Local,
      --  Gtk.Main.Init, then waits for each of the following entries,
      --  in the order declared. Then loops back to wait for
      --  Create_Window, or terminate.
      --
      --  This allows more than one test case to run Gtk.Main.Init, or
      --  several test cases to share a single background task.

      entry Create_Window (Window : out Gtk.Window.Gtk_Window);
      --  Calls Create_User_Window.

      entry Run (Event_Handler : in Gdk.Event.Event_Handler_Func := null);
      --  Show Window, run Gtk.Main.Main.
      --
      --  If Event_Handler is null, or if Debug_Level >= 2, uses
      --  default debug event handler, that sets Application_Died :=
      --  True for any exceptions propagated out of Gtk.Main.Main, and
      --  displays location of mouse clicks if Debug_Level >= 2.
      --  Otherwise, installs Event_Handler.


      entry Wait_Shutdown;
      --  Unblocks after message loop is exited (normally by Window
      --  being destroyed), and background task is ready to terminate.
      --  Useful for just letting user run window to generate messages
      --  for debugging.

   end Background_Task;

   procedure Test_Delay;
   --  Delay for Test_Delay_Time. This lets the background task message
   --  loop process window events. Note that Test_Delay_Time defaults
   --  to 0.1, but can be overridden by Process_Command_Line,
   --  allowing user to see what is happening.

   ----------
   --  Events
   --
   --  For other events, use Gdk.Test_Events

   procedure Close;
   --  Click on close button.

   procedure Move_To (Point : in Gdk.Test_Events.Point_Type);
   --  Move mouse to Point, in Window coordinates. Does not delay.
   --
   --  If Debug_Level >= 1, shows destination point.

   procedure Click_Left;
   --  Click left mouse button at current position, then delay for
   --  Test_Delay_Time.

private

   procedure Debug_Event_Handler (Event : in Gdk.Event.Gdk_Event; Data : in System.Address);

end Gtk.Gen_Background_Window;
