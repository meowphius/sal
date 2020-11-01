--  Abstract :
--
--  see spec.
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

pragma License (GPL);

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Gdk.Event;
with Gdk.Test_Events;
with Gdk.Window;
with Gtk.Main;
with Gtk.Message_Box;
with Interfaces;
with SAL.Interfaces_More.Images;
with System.Storage_Elements;
package body Gtk.Gen_Background_Window is

   function Image (Item : in Gdk.Gdk_Window) return String
   is
      function To_Unsigned_32 is new Ada.Unchecked_Conversion
        (Source => Gdk.Gdk_Drawable,
         Target => Interfaces.Unsigned_32);
   begin
      return SAL.Interfaces_More.Images.Hex_Image (To_Unsigned_32 (Item));
   end Image;

   procedure Process_Command_Line_Args (Next_Arg : in out Natural)
   is
      use Ada.Command_Line;
      function Remaining_Args return Natural
      is begin
         return Argument_Count - Next_Arg + 1;
      end Remaining_Args;
   begin
      if Remaining_Args = 0 then
         --  Use default Debug_Level
         null;
      else
         Debug_Level := Natural'Value (Argument (Next_Arg));
         Next_Arg    := Next_Arg + 1;
      end if;

      if Remaining_Args = 0 then
         --  Use default Test_Delay_Time
         null;
      else
         Test_Delay_Time := Duration'Value (Argument (Next_Arg));
         Next_Arg        := Next_Arg + 1;
      end if;
   end Process_Command_Line_Args;

   Mouse_Origin    : Gdk.Test_Events.Point_Type;
   Gtk_Main_Window : Gtk.Window.Gtk_Window;

   procedure Debug_Put (Level : in Integer; Message : in String)
   is begin
      if Debug_Level >= Level then
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Debug_Put;

   procedure Debug_Event_Handler (Event : in Gdk.Event.Gdk_Event; Data : in System.Address)
   is
      pragma Unreferenced (Data);
      use Gdk.Event;
      Event_Type  : constant Gdk_Event_Type := Get_Event_Type (Event);
   begin
      if Debug_Level >= 2 then
         case Event_Type is
         when Button_Press | Button_Release =>
            declare
               use Gdk.Window, Gdk.Test_Events, Ada.Strings.Fixed;
               Event_Window      : Gdk.Gdk_Drawable;
               Junk_Point        : Point_Type;
               Top_Window        : Gdk.Gdk_Drawable;
               Top_Window_Origin : Point_Type;
               Event_Point       : constant Point_Type :=
                 (Glib.Gint (Get_X_Root (Event)), Glib.Gint (Get_Y_Root (Event)));
               Event_Image       : String (1 .. Gdk_Event_Type'Width);
            begin
               Window_At_Pointer (Junk_Point.X, Junk_Point.Y, Event_Window);
               --  Not clear what Junk_Point is, but it is _not_
               --  event_point, at least on Windows!

               Top_Window := Get_Toplevel (Event_Window);

               Get_Root_Origin (Top_Window, Top_Window_Origin.X, Top_Window_Origin.Y);

               Move
                 (Source => Gdk_Event_Type'Image (Event_Type),
                  Target => Event_Image);

               Ada.Text_IO.Put_Line
                 (Event_Image & " " &
                    Image (Get_Window (Event)) & " " &
                    Guint'Image (Get_Button (Event)) &
                    Gdk.Test_Events.Image (Top_Window_Origin) &
                    Gdk.Test_Events.Image (Event_Point - Top_Window_Origin));
            end;
         when others =>
            if Debug_Level >= 3 then
               Ada.Text_IO.Put_Line
                 (Gdk_Event_Type'Image (Event_Type) & " " &
                  Image (Get_Window (Event)));
            end if;
         end case;
      end if;

      begin
         Gtk.Main.Do_Event (Event);
      exception
      when E : others =>
         Application_Died := True;
         Gtk.Message_Box.Information_Box
           (Title   => "Unhandled exception " & Ada.Exceptions.Exception_Name (E),
            Message => Ada.Exceptions.Exception_Message (E));
      end;
   end Debug_Event_Handler;

   task body Background_Task is
      use type Gdk.Event.Event_Handler_Func;
   begin
      select
         accept Init;
      or
         terminate;
      end select;

      Gtk.Main.Set_Locale;
      Gtk.Main.Init;

      loop
         select
            accept Create_Window (Window : out Gtk.Window.Gtk_Window)
            do
               Debug_Put (1, "App_Task: Creating User Window");
               Create_User_Window (Window);
               Gtk_Main_Window := Window;
            end Create_Window;
         or
            --  exit loop
            terminate;
         end select;

         Gtk.Window.Show (Gtk_Main_Window);

         Gdk.Window.Get_Root_Origin
           (Gtk.Window.Get_Window (Gtk_Main_Window),
            X => Glib.Gint (Mouse_Origin.X),
            Y => Glib.Gint (Mouse_Origin.Y));

         Debug_Put
           (1,
            "App_Task: Window size " & Test_Events.Image
              ((X => Gtk.Window.Get_Allocation_Width (Gtk_Main_Window),
                Y => Gtk.Window.Get_Allocation_Height (Gtk_Main_Window))));

         Debug_Put (1, "App_Task: Window origin " & Test_Events.Image (Mouse_Origin));

         accept Run (Event_Handler : in Gdk.Event.Event_Handler_Func := null)
         do
            if Event_Handler = null or Debug_Level >= 2 then
               Gdk.Event.Event_Handler_Set (Debug_Event_Handler'Access, System.Storage_Elements.To_Address (0));
            else
               Gdk.Event.Event_Handler_Set (Event_Handler, System.Storage_Elements.To_Address (0));
            end if;
         end Run;

         Debug_Put (1, "App_Task: Message loop entered");

         begin
            Application_Died := False;
            Gtk.Main.Main;
         exception
         when E : others =>
            Application_Died := True;
            Ada.Text_IO.Put_Line
              ("App_Task: exception " &
                 Ada.Exceptions.Exception_Name (E) & " " &
                 Ada.Exceptions.Exception_Message (E));
         end;

         Debug_Put (1, "App_Task: Message loop exited");

         accept Wait_Shutdown;
      end loop;

   end Background_Task;

   procedure Test_Delay
   is begin
      delay Test_Delay_Time;
   end Test_Delay;

   procedure Close
   is
      use Gdk.Test_Events;
      Width : constant Glib.Gint := Glib.Gint (Gtk.Window.Get_Allocation_Width (Gtk_Main_Window));
   begin
      Debug_Put (1, "Test_Driver: Close");

      --  Click on close button (x in upper right).
      Mouse_Move (Mouse_Origin + (Width - 10, 10));

      Click_Left;

   end Close;

   procedure Move_To (Point : in Gdk.Test_Events.Point_Type)
   is
      use Gdk.Test_Events;
   begin
      Debug_Put (1, "Test_Driver: Move_To " & Image (Mouse_Origin) & Image (Point));

      Mouse_Move (Mouse_Origin + Point);
   end Move_To;

   procedure Click_Left
   is begin
      Gdk.Test_Events.Mouse_Click (Button => 1);
   end Click_Left;

end Gtk.Gen_Background_Window;
