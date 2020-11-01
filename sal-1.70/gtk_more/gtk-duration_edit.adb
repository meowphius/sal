--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003, 2004 Stephen Leake.  All Rights Reserved.
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

with Gtk.Handlers;
with Interfaces.C.Strings;
package body Gtk.Duration_Edit is

   --  Global pointer to the Duration_Edit C 'class record' for
   --  gtk.
   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   --  New signals for this widget. String memory freed by operating
   --  system on program exit.
   Signals : constant Interfaces.C.Strings.chars_ptr_array := (0 => Interfaces.C.Strings.New_String ("activate"));

   package Plain is new Gtk.Handlers.Callback (Gtk_Duration_Edit_Record);

   --------------
   --  public operations

   procedure Gtk_New
     (Duration      :    out Gtk_Duration_Edit;
      Fields        : in     Field_Sets;
      Initial_Value : in     Ada.Calendar.Day_Duration := 0.0)
   is begin
      Duration := new Gtk_Duration_Edit_Record;
      Initialize (Duration, Fields, Initial_Value);
   end Gtk_New;

   function Get
     (Duration : access Gtk_Duration_Edit_Record)
      return Ada.Calendar.Day_Duration
   is
      use Ada.Calendar, GNAT.Calendar;
      Hour        : Hour_Number   := 0;
      Minute      : Minute_Number := 0;
      Second      : Second_Number := 0;
      Sub_Seconds : Gdouble       := 0.0;

      use type Hour_Spin_Button.Gtk_Integer;
      use type Minute_Spin_Button.Gtk_Integer;
      use type Second_Spin_Button.Gtk_Integer;
      use type Gtk.Spin_Button.Gtk_Spin_Button;
   begin
      if Duration.Hour /= null then
         Hour := Hour_Spin_Button.Get (Duration.Hour);
      end if;

      if Duration.Minute /= null then
         Minute := Minute_Spin_Button.Get (Duration.Minute);
      end if;

      if Duration.Second /= null then
         Second := Second_Spin_Button.Get (Duration.Second);
      end if;

      if Duration.Sub_Seconds /= null then
         Sub_Seconds := Gtk.Spin_Button.Get_Value (Duration.Sub_Seconds);
      end if;

      Duration.Value_Changed := False;
      return Day_Duration (Sub_Seconds) + Day_Duration (Second) + 60.0 * (Day_Duration (Minute) + 60.0 * Hour);
   end Get;

   procedure Set
     (Duration : access Gtk_Duration_Edit_Record;
      Value : in Ada.Calendar.Day_Duration)
   is
      use Ada.Calendar, GNAT.Calendar;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;

      Secs : Natural;

      use type Hour_Spin_Button.Gtk_Integer;
      use type Minute_Spin_Button.Gtk_Integer;
      use type Second_Spin_Button.Gtk_Integer;
      use type Gtk.Spin_Button.Gtk_Spin_Button;
   begin
      if Value = 0.0 then
         Secs := 0;
      else
         Secs := Natural (Value - 0.5);
      end if;

      Sub_Second := Second_Duration (Value - Day_Duration (Secs));
      Hour       := Hour_Number (Secs / 3600);
      Secs       := Secs mod 3600;
      Minute     := Minute_Number (Secs / 60);
      Second     := Second_Number (Secs mod 60);

      if Duration.Hour /= null then
         Hour_Spin_Button.Set (Duration.Hour, Hour);
      end if;

      if Duration.Minute /= null then
         Minute_Spin_Button.Set (Duration.Minute, Minute);
      end if;

      if Duration.Second /= null then
         Second_Spin_Button.Set (Duration.Second, Second);
      end if;

      if Duration.Sub_Seconds /= null then
         Spin_Button.Set_Value (Duration.Sub_Seconds, Gdouble (Sub_Second));
      end if;

      Duration.Value_Changed := False;
   end Set;

   procedure Connect_Activate
     (Duration : access Gtk_Duration_Edit_Record'Class;
      Handler  : in     Simple_Event_Handler)
   is begin
      Plain.Connect
        (Duration,
         "activate",
         Plain.Marshallers.Void_Marshaller.To_Marshaller
           (Plain.Marshallers.Void_Marshaller.Handler (Handler)));
   end Connect_Activate;

   ----------
   --  Private subprograms

   procedure On_Hour_Spin_Activate (Integer : access Hour_Spin_Button.Gtk_Integer_Record'Class)
   is
      Duration : Gtk_Duration_Edit := Gtk_Duration_Edit (Hour_Spin_Button.Get_Parent (Integer));
   begin
      Duration.Value_Changed := True;
   end On_Hour_Spin_Activate;

   procedure Initialize
     (Duration      : access Gtk_Duration_Edit_Record'Class;
      Fields        : in     Field_Sets;
      Initial_Value : in     Ada.Calendar.Day_Duration)
   is begin
      Gtk.Box.Initialize_Hbox (Duration);

      Glib.Object.Initialize_Class_Record (Duration, Signals, Class_Record, "Duration_Edit_Widget");

      if Fields (Hour) then
         Hour_Spin_Button.Gtk_New (Duration.Hour);
         Hour_Spin_Button.Show (Duration.Hour);
         Hour_Spin_Button.Connect_Activate (Duration.Hour, On_Hour_Spin_Activate'Access);
         Pack_Start (Duration, Duration.Hour);
      end if;

      if Fields (Minute) then
         Minute_Spin_Button.Gtk_New (Duration.Minute);
         Minute_Spin_Button.Show (Duration.Minute);
         Pack_Start (Duration, Duration.Minute);
      end if;

      if Fields (Second) then
         Second_Spin_Button.Gtk_New (Duration.Second);
         Second_Spin_Button.Show (Duration.Second);
         Pack_Start (Duration, Duration.Second);
      end if;

      if Fields (Sub_Seconds) then
         Gtk.Spin_Button.Gtk_New
           (Duration.Sub_Seconds,
            Min  => 0.0,
            Max  => 1.0,
            Step => 0.1);
         Gtk.Spin_Button.Show (Duration.Sub_Seconds);
         Pack_Start (Duration, Duration.Sub_Seconds);
      end if;

      Set (Duration, Initial_Value);

   end Initialize;

end Gtk.Duration_Edit;
