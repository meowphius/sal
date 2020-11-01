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

with Glib.Object;
with Gtk.Handlers;
with Interfaces.C.Strings;
package body Gtk.Date_Time_Edit is

   Activate   : constant String := "activate";
   Class_Name : constant String := "Date_Time_Edit_Widget";

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (0 => Interfaces.C.Strings.New_String (Activate));

   package Plain is new Gtk.Handlers.Callback (Gtk_Date_Time_Edit_Record);

   --------------
   --  public operations

   procedure Gtk_New
     (Date_Time     :    out Gtk_Date_Time_Edit;
      Fields        : in     Field_Sets;
      Initial_Value : in     Ada.Calendar.Time)
   is begin
      Date_Time := new Gtk_Date_Time_Edit_Record;
      Initialize (Date_Time, Fields, Initial_Value);
   end Gtk_New;

   function Get (Date_Time : access Gtk_Date_Time_Edit_Record) return Ada.Calendar.Time
   is
      use Ada.Calendar, GNAT.Calendar;
      Day        : Day_Number;
      Month      : Month_Number;
      Year       : Year_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;

      Seconds : Gdouble;

      use type Day_Spin_Button.Gtk_Integer;
      use type Month_Spin_Button.Gtk_Integer;
      use type Year_Spin_Button.Gtk_Integer;
      use type Hour_Spin_Button.Gtk_Integer;
      use type Minute_Spin_Button.Gtk_Integer;
      use type Spin_Button.Gtk_Spin_Button;
      use type Am_Pm_Combo.Gtk_Enumeral;
   begin
      if Date_Time.Day = null then
         Day := Day_Number'First;
      else
         Day := Day_Spin_Button.Get (Date_Time.Day);
      end if;

      if Date_Time.Month = null then
         Month := Month_Number'First;
      else
         Month := Month_Spin_Button.Get (Date_Time.Month);
      end if;

      if Date_Time.Year = null then
         Year := Year_Number'First;
      else
         Year := Year_Spin_Button.Get (Date_Time.Year);
      end if;

      if Date_Time.Hour = null then
         Hour := Hour_Number'First;
      else
         Hour := Hour_Spin_Button.Get (Date_Time.Hour);
         if Date_Time.Am_Pm = null then
            null;
         else
            case Am_Pm_Combo.Get (Date_Time.Am_Pm) is
            when AM =>
               null;
            when PM =>
               Hour := Hour + 12;
            end case;
         end if;
      end if;

      if Date_Time.Minute = null then
         Minute := Minute_Number'First;
      else
         Minute := Minute_Spin_Button.Get (Date_Time.Minute);
      end if;

      if Date_Time.Seconds = null then
         Second     := 0;
         Sub_Second := 0.0;
      else
         Seconds    := Gtk.Spin_Button.Get_Value (Date_Time.Seconds);
         Second     := Second_Number (Gdouble'Floor (Seconds));
         Sub_Second := Second_Duration (Gdouble'Fraction (Seconds));
      end if;

      Date_Time.Value_Changed := False;

      return Time_Of
        (Year       => Year,
         Month      => Month,
         Day        => Day,
         Hour       => Hour,
         Minute     => Minute,
         Second     => Second,
         Sub_Second => Sub_Second);
   end Get;

   procedure Set
     (Date_Time : access Gtk_Date_Time_Edit_Record;
      Value     : in     Ada.Calendar.Time)
   is
      use Ada.Calendar, GNAT.Calendar;
      Day        : Day_Number;
      Month      : Month_Number;
      Year       : Year_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;

      use type Day_Spin_Button.Gtk_Integer;
      use type Month_Spin_Button.Gtk_Integer;
      use type Year_Spin_Button.Gtk_Integer;
      use type Hour_Spin_Button.Gtk_Integer;
      use type Minute_Spin_Button.Gtk_Integer;
      use type Spin_Button.Gtk_Spin_Button;
      use type Am_Pm_Combo.Gtk_Enumeral;
   begin
      Split
        (Value,
         Year       => Year,
         Month      => Month,
         Day        => Day,
         Hour       => Hour,
         Minute     => Minute,
         Second     => Second,
         Sub_Second => Sub_Second);

      if Date_Time.Day /= null then
         Day_Spin_Button.Set (Date_Time.Day, Day);
      end if;

      if Date_Time.Month /= null then
         Month_Spin_Button.Set (Date_Time.Month, Month);
      end if;

      if Date_Time.Year /= null then
         Year_Spin_Button.Set (Date_Time.Year, Year);
      end if;

      if Date_Time.Hour /= null then
         if Date_Time.Am_Pm /= null then
            if Hour < 12 then
               Am_Pm_Combo.Set (Date_Time.Am_Pm, AM);
            else
               Am_Pm_Combo.Set (Date_Time.Am_Pm, PM);
               Hour := Hour - 12;
            end if;
         end if;
         Hour_Spin_Button.Set (Date_Time.Hour, Hour);
      end if;

      if Date_Time.Minute /= null then
         Minute_Spin_Button.Set (Date_Time.Minute, Minute);
      end if;

      if Date_Time.Seconds /= null then
         Spin_Button.Set_Value (Date_Time.Seconds, Gdouble (Second) + Gdouble (Sub_Second));
      end if;

      Date_Time.Value_Changed := False;
   end Set;

   procedure Connect_Activate
     (Date_Time : access Gtk_Date_Time_Edit_Record'Class;
      Handler   : in     Simple_Event_Handler)
   is begin
      Plain.Connect
        (Date_Time,
         "activate",
         Plain.Marshallers.Void_Marshaller.To_Marshaller
           (Plain.Marshallers.Void_Marshaller.Handler (Handler)));
   end Connect_Activate;

   ----------
   --  Private subprograms

   procedure On_Day_Spin_Activate (Integer : access Day_Spin_Button.Gtk_Integer_Record'Class)
   is
      Date_Time : Gtk_Date_Time_Edit := Gtk_Date_Time_Edit (Day_Spin_Button.Get_Parent (Integer));
   begin
      Date_Time.Value_Changed := True;
   end On_Day_Spin_Activate;

   procedure Initialize
     (Date_Time     : access Gtk_Date_Time_Edit_Record'Class;
      Fields        : in     Field_Sets;
      Initial_Value : in     Ada.Calendar.Time)
   is begin
      Gtk.Box.Initialize_Hbox (Date_Time);

      Glib.Object.Initialize_Class_Record (Date_Time, Signals, Class_Record, Class_Name);

      Date_Time.Value_Changed := False;

      if Fields (Day) then
         Day_Spin_Button.Gtk_New (Date_Time.Day);
         Day_Spin_Button.Show (Date_Time.Day);
         Day_Spin_Button.Connect_Activate (Date_Time.Day, On_Day_Spin_Activate'Access);
         Pack_Start (Date_Time, Date_Time.Day, Expand => False);
      end if;

      if Fields (Month) then
         Month_Spin_Button.Gtk_New (Date_Time.Month);
         Month_Spin_Button.Show (Date_Time.Month);
         Pack_Start (Date_Time, Date_Time.Month, Expand => False);
      end if;

      if Fields (Year) then
         Year_Spin_Button.Gtk_New (Date_Time.Year);
         Year_Spin_Button.Show (Date_Time.Year);
         Pack_Start (Date_Time, Date_Time.Year, Expand => False);
      end if;

      if Fields (Hour) then
         Hour_Spin_Button.Gtk_New (Date_Time.Hour);
         Hour_Spin_Button.Show (Date_Time.Hour);
         Pack_Start (Date_Time, Date_Time.Hour, Expand => False);
      end if;

      if Fields (Minute) then
         Minute_Spin_Button.Gtk_New (Date_Time.Minute);
         Minute_Spin_Button.Show (Date_Time.Minute);
         Pack_Start (Date_Time, Date_Time.Minute, Expand => False);
      end if;

      if Fields (Seconds) then
         Gtk.Spin_Button.Gtk_New
           (Date_Time.Seconds,
            Min  => 0.0,
            Max  => 60.0,
            Step => 1.0);
         Gtk.Spin_Button.Show (Date_Time.Seconds);
         Pack_Start (Date_Time, Date_Time.Seconds, Expand => False);
      end if;

      if Fields (AM_PM) then
         Am_Pm_Combo.Gtk_New (Date_Time.Am_Pm);
         Am_Pm_Combo.Show (Date_Time.Am_Pm);
         Pack_Start (Date_Time, Date_Time.Am_Pm, Expand => False);
      end if;

      Set (Date_Time, Initial_Value);
   end Initialize;

end Gtk.Date_Time_Edit;
