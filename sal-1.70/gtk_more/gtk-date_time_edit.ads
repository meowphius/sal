--  Abstract :
--
--  Display and edit Ada.Calendar.Time
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

pragma License (Modified_GPL);
with Ada.Calendar;
with GNAT.Calendar;
with Gtk.Box;
with Gtk.Gen_Enumeral_Combo;
with Gtk.Gen_Integer_Spin_Button;
with Gtk.Spin_Button;
with Gtk.Widget;
with SAL.Gen_Sets;

package Gtk.Date_Time_Edit is

   type Field_Labels is (Day, Month, Year, Hour, Minute, Seconds, AM_PM);

   package Field_Sets_Pkg is new SAL.Gen_Sets (Field_Labels);

   subtype Field_Subsets is Field_Sets_Pkg.Subset_Type;
   subtype Field_Sets is Field_Sets_Pkg.Set_Type;

   function Any (Item : in Field_Subsets) return Boolean
     renames Field_Sets_Pkg.Any;

   function First (Item : in Field_Subsets) return Field_Labels
     renames Field_Sets_Pkg.First;

   type Gtk_Date_Time_Edit_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Date_Time_Edit is access all Gtk_Date_Time_Edit_Record;

   procedure Gtk_New
     (Date_Time     :    out Gtk_Date_Time_Edit;
      Fields        : in     Field_Sets;
      Initial_Value : in     Ada.Calendar.Time);
   --  It's too hard to edit a time if we allow the user to specify an
   --  arbitrary picture string, so we only let them specify what
   --  fields are present.

   function Get (Date_Time : access Gtk_Date_Time_Edit_Record) return Ada.Calendar.Time;

   procedure Set (Date_Time : access Gtk_Date_Time_Edit_Record; Value : in Ada.Calendar.Time);

   ----------
   --  Signals

   type Simple_Event_Handler is access procedure (Date_Time : access Gtk_Date_Time_Edit_Record'Class);

   procedure Connect_Activate
     (Date_Time : access Gtk_Date_Time_Edit_Record'Class;
      Handler   : in     Simple_Event_Handler);
   --  Issued when value has changed since last Set or Get, and focus
   --  leaves Date_Time or user hits <return>.

private

   --  Someone might want to derive from Gtk_Date_Time_Edit, so we
   --  provide an Initialize.
   procedure Initialize
     (Date_Time     : access Gtk_Date_Time_Edit_Record'Class;
      Fields        : in     Field_Sets;
      Initial_Value : in     Ada.Calendar.Time);

   type Am_Pm_Enum is (AM, PM);

   package Day_Spin_Button is new Gtk.Gen_Integer_Spin_Button (Ada.Calendar.Day_Number);
   package Month_Spin_Button is new Gtk.Gen_Integer_Spin_Button (Ada.Calendar.Month_Number);
   package Year_Spin_Button is new Gtk.Gen_Integer_Spin_Button (Ada.Calendar.Year_Number);
   package Hour_Spin_Button is new Gtk.Gen_Integer_Spin_Button (GNAT.Calendar.Hour_Number);
   package Minute_Spin_Button is new Gtk.Gen_Integer_Spin_Button (GNAT.Calendar.Minute_Number);
   package Am_Pm_Combo is new Gtk.Gen_Enumeral_Combo (Am_Pm_Enum);

   type Gtk_Date_Time_Edit_Record is new Gtk.Box.Gtk_Box_Record with record
      Day     : Day_Spin_Button.Gtk_Integer;
      Month   : Month_Spin_Button.Gtk_Integer;
      Year    : Year_Spin_Button.Gtk_Integer;
      Hour    : Hour_Spin_Button.Gtk_Integer;
      Minute  : Minute_Spin_Button.Gtk_Integer;
      Seconds : Gtk.Spin_Button.Gtk_Spin_Button; --  float
      Am_Pm   : Am_Pm_Combo.Gtk_Enumeral;

      Value_Changed : Boolean;
   end record;

end Gtk.Date_Time_Edit;
