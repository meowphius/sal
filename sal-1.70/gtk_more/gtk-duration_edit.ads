--  Abstract :
--
--  Display and edit Ada.Calendar.Day_Duration
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
with Gtk.Gen_Integer_Spin_Button;
with Gtk.Spin_Button;
with Gtk.Widget;
with SAL.Gen_Sets;
package Gtk.Duration_Edit is

   type Field_Labels is (Hour, Minute, Second, Sub_Seconds);

   package Field_Sets_Pkg is new SAL.Gen_Sets (Field_Labels);

   subtype Field_Subsets is Field_Sets_Pkg.Subset_Type;
   subtype Field_Sets is Field_Sets_Pkg.Set_Type;

   function Any (Item : in Field_Subsets) return Boolean
     renames Field_Sets_Pkg.Any;

   function First (Item : in Field_Subsets) return Field_Labels
     renames Field_Sets_Pkg.First;

   type Gtk_Duration_Edit_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Duration_Edit is access all Gtk_Duration_Edit_Record;

   procedure Gtk_New
     (Duration      :    out Gtk_Duration_Edit;
      Fields        : in     Field_Sets;
      Initial_Value : in     Ada.Calendar.Day_Duration := 0.0);

   function Get (Duration : access Gtk_Duration_Edit_Record) return Ada.Calendar.Day_Duration;

   procedure Set (Duration : access Gtk_Duration_Edit_Record; Value : in Ada.Calendar.Day_Duration);

   ----------
   --  Signals

   type Simple_Event_Handler is access procedure (View : access Gtk_Duration_Edit_Record'Class);

   procedure Connect_Activate
     (Duration : access Gtk_Duration_Edit_Record'Class;
      Handler  : in     Simple_Event_Handler);
   --  Issued when value has changed since last Get or Set, and focus
   --  leaves Duration or user hits <return>.

private

   --  Someone might want to derive from Gtk_Duration_Edit, so we
   --  provide an Initialize.
   procedure Initialize
     (Duration      : access Gtk_Duration_Edit_Record'Class;
      Fields        : in     Field_Sets;
      Initial_Value : in     Ada.Calendar.Day_Duration);

   package Hour_Spin_Button is new Gtk.Gen_Integer_Spin_Button (GNAT.Calendar.Hour_Number);
   package Minute_Spin_Button is new Gtk.Gen_Integer_Spin_Button (GNAT.Calendar.Minute_Number);
   package Second_Spin_Button is new Gtk.Gen_Integer_Spin_Button (GNAT.Calendar.Second_Number);

   type Gtk_Duration_Edit_Record is new Gtk.Box.Gtk_Box_Record with record
      Hour        : Hour_Spin_Button.Gtk_Integer;
      Minute      : Minute_Spin_Button.Gtk_Integer;
      Second      : Second_Spin_Button.Gtk_Integer;
      Sub_Seconds : Gtk.Spin_Button.Gtk_Spin_Button; --  float

      Value_Changed : Boolean;
   end record;

end Gtk.Duration_Edit;
