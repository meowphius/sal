--  Abstract :
--
--  Display Ada.Calendar.Time
--
--  Copyright (C) 2003 Stephen Leake.  All Rights Reserved.
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
with Ada.Strings.Unbounded;
with Gtk.GEntry;
with Gtk.Widget;
package Gtk.Date_Time is

   --  Visibly derive from Widget so we can put this in containers.
   type Gtk_Date_Time_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Date_Time is access all Gtk_Date_Time_Record;

   procedure Gtk_New
     (Date_Time     :    out Gtk_Date_Time;
      Initial_Value : in Ada.Calendar.Time := Ada.Calendar.Time_Of
      (Year    => Ada.Calendar.Year_Number'First,
       Month   => Ada.Calendar.Month_Number'First,
       Day     => Ada.Calendar.Day_Number'First,
       Seconds => 0.0);
      Picture       : in     String        := "%d %b %y %I:%M %p");

   function Get_Type return Gtk.Gtk_Type;

   procedure Set_Date_Time
     (Date_Time : access Gtk_Date_Time_Record;
      Value     : in     Ada.Calendar.Time);

private

   --  Someone might want to derive from Gtk_Date_Time, so we
   --  provide an Initialize.
   procedure Initialize
     (Date_Time     : access Gtk_Date_Time_Record'Class;
      Initial_Value : in     Ada.Calendar.Time;
      Picture       : in     String);

   type Gtk_Date_Time_Record is new Gtk.GEntry.Gtk_Entry_Record with record
      Data    : Ada.Calendar.Time;
      Picture : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   procedure Display_Data (Date_Time : access Gtk_Date_Time_Record);
   --  Set Gentry text to Date_Time.Data formatted using
   --  Date_Time.Picture.

end Gtk.Date_Time;
