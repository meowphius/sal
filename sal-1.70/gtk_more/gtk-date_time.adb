--  Abstract :
--
--  See spec.
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

with GNAT.Calendar.Time_IO;
package body Gtk.Date_Time is

   procedure Gtk_New
     (Date_Time     : out Gtk_Date_Time;
      Initial_Value : in Ada.Calendar.Time := Ada.Calendar.Time_Of
      (Year    => Ada.Calendar.Year_Number'First,
       Month   => Ada.Calendar.Month_Number'First,
       Day     => Ada.Calendar.Day_Number'First,
       Seconds => 0.0);
      Picture       : in     String        := "%d %b %y %I:%M %p")
   is begin
      Date_Time := new Gtk_Date_Time_Record;
      Initialize (Date_Time, Initial_Value, Picture);
   end Gtk_New;

   function Get_Type return Gtk.Gtk_Type is
   begin
      return Gtk.GEntry.Get_Type;
   end Get_Type;

   procedure Set_Date_Time
     (Date_Time : access Gtk_Date_Time_Record;
      Value     : in     Ada.Calendar.Time)
   is begin
      Date_Time.Data := Value;
      Display_Data (Date_Time);
   end Set_Date_Time;

   procedure Initialize
     (Date_Time     : access Gtk_Date_Time_Record'Class;
      Initial_Value : in     Ada.Calendar.Time;
      Picture       : in     String)
   is begin
      Date_Time.Picture := Ada.Strings.Unbounded.To_Unbounded_String (Picture);
      Date_Time.Data    := Initial_Value;
      Display_Data (Date_Time);
   end Initialize;

   procedure Display_Data (Date_Time : access Gtk_Date_Time_Record)
   is
      use GNAT.Calendar.Time_IO;
      Formatted_Data : constant String := Image
        (Date_Time.Data, Picture_String (Ada.Strings.Unbounded.To_String (Date_Time.Picture)));
   begin
      Set_Text (Date_Time, Formatted_Data);
   end Display_Data;

end Gtk.Date_Time;
