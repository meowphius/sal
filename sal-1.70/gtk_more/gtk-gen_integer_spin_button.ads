--  Abstract :
--
--  Simplified interface to a spin button; display and edit an
--  integer.
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

with Gtk.Handlers;
with Gtk.Spin_Button;
with Gtk.Widget;
generic
   type Int is range <>;
package Gtk.Gen_Integer_Spin_Button is

   --  Visibly derive from Widget so we can put this in containers.
   type Gtk_Integer_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Integer is access all Gtk_Integer_Record;

   procedure Gtk_New
     (Integer        :    out Gtk_Integer;
      Initial_Value  : in     Int         := Int'First;
      Page_Increment : in     Int'Base    := 1;
      Climb_Rate     : in     Gdouble     := 0.0);

   --  There should be no need to derive from Gtk_Integer_Record, so
   --  we don't provide an Initialize.

   function Get_Type return Gtk.Gtk_Type;

   function Get (Integer : access Gtk_Integer_Record) return Int;
   procedure Set (Integer : access Gtk_Integer_Record; To : in Int);

   ----------
   --  Signals

   type Event_Handler is access procedure (Integer : access Gtk_Integer_Record'Class);

   procedure Connect_Activate
     (Integer : access Gtk_Integer_Record'Class;
      Handler : in     Event_Handler);
   --  Sent when value has changed since last Set or Get, and user
   --  hits <return> or focus leaves Integer.

private

   type Gtk_Integer_Record is new Gtk.Spin_Button.Gtk_Spin_Button_Record with null record;

   --  This must be in the spec due to RM 3.9.1(4) (see AARM for explanation).
   package Plain is new Gtk.Handlers.Callback (Gtk_Integer_Record);

end Gtk.Gen_Integer_Spin_Button;
