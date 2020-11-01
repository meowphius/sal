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

with Gtk.Adjustment;
package body Gtk.Gen_Integer_Spin_Button is

   procedure Gtk_New
     (Integer        :    out Gtk_Integer;
      Initial_Value  : in     Int         := Int'First;
      Page_Increment : in     Int'Base    := 1;
      Climb_Rate     : in     Gdouble     := 0.0)
   is
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
   begin
      --  FIXME: We assume Adjustment will be deallocated by the spin
      --  button when it is.
      Gtk.Adjustment.Gtk_New
        (Adjustment,
         Value          => Gdouble (Initial_Value),
         Lower          => Gdouble (Int'First),
         Upper          => Gdouble (Int'Last),
         Step_Increment => 1.0,
         Page_Increment => Gdouble (Page_Increment),
         Page_Size      => 0.0);

      Integer := new Gtk_Integer_Record;

      Gtk.Spin_Button.Initialize
        (Spin_Button => Integer,
         Adjustment  => Adjustment,
         Climb_Rate  => Climb_Rate,
         The_Digits  => 0);

   end Gtk_New;

   function Get_Type return Gtk.Gtk_Type is
   begin
      return Gtk.Spin_Button.Get_Type;
   end Get_Type;

   function Get (Integer : access Gtk_Integer_Record) return Int is
   begin
      return Int (Get_Value (Integer));
   end Get;

   procedure Set (Integer : access Gtk_Integer_Record; To : in Int) is
   begin
      Set_Value (Integer, Gdouble (To));
   end Set;

   procedure Connect_Activate
     (Integer : access Gtk_Integer_Record'Class;
      Handler : in     Event_Handler)
   is begin
      Plain.Connect
        (Integer,
         "activate",
         Plain.Marshallers.Void_Marshaller.To_Marshaller
           (Plain.Marshallers.Void_Marshaller.Handler (Handler)));
   end Connect_Activate;

end Gtk.Gen_Integer_Spin_Button;
