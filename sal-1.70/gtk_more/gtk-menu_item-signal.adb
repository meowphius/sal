--  Abstract :
--
--  See spec
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

with Gtk.Handlers;
package body Gtk.Menu_Item.Signal is

   package Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Menu_Item_Record,
      User_Type   => Gtk.Widget.Gtk_Widget);

   procedure Connect_Activate
     (Menu_Item : access Gtk_Menu_Item_Record'Class;
      Handler   : in     Event_Handler;
      Owner     : in     Gtk.Widget.Gtk_Widget)
   is
   begin
      Callback.Connect
        (Widget    => Menu_Item,
         Name      => "activate",
         Marsh     => Callback.Marshallers.Void_Marshaller.To_Marshaller
           (Callback.Marshallers.Void_Marshaller.Handler (Handler)),
         User_Data => Owner);
   end Connect_Activate;

   procedure Connect_Activate_Item
     (Menu_Item : access Gtk_Menu_Item_Record'Class;
      Handler   : in     Event_Handler;
      Owner     : in     Gtk.Widget.Gtk_Widget)
   is
   begin
      Callback.Connect
        (Menu_Item,
         "activate_item",
         Callback.Marshallers.Void_Marshaller.To_Marshaller
           (Callback.Marshallers.Void_Marshaller.Handler (Handler)),
        Owner);
   end Connect_Activate_Item;

end Gtk.Menu_Item.Signal;
