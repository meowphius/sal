--  Abstract :
--
--  Safe Connect subprograms for Gtk.Menu_Item signals.
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

pragma License (Modified_GPL);

package Gtk.Menu_Item.Signal is

   type Event_Handler is access procedure
     (Menu_Item : access Gtk_Menu_Item_Record'Class;
      Owner     : in     Gtk.Widget.Gtk_Widget);

   procedure Connect_Activate
     (Menu_Item : access Gtk_Menu_Item_Record'Class;
      Handler   : in     Event_Handler;
      Owner     : in     Gtk.Widget.Gtk_Widget);
   --  Emitted when a menu item is activated

   procedure Connect_Activate_Item
     (Menu_Item : access Gtk_Menu_Item_Record'Class;
      Handler   : in     Event_Handler;
      Owner     : in     Gtk.Widget.Gtk_Widget);
   --  Emitted when a submenu is activated

end Gtk.Menu_Item.Signal;
