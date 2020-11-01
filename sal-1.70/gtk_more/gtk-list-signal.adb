--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2003, 2004 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

with Gtk.Handlers;
package body Gtk.List.Signal is

   package Plain is new Gtk.Handlers.Callback (Gtk_List_Record);
   --  No user data, no return value.

   package Returns is new Gtk.Handlers.Return_Callback (Gtk_List_Record, Boolean);

   procedure Connect_Event
     (List    : access Gtk_List_Record'Class;
      Handler : in     Boolean_Event_Handler)
   is begin
      Returns.Connect
        (List,
         "event",
         Returns.Event_Marshaller.To_Marshaller
           (Returns.Event_Marshaller.Handler (Handler)));
   end Connect_Event;

   procedure Connect_Selection_Changed
     (List    : access Gtk_List_Record'Class;
      Handler : in     Event_Handler)
   is begin
      Plain.Connect
        (List,
         "selection_changed",
         Plain.Marshallers.Void_Marshaller.To_Marshaller
           (Plain.Marshallers.Void_Marshaller.Handler (Handler)));
   end Connect_Selection_Changed;

end Gtk.List.Signal;
