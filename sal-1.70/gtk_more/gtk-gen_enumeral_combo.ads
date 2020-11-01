--  Abstract :
--
--  Generic to create a Gtk_Combo containing enumeration literals.
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
with Gtk.Combo;
with Gtk.Editable;
with Gtk.Handlers;
with Gtk.Widget;
generic
   type Enum is (<>);
package Gtk.Gen_Enumeral_Combo is

   --  Visibly derive from Widget so we can put this in containers.
   type Gtk_Enumeral_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Enumeral is access all Gtk_Enumeral_Record;

   procedure Gtk_New (Combo : out Gtk_Enumeral);

   --  There should be no need to derive from Gtk_Enumeral_Record, so
   --  we don't provide an Initialize.

   procedure Disable_Activate (Combo : access Gtk_Enumeral_Record);
   --  Disable the standard handler for the <return> key in the entry field.
   --  The default behavior is to popdown the combo box list, so that the user
   --  can choose from it. However, if you want to add your own callback
   --  for the return key, you need to call this subprogram, and connect
   --  a handler to the "activate" signal for the entry.

   function Get (Combo : access Gtk_Enumeral_Record) return Enum;

   procedure Set (Combo : access Gtk_Enumeral_Record; Item : in Enum);
   --  Set current selection to Item.

   ----------
   --  Signals

   type Event_Handler is access procedure (Combo : access Gtk_Enumeral_Record'Class);

   procedure Connect_Selection_Changed
     (Combo   : access Gtk_Enumeral_Record'Class;
      Handler : in     Event_Handler);
   --  Issued when user selects a value.

private
   type Gtk_Enumeral_Record is new Gtk.Combo.Gtk_Combo_Record with null record;

   --  This must be in the spec to allow taking
   --  On_Entry_Changed'access in body.
   procedure On_Entry_Changed (Editable : access Gtk.Editable.Gtk_Editable_Record'Class);

   package Plain is new Gtk.Handlers.Callback (Gtk_Enumeral_Record);

end Gtk.Gen_Enumeral_Combo;

