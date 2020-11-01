--  Abstract :
--
--  See spec.
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

with Gtk.Editable.Signal;
with Gtk.Enumeral_Combo;
with Gtk.Enums;
with Gtk.GEntry;
with Gtk.List;
with Pango.Font;
package body Gtk.Gen_Enumeral_Combo is

   procedure Emit_By_Name (Combo : access Gtk_Enumeral_Record'Class; Name : in String)
     renames Plain.Marshallers.Void_Marshaller.Emit_By_Name;

   procedure On_Entry_Changed (Editable : access Gtk.Editable.Gtk_Editable_Record'Class)
   is
      Combo : constant Gtk_Enumeral := Gtk_Enumeral (Gtk.Editable.Get_Parent (Editable));
      Text  : constant String       := Gtk.GEntry.Get_Text (Get_Entry (Combo));
   begin
      if Text'Length = 0 then
         --  Entry was just emptied; wait for next change.
         null;
      else
         Emit_By_Name (Combo, Gtk.Enumeral_Combo.Selection_Changed);
      end if;
   end On_Entry_Changed;

   ----------
   --  Public subprograms

   procedure Gtk_New (Combo : out Gtk_Enumeral)
   is
      Strings : Gtk.Enums.String_List.Glist;

      --  WORKAROUND: Set_Width_Chars assumes a fixed width font, so
      --  ensure we are using one for the combo box entry widget.
      --  Better would be to compute the actual length of all the
      --  enumerals using the current font, using
      --  Gtk.Widget.Create_Pango_Layout, Pango.Layout.Set_Text, and
      --  Pango.Layout.Get_Pixel_Size; left for later.
      Font_Desc : constant Pango.Font.Pango_Font_Description := Pango.Font.From_String ("monospace 10");
   begin
      Combo := new Gtk_Enumeral_Record;
      Gtk.Combo.Initialize (Combo);

      Glib.Object.Initialize_Class_Record
        (Combo, Gtk.Enumeral_Combo.Signals, Gtk.Enumeral_Combo.Class_Record, Gtk.Enumeral_Combo.Class_Name);

      Gtk.GEntry.Modify_Font (Get_Entry (Combo), Font_Desc);

      Set_Value_In_List (Combo, Val => True, Ok_If_Empty => False);

      Gtk.GEntry.Set_Width_Chars (Get_Entry (Combo), Enum'Width);

      for I in Enum'Range loop
         Gtk.Enums.String_List.Append (Strings, Enum'Image (I));
      end loop;

      Set_Popdown_Strings (Combo, Strings);

      Gtk.Editable.Signal.Connect_Changed (Get_Entry (Combo), On_Entry_Changed'Access);
   end Gtk_New;

   procedure Disable_Activate (Combo : access Gtk_Enumeral_Record) is
   begin
      --  We are making the privately inherited subprogram visible;
      --  avoid infinite recursion.
      Gtk.Combo.Disable_Activate (Gtk.Combo.Gtk_Combo (Combo));
   end Disable_Activate;

   function Get (Combo : access Gtk_Enumeral_Record) return Enum
   is
      Text : constant String := Gtk.GEntry.Get_Text (Get_Entry (Combo));
   begin
      --  This will raise Constraint_Error only if abstraction is
      --  violated; Entry should always contain a valid Enum image. It
      --  doesn't only while it is being changed from one value to
      --  another.
      return Enum'Value (Text);
   end Get;

   procedure Set
     (Combo : access Gtk_Enumeral_Record;
      Item  : in     Enum)
   is
      Combo_List : constant Gtk.List.Gtk_List := Get_List (Combo);
   begin
      Gtk.List.Select_Item (Combo_List, Enum'Pos (Item));
   end Set;

   procedure Connect_Selection_Changed
     (Combo   : access Gtk_Enumeral_Record'Class;
      Handler : in     Event_Handler)
   is begin
      Plain.Connect
        (Combo,
         Gtk.Enumeral_Combo.Selection_Changed,
         Plain.Marshallers.Void_Marshaller.To_Marshaller
           (Plain.Marshallers.Void_Marshaller.Handler (Handler)));
   end Connect_Selection_Changed;

end Gtk.Gen_Enumeral_Combo;
