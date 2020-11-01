with Gtk.Box;
with Gtk.Button;
with Gtk.GEntry;
with Gtk.Hbutton_Box;
package MDI_Child is

   type Gtk_Table_View_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Table_View is access all Gtk_Table_View_Record'Class;

   procedure Gtk_New (Table_View : out Gtk_Table_View);

   procedure Initialize (Table_View : access Gtk_Table_View_Record'Class);

private

   type Gtk_Table_View_Record is new Gtk.Box.Gtk_Box_Record with record
      Button_Box    : Gtk.Hbutton_Box.Gtk_Hbutton_Box;
      Add_Button    : Gtk.Button.Gtk_Button;
      Edit_Button   : Gtk.Button.Gtk_Button;
      Delete_Button : Gtk.Button.Gtk_Button;
      Text_Entry    : Gtk.GEntry.Gtk_Entry;
   end record;

end MDI_Child;
