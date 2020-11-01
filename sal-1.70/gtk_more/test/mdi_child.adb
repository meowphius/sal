with Gtk.Box;
with Gtk.Enums;
with Gtk.Hbutton_Box;
package body MDI_Child is

   procedure Gtk_New (Table_View : out Gtk_Table_View)
   is begin
      Table_View := new Gtk_Table_View_Record;
      Initialize (Table_View);
   end Gtk_New;

   procedure Initialize (Table_View : access Gtk_Table_View_Record'Class)
   is
   begin
      Gtk.Box.Initialize_Vbox (Table_View);

      Gtk.Hbutton_Box.Gtk_New (Table_View.Button_Box);
      Gtk.Hbutton_Box.Set_Layout (Table_View.Button_Box, Gtk.Enums.Buttonbox_Start);
      Pack_Start (Table_View, Table_View.Button_Box, Expand => False);

      Gtk.Button.Gtk_New (Table_View.Add_Button, "Add");
      Gtk.Hbutton_Box.Add (Table_View.Button_Box, Table_View.Add_Button);

      Gtk.Button.Gtk_New (Table_View.Edit_Button, "Edit");
      Gtk.Hbutton_Box.Add (Table_View.Button_Box, Table_View.Edit_Button);

      Gtk.Button.Gtk_New (Table_View.Delete_Button, "Delete");
      Gtk.Hbutton_Box.Add (Table_View.Button_Box, Table_View.Delete_Button);

      Gtk.GEntry.Gtk_New (Table_View.Text_Entry);
      Add (Table_View, Table_View.Text_Entry);

      Show_All (Table_View);

   end Initialize;

end MDI_Child;
