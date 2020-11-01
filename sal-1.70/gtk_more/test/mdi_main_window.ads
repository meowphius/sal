with Gtk.Accel_Group;
with Gtk.Box;
with Gtk.Window;
with Gtkada.MDI;
with MDI_Child;
package MDI_Main_Window is

   type Gtk_Window_Record is new Gtk.Window.Gtk_Window_Record with private;
   type Gtk_Window is access all Gtk_Window_Record'Class;

   type Create_Parameters_Type is record
      Opaque_Move : Boolean;
      Opaque_Resize : Boolean;
   end record;

   procedure Gtk_New (Window : out Gtk_Window; Param : in Create_Parameters_Type);

private

   type Gtk_Window_Record is new Gtk.Window.Gtk_Window_Record with record
      Vbox : Gtk.Box.Gtk_Box;
      --  Vbox contains menu bar and MDI

      MDI : Gtkada.MDI.MDI_Window;
      --  MDI contains children

      Accel_Group      : Gtk.Accel_Group.Gtk_Accel_Group;
      Author_View      : MDI_Child.Gtk_Table_View;
      Author_Child     : Gtkada.MDI.MDI_Child;
      Title_View       : MDI_Child.Gtk_Table_View;
      Title_Child      : Gtkada.MDI.MDI_Child;

   end record;

end MDI_Main_Window;
