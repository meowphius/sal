with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Object;
package body MDI_Main_Window is

   package Plain is new Gtk.Handlers.Callback (Gtk.Object.Gtk_Object_Record);
   --  No user data, no return value.

   type Event_Handler is access procedure (Object : access Gtk.Object.Gtk_Object_Record'Class);

   procedure Connect_Destroy
     (Object  : access Gtk.Object.Gtk_Object_Record'Class;
      Handler : in     Event_Handler)
   is begin
      Plain.Connect
        (Object,
         "destroy",
         Plain.Marshallers.Void_Marshaller.To_Marshaller
           (Plain.Marshallers.Void_Marshaller.Handler (Handler)));
   end Connect_Destroy;

   procedure On_Window_Destroy (Object : access Gtk.Object.Gtk_Object_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Main_Quit;
   end On_Window_Destroy;

   procedure Initialize (Window : access Gtk_Window_Record'Class; Param : in Create_Parameters_Type)
   is begin
      Gtk.Window.Initialize (Window, Gtk.Enums.Window_Toplevel);
      Set_Title (Window, "MDI");

      Connect_Destroy (Window, On_Window_Destroy'Access);

      Set_Size_Request (Window, 300, 200);

      Gtk.Accel_Group.Gtk_New (Window.Accel_Group);
      Add_Accel_Group (Window, Window.Accel_Group);
      --  FIXME: define accelerators

      Gtk.Box.Gtk_New_Vbox (Window.Vbox);
      Gtk.Box.Show (Window.Vbox);
      Add (Window, Window.Vbox);

      --  FIXME: add menu bar

      Gtkada.MDI.Gtk_New (Window.MDI, Window.Accel_Group);
      Gtkada.MDI.Show_All (Window.MDI);
      Gtkada.MDI.Configure
        (Window.MDI,
         Opaque_Resize => Param.Opaque_Resize,
         Opaque_Move   => Param.Opaque_Move);

      Gtk.Box.Add (Window.Vbox, Window.MDI);

      MDI_Child.Gtk_New (Window.Author_View);
      Window.Author_Child := Gtkada.MDI.Put (Window.MDI, Window.Author_View);
      Gtkada.MDI.Set_Title (Window.Author_Child, "Authors");

      MDI_Child.Gtk_New (Window.Title_View);
      Window.Title_Child := Gtkada.MDI.Put (Window.MDI, Window.Title_View);
      Gtkada.MDI.Set_Title (Window.Title_Child, "Titles");
   end Initialize;

   procedure Gtk_New (Window : out Gtk_Window; Param : in Create_Parameters_Type)
   is  begin
      Window := new Gtk_Window_Record;
      Initialize (Window, Param);
   end Gtk_New;

end MDI_Main_Window;
