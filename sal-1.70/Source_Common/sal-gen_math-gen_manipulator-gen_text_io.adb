--  Abstract :
--
--  See spec. This file is auto-generated by auto_text_io
--  from SAL.Gen_Math.Gen_Manipulator
--
package body SAL.Gen_Math.Gen_Manipulator.Gen_Text_IO is

   use Ada.Text_IO;

   procedure Put
      (File                      : in Ada.Text_IO.File_Type;
       Item                      : in Jacobian_Type;
       Single_Line_Array         : in Boolean := False;
       Named_Association_Array   : in Boolean := False;
       Single_Line_Element       : in Boolean := True;
       Named_Association_Element : in Boolean := False)
   is begin
      Put
       (File, DC_Array_JAR_Type (Item),
       Single_Line_Array, Named_Association_Array, Single_Line_Element, Named_Association_Element);
   end Put;

   procedure Put
      (Item                      : in Jacobian_Type;
       Single_Line_Array         : in Boolean := False;
       Named_Association_Array   : in Boolean := False;
       Single_Line_Element       : in Boolean := True;
       Named_Association_Element : in Boolean := False)
   is begin
      Put (Current_Output, Item,
           Single_Line_Array, Named_Association_Array, Single_Line_Element, Named_Association_Element);
   end Put;

   procedure Put_Item
      (File              : in Ada.Text_IO.File_Type;
       Item              : in Jacobian_Type;
       Single_Line       : in Boolean := False;
       Named_Association : in Boolean := False)
   is begin
      Put (File, Item, Single_Line, Named_Association, Single_Line, Named_Association);
   end Put_Item;

   procedure Get
      (File                      : in     Ada.Text_IO.File_Type;
       Item                      :    out Jacobian_Type;
       Named_Association_Array   : in     Boolean := False;
       Named_Association_Element : in     Boolean := False)
   is begin
      Get
       (File, DC_Array_JAR_Type (Item),
      Named_Association_Array, Named_Association_Element);
   end Get;

   procedure Get
      (Item                      :    out Jacobian_Type;
       Named_Association_Array   : in     Boolean := False;
       Named_Association_Element : in     Boolean := False)
   is begin
      Get (Current_Input, Item, Named_Association_Array, Named_Association_Element);
   end Get;

   procedure Get_Item
      (File              : in     Ada.Text_IO.File_Type;
       Item              :    out Jacobian_Type;
       Named_Association : in     Boolean := False)
   is begin
      Get (File, Item, Named_Association, Named_Association);
   end Get_Item;

   procedure Put
      (File                      : in Ada.Text_IO.File_Type;
       Item                      : in Inverse_Jacobian_Type;
       Single_Line_Array         : in Boolean := False;
       Named_Association_Array   : in Boolean := False;
       Single_Line_Element       : in Boolean := True;
       Named_Association_Element : in Boolean := False)
   is begin
      Put
       (File, Joint_Array_DCV_Type (Item),
       Single_Line_Array, Named_Association_Array, Single_Line_Element, Named_Association_Element);
   end Put;

   procedure Put
      (Item                      : in Inverse_Jacobian_Type;
       Single_Line_Array         : in Boolean := False;
       Named_Association_Array   : in Boolean := False;
       Single_Line_Element       : in Boolean := True;
       Named_Association_Element : in Boolean := False)
   is begin
      Put (Current_Output, Item,
           Single_Line_Array, Named_Association_Array, Single_Line_Element, Named_Association_Element);
   end Put;

   procedure Put_Item
      (File              : in Ada.Text_IO.File_Type;
       Item              : in Inverse_Jacobian_Type;
       Single_Line       : in Boolean := False;
       Named_Association : in Boolean := False)
   is begin
      Put (File, Item, Single_Line, Named_Association, Single_Line, Named_Association);
   end Put_Item;

   procedure Get
      (File                      : in     Ada.Text_IO.File_Type;
       Item                      :    out Inverse_Jacobian_Type;
       Named_Association_Array   : in     Boolean := False;
       Named_Association_Element : in     Boolean := False)
   is begin
      Get
       (File, Joint_Array_DCV_Type (Item),
      Named_Association_Array, Named_Association_Element);
   end Get;

   procedure Get
      (Item                      :    out Inverse_Jacobian_Type;
       Named_Association_Array   : in     Boolean := False;
       Named_Association_Element : in     Boolean := False)
   is begin
      Get (Current_Input, Item, Named_Association_Array, Named_Association_Element);
   end Get;

   procedure Get_Item
      (File              : in     Ada.Text_IO.File_Type;
       Item              :    out Inverse_Jacobian_Type;
       Named_Association : in     Boolean := False)
   is begin
      Get (File, Item, Named_Association, Named_Association);
   end Get_Item;

   procedure Put
      (File                      : in Ada.Text_IO.File_Type;
       Item                      : in Projector_Type;
       Single_Line_Array         : in Boolean := False;
       Named_Association_Array   : in Boolean := False;
       Single_Line_Element       : in Boolean := True;
       Named_Association_Element : in Boolean := False)
   is begin
      Put
       (File, Joint_Array_JAR_Type (Item),
       Single_Line_Array, Named_Association_Array, Single_Line_Element, Named_Association_Element);
   end Put;

   procedure Put
      (Item                      : in Projector_Type;
       Single_Line_Array         : in Boolean := False;
       Named_Association_Array   : in Boolean := False;
       Single_Line_Element       : in Boolean := True;
       Named_Association_Element : in Boolean := False)
   is begin
      Put (Current_Output, Item,
           Single_Line_Array, Named_Association_Array, Single_Line_Element, Named_Association_Element);
   end Put;

   procedure Put_Item
      (File              : in Ada.Text_IO.File_Type;
       Item              : in Projector_Type;
       Single_Line       : in Boolean := False;
       Named_Association : in Boolean := False)
   is begin
      Put (File, Item, Single_Line, Named_Association, Single_Line, Named_Association);
   end Put_Item;

   procedure Get
      (File                      : in     Ada.Text_IO.File_Type;
       Item                      :    out Projector_Type;
       Named_Association_Array   : in     Boolean := False;
       Named_Association_Element : in     Boolean := False)
   is begin
      Get
       (File, Joint_Array_JAR_Type (Item),
      Named_Association_Array, Named_Association_Element);
   end Get;

   procedure Get
      (Item                      :    out Projector_Type;
       Named_Association_Array   : in     Boolean := False;
       Named_Association_Element : in     Boolean := False)
   is begin
      Get (Current_Input, Item, Named_Association_Array, Named_Association_Element);
   end Get;

   procedure Get_Item
      (File              : in     Ada.Text_IO.File_Type;
       Item              :    out Projector_Type;
       Named_Association : in     Boolean := False)
   is begin
      Get (File, Item, Named_Association, Named_Association);
   end Get_Item;

   procedure Put
      (File                      : in Ada.Text_IO.File_Type;
       Item                      : in Inertia_Type;
       Single_Line_Array         : in Boolean := False;
       Named_Association_Array   : in Boolean := False;
       Single_Line_Element       : in Boolean := True;
       Named_Association_Element : in Boolean := False)
   is begin
      Put
       (File, Joint_Array_JAR_Type (Item),
       Single_Line_Array, Named_Association_Array, Single_Line_Element, Named_Association_Element);
   end Put;

   procedure Put
      (Item                      : in Inertia_Type;
       Single_Line_Array         : in Boolean := False;
       Named_Association_Array   : in Boolean := False;
       Single_Line_Element       : in Boolean := True;
       Named_Association_Element : in Boolean := False)
   is begin
      Put (Current_Output, Item,
           Single_Line_Array, Named_Association_Array, Single_Line_Element, Named_Association_Element);
   end Put;

   procedure Put_Item
      (File              : in Ada.Text_IO.File_Type;
       Item              : in Inertia_Type;
       Single_Line       : in Boolean := False;
       Named_Association : in Boolean := False)
   is begin
      Put (File, Item, Single_Line, Named_Association, Single_Line, Named_Association);
   end Put_Item;

   procedure Get
      (File                      : in     Ada.Text_IO.File_Type;
       Item                      :    out Inertia_Type;
       Named_Association_Array   : in     Boolean := False;
       Named_Association_Element : in     Boolean := False)
   is begin
      Get
       (File, Joint_Array_JAR_Type (Item),
      Named_Association_Array, Named_Association_Element);
   end Get;

   procedure Get
      (Item                      :    out Inertia_Type;
       Named_Association_Array   : in     Boolean := False;
       Named_Association_Element : in     Boolean := False)
   is begin
      Get (Current_Input, Item, Named_Association_Array, Named_Association_Element);
   end Get;

   procedure Get_Item
      (File              : in     Ada.Text_IO.File_Type;
       Item              :    out Inertia_Type;
       Named_Association : in     Boolean := False)
   is begin
      Get (File, Item, Named_Association, Named_Association);
   end Get_Item;

end SAL.Gen_Math.Gen_Manipulator.Gen_Text_IO;
