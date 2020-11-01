--  Abstract :
--
--  Text_IO for types in SAL.Gen_Math.Gen_Den_Hart
--
--  This file is auto-generated by auto_text_io
--  from SAL.Gen_Math.Gen_Den_Hart
--
with SAL.Gen_Math.Gen_Scalar.Gen_Text_IO;
with SAL.Gen_Math.Gen_Text_IO;
with Ada.Text_IO;
generic
   with package Math_Text_IO is new SAL.Gen_Math.Gen_Text_IO;
   with package Math_Scalar_Text_IO is new Parent_Math_Scalar.Gen_Text_IO
 (Math_Text_IO);
package SAL.Gen_Math.Gen_Den_Hart.Gen_Text_IO is

   package Joint_Class_Text_IO is new Ada.Text_IO.Enumeration_IO (Joint_Class_Type);
   procedure Put
      (File  : in Ada.Text_IO.File_Type;
       Item  : in Joint_Class_Type;
       Width : in Ada.Text_IO.Field := Joint_Class_Text_IO.Default_Width;
       Set   : in Ada.Text_IO.Type_Set := Joint_Class_Text_IO.Default_Setting)
      renames Joint_Class_Text_IO.Put;
   procedure Get
      (File  : in     Ada.Text_IO.File_Type;
       Item  :    out Joint_Class_Type)
      renames Joint_Class_Text_IO.Get;
   procedure Put
      (Item  : in Joint_Class_Type;
       Width : in Ada.Text_IO.Field := Joint_Class_Text_IO.Default_Width;
       Set   : in Ada.Text_IO.Type_Set := Joint_Class_Text_IO.Default_Setting)
      renames Joint_Class_Text_IO.Put;
   procedure Get
      (Item  :    out Joint_Class_Type)
      renames Joint_Class_Text_IO.Get;

   procedure Put
      (File                        : in Ada.Text_IO.File_Type;
       Item                        : in Den_Hart_Type;
       Single_Line_Record          : in Boolean := True;
       Named_Association_Record    : in Boolean := False;
       Single_Line_Component       : in Boolean := True;
       Named_Association_Component : in Boolean := False);
   procedure Put
      (Item                        : in Den_Hart_Type;
       Single_Line_Record          : in Boolean := True;
       Named_Association_Record    : in Boolean := False;
       Single_Line_Component       : in Boolean := True;
       Named_Association_Component : in Boolean := False);
   procedure Put_Item
      (File              : in Ada.Text_IO.File_Type;
       Item              : in Den_Hart_Type;
       Single_Line       : in Boolean := False;
       Named_Association : in Boolean := False);

   procedure Get
      (File                        : in     Ada.Text_IO.File_Type;
       Item                        :    out Den_Hart_Type;
       Named_Association_Record    : in     Boolean := False;
       Named_Association_Component : in     Boolean := False);
   procedure Get
      (Item                        :    out Den_Hart_Type;
       Named_Association_Record    : in     Boolean := False;
       Named_Association_Component : in     Boolean := False);
   procedure Get_Item
      (File              : in     Ada.Text_IO.File_Type;
       Item              :    out Den_Hart_Type;
       Named_Association : in     Boolean := False);

end SAL.Gen_Math.Gen_Den_Hart.Gen_Text_IO;