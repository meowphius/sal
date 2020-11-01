--  Abstract :
--
--  Text_IO for types in System.Storage_Elements, for use by Auto_Text_IO.
--

with Ada.Text_IO;
with System.Storage_Elements;
package System_Storage_Elements_Text_IO is
--   pragma Elaborate_Body; -- Ada.Text_IO is, but we have no body

   package Integer_Address_Text_IO is new Ada.Text_IO.Modular_IO (System.Storage_Elements.Integer_Address);
   procedure Put
     (File  : in Ada.Text_IO.File_Type;
      Item  : in System.Storage_Elements.Integer_Address;
      Width : in Ada.Text_IO.Field       := Integer_Address_Text_IO.Default_Width;
      Base  : in Ada.Text_IO.Number_Base := Integer_Address_Text_IO.Default_Base)
     renames Integer_Address_Text_IO.Put;
   procedure Get
     (File  : in     Ada.Text_IO.File_Type;
      Item  :    out System.Storage_Elements.Integer_Address;
      Width : in     Ada.Text_IO.Field     := 0)
     renames Integer_Address_Text_IO.Get;

   package Storage_Offset_Text_IO is new Ada.Text_IO.Integer_IO (System.Storage_Elements.Storage_Offset);
   procedure Put
     (File  : in Ada.Text_IO.File_Type;
      Item  : in System.Storage_Elements.Storage_Offset;
      Width : in Ada.Text_IO.Field       := Storage_Offset_Text_IO.Default_Width;
      Base  : in Ada.Text_IO.Number_Base := Storage_Offset_Text_IO.Default_Base)
     renames Storage_Offset_Text_IO.Put;
   procedure Get
     (File  : in     Ada.Text_IO.File_Type;
      Item  :    out System.Storage_Elements.Storage_Offset;
      Width : in     Ada.Text_IO.Field     := 0)
     renames Storage_Offset_Text_IO.Get;

end System_Storage_Elements_Text_IO;
