-- Abstract:
--  see spec
--
-- Notes:
--  We assume a byte-addressable machine
--
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
package body SAL.Memory_Streams.Address is

   procedure Create
      (Stream : in out Stream_Type;
       Address : in System.Address)
   is begin
      Stream.Address := Address;
   end Create;

   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      use type System.Storage_Elements.Storage_Offset;
   begin
      for I in Item'Range loop
         Item (I) := Stream_Element_Address_Conversions.To_Pointer (Stream.Address).all;
         Stream.Address := Stream.Address + 1;
      end loop;
      Last := Item'Last;
   end Read;

   procedure Write
     (Stream : in out Stream_Type;
      Item   : in Stream_Element_Array)
   is begin
      raise Status_Error;
   end Write;

   package Character_Address_Conversions is new System.Address_To_Access_Conversions (Character);

   function Null_Terminated_String_Length
      (Stream : in Stream_Type)
      return Natural
   is
      use type System.Storage_Elements.Storage_Offset;
      Count : System.Storage_Elements.Storage_Offset := 0;
   begin
      while ASCII.NUL /= Character_Address_Conversions.To_Pointer (Stream.Address + Count).all
      loop
         Count := Count + 1;
      end loop;
      return Natural (Count);
   end Null_Terminated_String_Length;

   package Wide_Character_Address_Conversions is new System.Address_To_Access_Conversions (Wide_Character);

   function Null_Terminated_Wide_String_Length
      (Stream : in Stream_Type)
      return Natural
   is
      use type System.Storage_Elements.Storage_Offset;
      Count : System.Storage_Elements.Storage_Offset := 0;
   begin
      while Wide_Character'Val (0) /=
         Wide_Character_Address_Conversions.To_Pointer (Stream.Address + 2 * Count).all
      loop
         Count := Count + 1;
      end loop;
      return Natural (Count);
   end Null_Terminated_Wide_String_Length;

end SAL.Memory_Streams.Address;
