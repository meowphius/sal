-- Abstract:
--  see spec
--
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
package body SAL.Memory_Streams.Bounded is

   procedure Create (Stream : in out Stream_Type)
   is begin
      Stream.Last := 0;
      Stream.Direction := Out_Stream;
   end Create;

   procedure Create
      (Stream : in out Stream_Type;
       Data : in Stream_Element_Array)
   is begin
      Stream.Raw (1 .. Data'Length) := Data;
      Stream.Last := 0;
      Stream.Direction := In_Stream;
   end Create;

   package Stream_Element_Address_Conversions is new System.Address_To_Access_Conversions (Stream_Element);

   procedure Create
      (Stream : in out Stream_Type;
       Address : in System.Address)
   is
      function "+" (Left : System.Address; Right : System.Storage_Elements.Storage_Offset)
                    return System.Address renames System.Storage_Elements."+";

      Temp : System.Address := Address;
   begin
      for I in Stream.Raw'Range loop
         Stream.Raw (I) := Stream_Element_Address_Conversions.To_Pointer (Address).all;
         Temp := Temp + 1;
      end loop;
      Stream.Direction := In_Stream;
      Stream.Last := 0;
   end Create;

   function Length (Stream : in Stream_Type) return Stream_Element_Count
   is begin
      case Stream.Direction is
      when In_Stream =>
         return Stream.Raw'Last - Stream.Last;
      when Out_Stream =>
         return Stream.Last;
      end case;
   end Length;

   function Address (Stream : in Stream_Type) return System.Address
   is begin
      case Stream.Direction is
      when In_Stream =>
         raise Status_Error;
      when Out_Stream =>
         return Stream.Raw (1)'Address;
      end case;
   end Address;

   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is begin
      case Stream.Direction is
      when In_Stream =>
         declare
            Remaining : constant Stream_Element_Offset := Stream.Raw'Last - Stream.Last;
         begin
            if Remaining >= Item'Length then
               Item := Stream.Raw (Stream.Last + 1 .. Stream.Last + Item'Length);
               Stream.Last := Stream.Last + Item'Length;
               Last := Item'Last;
            else
               Last := Item'First + Remaining - 1;
               Item (Item'First .. Last) := Stream.Raw (Stream.Last + 1 .. Stream.Raw'Last);
               Stream.Last := Stream.Raw'Last;
            end if;
         end;
      when Out_Stream =>
         raise Status_Error;
      end case;
   end Read;

   procedure Write
     (Stream : in out Stream_Type;
      Item   : in Stream_Element_Array)
   is begin
      case Stream.Direction is
      when In_Stream =>
         raise Status_Error;
      when Out_Stream =>
         declare
            Remaining : constant Stream_Element_Offset := Stream.Raw'Last - Stream.Last;
         begin
            if Remaining >= Item'Length then
               Stream.Raw (Stream.Last + 1 .. Stream.Last + Item'Length) := Item;
               Stream.Last := Stream.Last + Item'Length;
            else
               raise End_Error;
            end if;
         end;
      end case;
   end Write;

end SAL.Memory_Streams.Bounded;
