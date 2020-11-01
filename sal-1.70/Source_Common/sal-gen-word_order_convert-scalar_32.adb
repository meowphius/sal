-- Abstract :
--
-- See spec.
--
with SAL.Endianness; use SAL.Endianness;
with Ada.Unchecked_Conversion;
package body SAL.Gen.Word_Order_Convert.Scalar_32 is

   function To_2_Word is new Ada.Unchecked_Conversion
      (Target => Bus_Word_Array_2_Type,
       Source => Host_32_Type);

   function From_2_Word is new Ada.Unchecked_Conversion
      (Target => Host_32_Type,
       Source => Bus_Word_Array_2_Type);

   procedure To_Bus
      (Item   : in     Host_32_Type;
       Buffer :    out Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type)
   is begin
      case Byte_Order is
      when Big_Endian =>
         Buffer (Last + 1 .. Last + 2) := To_2_Word (Item);
      when Little_Endian =>
         declare
            Temp : constant Bus_Word_Array_Type (1 .. 2) := To_2_Word (Item);
         begin
            Buffer (Last + 1) := Temp (2);
            Buffer (Last + 2) := Temp (1);
         end;
      end case;
      Last := Last + 2;
   end To_Bus;

   procedure From_Bus
      (Item   :    out Host_32_Type;
       Buffer : in     Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type)
   is begin
      case Byte_Order is
      when Big_Endian =>
         Item := From_2_Word (Buffer (Last + 1 .. Last + 2));
      when Little_Endian =>
         declare
            Temp : Bus_Word_Array_Type (1 .. 2);
         begin
            Temp (1) := Buffer (Last + 2);
            Temp (2) := Buffer (Last + 1);
            Item := From_2_Word (Temp);
         end;
      end case;
      Last := Last + 2;
   end From_Bus;

end SAL.Gen.Word_Order_Convert.Scalar_32;
