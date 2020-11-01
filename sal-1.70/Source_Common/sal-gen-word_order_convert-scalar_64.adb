-- Abstract :
--
-- See spec.
--
with SAL.Endianness; use SAL.Endianness;
with Ada.Unchecked_Conversion;
package body SAL.Gen.Word_Order_Convert.Scalar_64 is

   function To_4_Word is new Ada.Unchecked_Conversion
      (Target => Bus_Word_Array_4_Type,
       Source => Host_64_Type);

   function From_4_Word is new Ada.Unchecked_Conversion
      (Target => Host_64_Type,
       Source => Bus_Word_Array_4_Type);

   procedure To_Bus
      (Item   : in     Host_64_Type;
       Buffer :    out Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type)
   is begin
      case Byte_Order is
      when Big_Endian =>
         Buffer (Last + 1 .. Last + 4) := To_4_Word (Item);
      when Little_Endian =>
         declare
            Temp : constant Bus_Word_Array_Type (1 .. 4) := To_4_Word (Item);
         begin
            Buffer (Last + 1) := Temp (4);
            Buffer (Last + 2) := Temp (3);
            Buffer (Last + 3) := Temp (2);
            Buffer (Last + 4) := Temp (1);
         end;
      end case;
      Last := Last + 4;
   end To_Bus;

   procedure From_Bus
      (Item   :    out Host_64_Type;
       Buffer : in     Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type)
   is begin
      case Byte_Order is
      when Big_Endian =>
         Item := From_4_Word (Buffer (Last + 1 .. Last + 4));
      when Little_Endian =>
         declare
            Temp : Bus_Word_Array_Type (1 .. 4);
         begin
            Temp (1) := Buffer (Last + 4);
            Temp (2) := Buffer (Last + 3);
            Temp (3) := Buffer (Last + 2);
            Temp (4) := Buffer (Last + 1);
            Item := From_4_Word (Temp);
         end;
      end case;
      Last := Last + 4;
   end From_Bus;

end SAL.Gen.Word_Order_Convert.Scalar_64;
