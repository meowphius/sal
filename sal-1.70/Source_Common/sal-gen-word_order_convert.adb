-- Abstract :
--
-- See spec.
--
with SAL.Endianness; use SAL.Endianness;
with Ada.Unchecked_Conversion;
package body SAL.Gen.Word_Order_Convert is

   function To_1_Word is new Ada.Unchecked_Conversion
      (Target => Bus_Word_Type,
       Source => Interfaces.Integer_16);

   function From_1_Word is new Ada.Unchecked_Conversion
      (Target => Interfaces.Integer_16,
       Source => Bus_Word_Type);

   function To_1_Word is new Ada.Unchecked_Conversion
      (Target => Bus_Word_Type,
       Source => Interfaces.Unsigned_16);

   function From_1_Word is new Ada.Unchecked_Conversion
      (Target => Interfaces.Unsigned_16,
       Source => Bus_Word_Type);

   function To_2_Word is new Ada.Unchecked_Conversion
      (Target => Bus_Word_Array_2_Type,
       Source => Interfaces.Integer_32);

   function From_2_Word is new Ada.Unchecked_Conversion
      (Target => Interfaces.Integer_32,
       Source => Bus_Word_Array_2_Type);

   function To_2_Word is new Ada.Unchecked_Conversion
      (Target => Bus_Word_Array_2_Type,
       Source => Interfaces.Unsigned_32);

   function From_2_Word is new Ada.Unchecked_Conversion
      (Target => Interfaces.Unsigned_32,
       Source => Bus_Word_Array_2_Type);

   procedure To_Bus
      (Item   : in     Bus_Word_Type;
       Buffer :    out Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type)
   is begin
      Last := Last + 1;
      Buffer (Last) := Item;
   end To_Bus;

   procedure From_Bus
      (Item   :    out Bus_Word_Type;
       Buffer : in     Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type)
   is
   begin
      Last := Last + 1;
      Item := Buffer (Last);
   end From_Bus;

   procedure To_Bus
      (Item   : in     Interfaces.Unsigned_16;
       Buffer :    out Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type)
   is begin
      Last := Last + 1;
      Buffer (Last) := To_1_Word (Item);
   end To_Bus;

   procedure From_Bus
      (Item   :    out Interfaces.Integer_16;
       Buffer : in     Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type)
   is
   begin
      Last := Last + 1;
      Item := From_1_Word (Buffer (Last));
   end From_Bus;

   procedure To_Bus
      (Item   : in     Interfaces.Integer_16;
       Buffer :    out Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type)
   is begin
      Last := Last + 1;
      Buffer (Last) := To_1_Word (Item);
   end To_Bus;

   procedure From_Bus
      (Item   :    out Interfaces.Unsigned_16;
       Buffer : in     Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type)
   is
   begin
      Last := Last + 1;
      Item := From_1_Word (Buffer (Last));
   end From_Bus;

   procedure To_Bus
      (Item   : in     Interfaces.Integer_32;
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
      (Item   :    out Interfaces.Integer_32;
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

   procedure To_Bus
      (Item   : in     Interfaces.Unsigned_32;
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
      (Item   :    out Interfaces.Unsigned_32;
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

end SAL.Gen.Word_Order_Convert;
