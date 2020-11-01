-- Abstract :
--
-- Provide conversions between 1773 bus order word arrays and scalar types.
--
-- The body assumes that the 1773 interface card handles any required byte
-- swapping within a 16 bit word.
--
-- Design Notes :
--
-- 1773 bus order is little-bit-endian, big-word-endian, 16 bit words.
--
-- The package SAL.Endianness defines the bit and word endianness for the
-- current processor.
--
-- The To_Bus functions copy words from Item to Buffer[Last+1..], changing
-- to bus word order. On return, Last points to the last word written.
--
-- The From_Network functions copy words from Buffer[Last+1..] to Item,
-- changing from bus word order. On return, Last points to the last word
-- written.
--
-- For scalar host types of size 16 or 32 bits, either use a type
-- conversion in a call to one of the functions below, or instantiate
-- one of the child packages provided (to avoid the type conversion).
--
-- We allow Bus_Word_Type to be signed or unsigned, at the user's
-- convenience.
--
with Interfaces;
generic
   type Bus_Word_Type       is (<>);
   type Bus_Index_Type      is range <>;
   type Bus_Word_Array_Type is array (Bus_Index_Type range <>) of Bus_Word_Type;
package SAL.Gen.Word_Order_Convert is
   pragma Pure;

   procedure To_Bus
      (Item   : in     Bus_Word_Type;
       Buffer :    out Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);

   procedure From_Bus
      (Item   :    out Bus_Word_Type;
       Buffer : in     Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);

   procedure To_Bus
      (Item   : in     Interfaces.Integer_16;
       Buffer :    out Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);

   procedure From_Bus
      (Item   :    out Interfaces.Integer_16;
       Buffer : in     Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);

   procedure To_Bus
      (Item   : in     Interfaces.Unsigned_16;
       Buffer :    out Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);

   procedure From_Bus
      (Item   :    out Interfaces.Unsigned_16;
       Buffer : in     Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);


   procedure To_Bus
      (Item   : in     Interfaces.Integer_32;
       Buffer :    out Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);

   procedure From_Bus
      (Item   :    out Interfaces.Integer_32;
       Buffer : in     Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);

   procedure To_Bus
      (Item   : in     Interfaces.Unsigned_32;
       Buffer :    out Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);

   procedure From_Bus
      (Item   :    out Interfaces.Unsigned_32;
       Buffer : in     Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);

private
   -- Visible for child packages, also used in body.
   subtype Bus_Word_Array_2_Type is Bus_Word_Array_Type (1 .. 2);
   subtype Bus_Word_Array_4_Type is Bus_Word_Array_Type (1 .. 4);

end SAL.Gen.Word_Order_Convert;
