-- Abstract :
--
-- Provide conversions between 1773 bus order word arrays and scalar
-- types of size 4 words (64 bits).
--
-- The body assumes that the 1773 interface card handles any required byte
-- swapping within a 16 bit word.
--
-- Host_32_Type is 'private' to allow signed or unsigned integer or
-- fixed or float.
--
generic
   type Host_64_Type is private;
package SAL.Gen.Word_Order_Convert.Scalar_64 is
   pragma Pure;

   procedure To_Bus
      (Item   : in     Host_64_Type;
       Buffer :    out Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);

   procedure From_Bus
      (Item   :    out Host_64_Type;
       Buffer : in     Bus_Word_Array_Type;
       Last   : in out Bus_Index_Type);

end SAL.Gen.Word_Order_Convert.Scalar_64;
