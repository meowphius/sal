-- Abstract:
--
-- Generic plain Hexadecimal image
--
generic
   Width : Natural;
   type Number_Type is mod <>;
function SAL.Generic_Hex_Image (Item : in Number_Type) return String;
-- Return a hexadecimal image of Item, padded with leading zeros to Width.
-- If Width is too small for Item, leading digits are silently truncated.
pragma Pure (SAL.Generic_Hex_Image);
