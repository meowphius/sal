-- Abstract:
--
-- see spec
--
package body SAL.Simple.Searches.Binary is

   function Middle (First, Last : in Index_Type) return Index_Type;
   pragma Inline (Middle);

   function Middle (First, Last : in Index_Type) return Index_Type
   is begin
      return Index_Type'Val (Index_Type'Pos (First) + (Index_Type'Pos (Last) - Index_Type'Pos (First)) / 2);
   end Middle;

   function Search_Less_Equal
      (List : in List_Type;
       Item : in Item_Type)
       return Index_Type
   is
      First : Index_Type := Binary.First (List);
      Last : Index_Type := Binary.Last (List);
      Middle : Index_Type := Binary.Middle (First, Last);
   begin
      loop
         if Middle = First then
            -- done searching; check for success
            if Less_Equal (List, Middle, Item) then
               return Middle;
            else
               raise SAL.Domain_Error;
            end if;
         else
            -- keep searching
            if Less_Equal (List, Middle, Item) then
               First := Middle;
            else
               Last := Middle;
            end if;
            Middle := Binary.Middle (First, Last);
         end if;
      end loop;
   end Search_Less_Equal;

end SAL.Simple.Searches.Binary;
