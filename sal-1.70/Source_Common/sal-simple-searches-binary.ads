-- Abstract:
--
-- Generic utility for binary searching
--
generic
   type Item_Type (<>) is limited private;
   type List_Type (<>) is limited private;
   type Index_Type is (<>);
   with function First (List : in List_Type) return Index_Type;
   with function Last (List : in List_Type) return Index_Type;
   with function Less_Equal (List : in List_Type; Left : in Index_Type; Right : in Item_Type) return Boolean;
   -- Return List (Left) <= Right.
package SAL.Simple.Searches.Binary is
   pragma Pure;

   function Search_Less_Equal
      (List : in List_Type;
       Item : in Item_Type)
      return Index_Type;
   -- Return Index of largest item in List less than or equal to Item.
   --
   -- Raises SAL.Domain_Error if no such item exists.

end SAL.Simple.Searches.Binary;
