-- Abstract:
--
-- Root of generic utilities for encoding functions as tables.
--
-- Monotonically increasing Domain_Values allows binary search in Compute.
--
-- See child packages for various compute options.
with Ada.Finalization;
generic
   type Domain_Type is digits <>;
   type Range_Type is digits <>;
package SAL.Simple.Function_Tables is

   type Function_Point_Type is record
      Domain_Value : Domain_Type;
      Range_Value  : Range_Type;
      -- could be just 'domain' and 'range', but 'range' is a reserved word.
   end record;

   type Table_Type is array (Integer range <>) of Function_Point_Type;

   type Table_Access_Type is access constant Table_Type;

   type Function_Table_Type (Table : Table_Access_Type) is abstract new Ada.Finalization.Limited_Controlled
      with private;

   -----------
   -- Dispatching operations on Function_Table_Type.

   procedure Initialize (Function_Table : in out Function_Table_Type);
   --
   -- Raises Initialization_Error if Domain_Values are not monotonically
   -- increasing.

   function Compute
      (Function_Table : in Function_Table_Type;
       Domain_Value   : in Domain_Type)
      return Range_Type
      is abstract;
   -- Compute function result. See child packages for specific lookup
   -- methods.
   --
   -- Raises Domain_Error if Domain_Value not in table domain.

private
   type Function_Table_Type (Table : Table_Access_Type) is
      abstract new Ada.Finalization.Limited_Controlled with
   record
      Domain_Max : Domain_Type;
      Domain_Min : Domain_Type;
   end record;
end SAL.Simple.Function_Tables;
