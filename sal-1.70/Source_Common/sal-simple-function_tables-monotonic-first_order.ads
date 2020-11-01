-- Abstract:
--
-- First order lookup for monotonic (and thus invertable) tables
--
generic
package SAL.Simple.Function_Tables.Monotonic.First_Order is

   type Function_Table_Type is new Function_Tables.Monotonic.Function_Table_Type with null record;

   ---------------
   -- Override Function_Table_Type operations

   function Compute
      (Function_Table : in Function_Table_Type;
       Domain_Value   : in Domain_Type)
      return Range_Type;

   function Compute_Inverse
      (Function_Table : in Function_Table_Type;
       Range_Value    : in Range_Type)
      return Domain_Type;
   -- Compute inverse function result, using a first order interpolation.
   --
   -- Raises Range_Error if Range_Value not in table range.
   -- Raises Initialization_Error if Function_Table not Initialized.

   procedure Initialize (Function_Table : in out Function_Table_Type);
   --  WORKAROUND: GNAT 3.16a added a warning. This just calls
   --  Monotonic.Initialize; lets GNAT 3.16a know that Function_Table
   --  is initialized, suppressing some warnings.

end SAL.Simple.Function_Tables.Monotonic.First_Order;
