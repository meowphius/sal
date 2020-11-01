-- Abstract:
--
-- Root of monotonic (and thus invertable) tables.
--
-- See child packages for different orders of interpolation.
--
generic
package SAL.Simple.Function_Tables.Monotonic is

   type Function_Table_Type is abstract new Function_Tables.Function_Table_Type with private;

   ---------------
   -- Override Function_Table_Type operations

   procedure Initialize (Function_Table : in out Function_Table_Type);
   -- Add restriction on Range_Values.
   --
   -- Raises Initialization_Error if Range_Values are not monotonically
   -- increasing or decreasing.

   ------------
   -- New operations

   function Compute_Inverse
      (Function_Table : in Function_Table_Type;
       Range_Value    : in Range_Type)
      return Domain_Type
      is abstract;
   -- Compute inverse function result.
   --
   -- Raises Range_Error if Range_Value not in table range.
   -- Raises Initialization_Error if Function_Table not Initialized.
private

   type Function_Table_Type is abstract new Function_Tables.Function_Table_Type with record
      Increasing_Range : Boolean;
      Range_Max        : Range_Type;
      Range_Min        : Range_Type;
   end record;

end SAL.Simple.Function_Tables.Monotonic;
