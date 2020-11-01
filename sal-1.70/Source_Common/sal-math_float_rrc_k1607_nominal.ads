--  Abstract:
--
--  Nominal geometry and mass parameters for an RRC K1607.
--
--  Design Decisions:
--
--  Provided here for use with SAL.Math_Float.Manipulator_7.
--
--  See body for coordinate frame definitions.

with SAL.Math_Float.DOF_6;
with SAL.Math_Float_Manipulator_7;
package SAL.Math_Float_RRC_K1607_Nominal is

   function Geometry return SAL.Math_Float_Manipulator_7.Math.Joint_Array_Den_Hart_Type;
   function Tlast_T_Tp return SAL.Math_Float.DOF_6.Pose_Type;
   function Mass return SAL.Math_Float_Manipulator_7.Math.Joint_Array_Mass_Type;

   function Is_Singular (Position : in SAL.Math_Float_Manipulator_7.Math.Joint_Array_Real_Type) return Boolean;
   --  Assuming joint 1 is specified to resolve redundancy, return
   --  True if Position is close to a singular position. See body
   --  for definition of `close'.

end SAL.Math_Float_RRC_K1607_Nominal;
