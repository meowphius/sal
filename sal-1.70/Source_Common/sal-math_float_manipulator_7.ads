--  Abstract:
--
--  Types and operations common to all 7-joint manipulators.
--

with SAL.Gen_Math.Gen_Manipulator;
with SAL.Math_Float.Den_Hart;
with SAL.Math_Float.DOF_3;
with SAL.Math_Float.DOF_6.DC_Array_DCV_Inverse;
with SAL.Math_Float.Elementary;
with SAL.Math_Float.Scalar;
package SAL.Math_Float_Manipulator_7 is

   type Joint_Index_Type is range 1 .. 7;

   package Math is new SAL.Math_Float.Gen_Manipulator
     (Elementary                      => SAL.Math_Float.Elementary,
      Math_Scalar                     => SAL.Math_Float.Scalar,
      Math_Dof_3                      => SAL.Math_Float.DOF_3,
      Math_DOF_6                      => SAL.Math_Float.DOF_6,
      Math_Den_Hart                   => SAL.Math_Float.Den_Hart,
      Math_DOF_6_DC_Array_DCV_Inverse => SAL.Math_Float.DOF_6.DC_Array_DCV_Inverse,
      Joint_Index_Type                => Joint_Index_Type);

   function "&"
     (Left  : in SAL.Math_Float.DOF_6.Dual_Cart_Vector_Type;
      Right : in SAL.Math_Float.Real_Type)
     return Math.Joint_Array_Real_Type;
   --  Concatenate a Cartesian vector with a redundant scalar to form
   --  an extended vector.

   function Cart (Right : in Math.Joint_Array_Real_Type) return SAL.Math_Float.DOF_6.Dual_Cart_Vector_Type;
   --  Extract the Cartesian part of an extended vector.

   function Extend_Jacobian
     (Jacobian              : in Math.Jacobian_Type;
      Redundant_Joint_Index : in Joint_Index_Type   := 1)
     return Math.Joint_Array_JAR_Type;
   --  Extend a 6x7 Jacobian into a 7x7 Jacobian by declaring the
   --  redundant joint index to be the 7th degree of freedom.
end SAL.Math_Float_Manipulator_7;
