--  Abstract:
--
--  Types and operations common to all 6-joint manipulators.
--

with SAL.Gen_Math.Gen_Manipulator;
with SAL.Math_Float.Den_Hart;
with SAL.Math_Float.DOF_3;
with SAL.Math_Float.DOF_6.DC_Array_DCV_Inverse;
with SAL.Math_Float.Elementary;
with SAL.Math_Float.Scalar;
package SAL.Math_Float_Manipulator_6 is

   type Joint_Index_Type is range 1 .. 6;

   package Math is new SAL.Math_Float.Gen_Manipulator
     (Elementary                      => SAL.Math_Float.Elementary,
      Math_Scalar                     => SAL.Math_Float.Scalar,
      Math_Dof_3                      => SAL.Math_Float.DOF_3,
      Math_DOF_6                      => SAL.Math_Float.DOF_6,
      Math_Den_Hart                   => SAL.Math_Float.Den_Hart,
      Math_DOF_6_DC_Array_DCV_Inverse => SAL.Math_Float.DOF_6.DC_Array_DCV_Inverse,
      Joint_Index_Type                => Joint_Index_Type);

end SAL.Math_Float_Manipulator_6;
