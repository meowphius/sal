--  Abstract:
--
--  Nominal geometry and mass parameters for a Kraft Hand Controller.
--
--  Design Decisions:
--
--  Provided here for use with SAL.Math_Float_Manipulator_6. A
--  separate Kraft_HC package provides control of the manipulator.
--

with SAL.Math_Float_Manipulator_6;
package SAL.Math_Float_Kraft_HC_Nominal is

   function Geometry return SAL.Math_Float_Manipulator_6.Math.Joint_Array_Den_Hart_Type;

end SAL.Math_Float_Kraft_HC_Nominal;
