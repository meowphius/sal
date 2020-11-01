--  Abstract :
--
--  Dummy unit that 'withs' all SAL Text_IO units, so the compiler will
--  compile them all. See all_sal.adb for main packages.
--
--  Listed in alphabetical order.
pragma Warnings (Off); -- all are unreferenced.
with SAL.Gen_Array_Text_IO;
with SAL.Gen_Math.Gen_Den_Hart.Gen_Text_IO;
with SAL.Gen_Math.Gen_DOF_2.Gen_Text_IO;
with SAL.Gen_Math.Gen_DOF_3.Gen_Text_IO;
with SAL.Gen_Math.Gen_DOF_6.Gen_Text_IO;
with SAL.Gen_Math.Gen_Manipulator.Gen_Text_IO;
with SAL.Gen_Math.Gen_Scalar.Gen_Text_IO;

with SAL.Math_Double.Den_Hart.Text_IO;
with SAL.Math_Double.DOF_2.Text_IO;
with SAL.Math_Double.DOF_3.Text_IO;
with SAL.Math_Double.DOF_6.Text_IO;
with SAL.Math_Double.Polynomials.Text_IO;
with SAL.Math_Double.Scalar.Text_IO;
with SAL.Math_Double.Text_IO;

with SAL.Math_Float.Den_Hart.Text_IO;
with SAL.Math_Float.DOF_2.Text_IO;
with SAL.Math_Float.DOF_3.Text_IO;
with SAL.Math_Float.DOF_6.Text_IO;
with SAL.Math_Float.Polynomials.Text_IO;
with SAL.Math_Float.Scalar.Text_IO;
with SAL.Math_Float.Text_IO;
with SAL.Math_Float_Manipulator_6.Text_IO;
with SAL.Math_Float_Manipulator_7.Text_IO;
pragma Warnings (On);

procedure All_SAL_Text_IO
is
begin
   null;
end All_SAL_Text_IO;
