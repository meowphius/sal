--  Abstract :
--
--  Text_IO for Math_Float_Manipulator_7
--
--  Copyright (C) 2002, 2003 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--

with SAL.Math_Float.Text_IO;
with SAL.Math_Float.Scalar.Text_IO;
with SAL.Math_Float.DOF_3.Text_IO;
with SAL.Math_Float.DOF_6.Text_IO;
with SAL.Math_Float.Den_Hart.Text_IO;
with SAL.Gen_Math.Gen_Manipulator.Gen_Text_IO;
package SAL.Math_Float_Manipulator_7.Text_IO is new SAL.Math_Float_Manipulator_7.Math.Gen_Text_IO
  (Math_Text_IO          => SAL.Math_Float.Text_IO,
   Math_Scalar_Text_IO   => SAL.Math_Float.Scalar.Text_IO,
   Math_DOF_3_Text_IO    => SAL.Math_Float.DOF_3.Text_IO,
   Math_DOF_6_Text_IO    => SAL.Math_Float.DOF_6.Text_IO,
   Math_Den_Hart_Text_IO => SAL.Math_Float.Den_Hart.Text_IO);

