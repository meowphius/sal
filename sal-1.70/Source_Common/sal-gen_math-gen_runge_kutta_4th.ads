--  Abstract :
--
--  Generic Runge-Kutta fourth order integrator
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

generic
   type State_Type is private;
   type Derivative_Type is private;
   with function Compute_Derivative (State : in State_Type) return Derivative_Type;
   with function "*" (Derivative : in Derivative_Type; Time_Step  : in Real_Type) return State_Type is <>;
   with function "+" (Left, Right : in State_Type) return State_Type is <>;

procedure SAL.Gen_Math.Gen_Runge_Kutta_4th (State : in out State_Type; Time_Step : in Real_Type);
pragma Pure (SAL.Gen_Math.Gen_Runge_Kutta_4th);
--  Computes State := State + Derivative (State) * T, but allows
--  Derivative to be non-constant.
--
--  In function terms, this computes F(t+dt) = F(t) + dF/dt * dt. This
--  allows computing F(t) when only F(t_init) and dF/dt are known.

