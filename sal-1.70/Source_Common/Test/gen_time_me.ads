--  Abstract :
--
--  Generic function for timing stuff.
--
--  Copyright (C) 2002 Stephen Leake.  All Rights Reserved.
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
--  Historical interest :
--
--  29 Mar 1989 Stephe Leake Created

generic
   with procedure Time_Me;
function Gen_Time_Me (Iterations : in Natural) return Duration;
--  Time function Time_Me in loop for Iterations; returns total time
--  in seconds. In order to get good statistics, the body may run
--  Time_Me more than Iterations times (before timing starts). To get
--  consistent results, Time_Me should do exactly the same thing each
--  time it is run.
--
--  Exceptions raised by Time_Me are propagated up. To time a routine
--  that raises an exception, you must encapsulate it in one that
--  handles the exception.
