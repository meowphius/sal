-- Abstract:
--
-- see spec
--
-- References:
--
-- [1] CRC Handbook of Chemistry and Physics, 69th Edition
--     The Chemical Rubber Publishing Co.
--
-- Copyright (C) 2001, 2002, 2003 Stephen Leake.  All Rights Reserved.
--
-- This library is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2, or (at
-- your option) any later version. This library is distributed in the
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even
-- the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE. See the GNU General Public License for more details. You
-- should have received a copy of the GNU General Public License
-- distributed with this program; see file COPYING. If not, write to
-- the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
-- MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this  unit  does not  by itself cause  the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file  might be covered by the  GNU Public License.
--
package body SAL.Gen_Math.Gen_Scalar is

   function Modulo (Dividend, Divisor : in Real_Type) return Real_Type
   is
      -- match names in Ada LRM 4.5.5

      A : Real_Type renames Dividend;
      B : Real_Type renames Divisor;
      N : constant Integer := Integer (Real_Type'Floor (A / B));
   begin
      return A - B * Real_Type (N);
   end Modulo;

   function Dead_Band
      (Item        : in Real_Type;
       Lower_Limit : in Real_Type)
       return Real_Type
   is
   begin
      if abs Item < abs Lower_Limit then
         return 0.0;
      elsif Item >= 0.0 then
         return Item - abs (Lower_Limit);
      else
         return Item + abs (Lower_Limit);
      end if;
   end Dead_Band;

   function Detent
      (Item        : in Real_Type;
       Dead_Band   : in Real_Type;
       Upper_Limit : in Real_Type)
       return Real_Type
   is begin
      if abs Item < Dead_Band then
         return 0.0;
      elsif abs Item >= Upper_Limit then
         if Item >= 0.0 then
            return +1.0;
         else
            return -1.0;
         end if;
      else -- between Dead_Band and Upper_Limit
         if Item >= 0.0 then
            return (abs Item - Dead_Band) / (Upper_Limit - Dead_Band);
         else
            return -(abs Item - Dead_Band) / (Upper_Limit - Dead_Band);
         end if;
      end if;
   end Detent;

   -----------
   -- Limits operations

   function To_Limit (Low, High : in Real_Type) return Limit_Type
   is begin
      if Low >= High then
         raise Invalid_Limit;
      else
         return (Low, High);
      end if;
   end To_Limit;

   function High (Item : in Limit_Type) return Real_Type
   is begin
      return Item.High;
   end High;

   function Low (Item : in Limit_Type) return Real_Type
   is begin
      return Item.Low;
   end Low;

   function "and" (Left, Right : in Limit_Type) return Limit_Type
   is begin
      return To_Limit (Real_Type'Max (Left.Low, Right.Low), Real_Type'Min (Left.High, Right.High));
   end "and";

   procedure Clip
      (Item    : in out Real_Type;
       Limit   : in     Limit_Type;
       Clipped :    out Boolean)
   is
   begin
      if Item > Limit.High then
         Clipped := True;
         Item    := Limit.High;
      elsif Item < Limit.Low then
         Clipped := True;
         Item    := Limit.Low;
      else
         Clipped := False;
      end if;
   end Clip;

   function "<="
      (Item   : in     Real_Type;
       Limit  : in     Limit_Type)
       return Boolean
   is
   begin
      return Limit.Low <= Item and Item <= Limit.High;
   end "<=";

   --------------
   -- Trig operations

   function First_Order_Trig return Real_Type
   is begin
      return Elementary.Sqrt (Real_Type'Model_Epsilon);
   end First_Order_Trig;

   function Sin_Cos (Angle : in Real_Type) return Trig_Pair_Type
   is begin
      return (Elementary.Sin (Angle), Elementary.Cos (Angle));
   end Sin_Cos;

   function Atan2 (Trig : in Trig_Pair_Type) return Real_Type
   is begin
      return Elementary.Arctan (Trig.Sin, Trig.Cos);
   end Atan2;

   function Sin (Trig : in Trig_Pair_Type) return Real_Type
   is begin
      return Trig.Sin;
   end Sin;

   function Cos (Trig : in Trig_Pair_Type) return Real_Type
   is begin
      return Trig.Cos;
   end Cos;

   function To_Trig_Pair (Sin, Cos : in Real_Type) return Trig_Pair_Type
   is
      Mag : constant Real_Type := Elementary.Sqrt (Sin * Sin + Cos * Cos);
   begin
      -- (sin, cos) / Mag is guaranteed < 1.0, so we only need to check
      -- for precisely 0.0.
      if Mag = 0.0 then
         raise Non_Normalizable_Trig_Pair;
      else
         return (Sin / Mag, Cos / Mag);
      end if;
   end To_Trig_Pair;

   function Unchecked_Trig_Pair (Sin, Cos : in Real_Type) return Trig_Pair_Type
   is begin
      return (Sin, Cos);
   end Unchecked_Trig_Pair;

   function "+" (Left : in Trig_Pair_Type; Right : in Trig_Pair_Type) return Trig_Pair_Type
   is begin
      return (Left.Sin * Right.Cos + Left.Cos * Right.Sin, Left.Cos * Right.Cos - Left.Sin * Right.Sin);
   end "+";

   function "-" (Left : in Trig_Pair_Type; Right : in Trig_Pair_Type) return Trig_Pair_Type
   is begin
      return (Left.Sin * Right.Cos - Left.Cos * Right.Sin, Left.Cos * Right.Cos + Left.Sin * Right.Sin);
   end "-";

   function Half_Trig (Trig : in Trig_Pair_Type) return Trig_Pair_Type
   is
      -- The result Trig.Cos is >= 0.0.
      --
      -- A linear approximation is used when Trig.Sin <
      -- First_Order_Trig. this is exact since cos x = 1 - x**2 for
      -- this range of x.
   begin
      if abs Trig.Sin < First_Order_Trig then
         -- angle near 0 or Pi.
         if Trig.Cos > 0.0 then
            -- angle near 0
            return (Trig.Sin / 2.0, 1.0);
         else    -- angle near Pi
            if Trig.Sin >= 0.0 then
               return (1.0 - Trig.Sin / 2.0, 0.0);
            else
               return (-1.0 + Trig.Sin / 2.0, 0.0);
            end if;
         end if;
      else    -- angle not near 0 or Pi
         if Trig.Sin >= 0.0 then
            return (Elementary.Sqrt ((1.0 - Trig.Cos) / 2.0), Elementary.Sqrt ((1.0 + Trig.Cos) / 2.0));
         else
            return (-Elementary.Sqrt ((1.0 - Trig.Cos) / 2.0), Elementary.Sqrt ((1.0 + Trig.Cos) / 2.0));
         end if;
      end if;
   end Half_Trig;

   function Double_Trig (Trig : in Trig_Pair_Type) return Trig_Pair_Type
   is begin
      return (2.0 * Trig.Sin * Trig.Cos, Trig.Cos * Trig.Cos - Trig.Sin * Trig.Sin);
   end Double_Trig;

end SAL.Gen_Math.Gen_Scalar;
