-- Abstract:
--
-- Scalar types and operations, in addition to Ada.Numerics.Generic_Elementary_Functions.
--
-- References:
--
-- [1]  Handbook of Mathematical Functions
--      U.S. Department of Commerce, National Bureau of Standards
--      Applied Mathematics Series 55, 1967, Library of Congress Number: 64-60036
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
with Ada.Numerics.Generic_Elementary_Functions;
generic
   --  Auto_Text_IO : ignore
   with package Elementary is new Ada.Numerics.Generic_Elementary_Functions (Real_Type);
package SAL.Gen_Math.Gen_Scalar is
   pragma Pure;

   e : constant := 2.7182_81828_45904_52353_60287;     -- [1]

   function Modulo (Dividend, Divisor : in Real_Type) return Real_Type;
   function "mod" (Dividend, Divisor : in Real_Type) return Real_Type renames Modulo;
   -- Modulus operation for type Real_Type analogous to Mod for type INTEGER.
   -- Note the definition for Mod of negative numbers as illustrated by
   -- the ADA reference manual applies also to type Real_Type.
   --
   -- raises Constraint_Error if Dividend / Divisor does.

   function Dead_Band
      (Item        : in Real_Type;
       Lower_Limit : in Real_Type)
       return Real_Type;
   --  return Item dead-banded by |Lower_Limit|.
   --
   --                 output
   --                   |   /
   --               ____|__/_ input
   --                /  |
   --               /   |

   function Detent
      (Item        : in Real_Type;
       Dead_Band   : in Real_Type;
       Upper_Limit : in Real_Type)
       return Real_Type;
   --  Smooth, dead-banded version of Sign.
   --  Returns:
   --    0.0 for |Item| < |Dead_Band|,
   --  +-1.0 for |Item| > |Upper_Limit|,
   --  linear interpolation for |Dead_Band| < |Item| < |Upper_Limit|
   --
   --  Dead_Band, Upper_Limit are assumed >= 0.0.
   --
   --                 Detent
   --                   |   ____+1
   --               ____|__/_______ input
   --         -1  ___/  |
   --                   |

   -----------
   -- Limits operations

   --  Auto_Text_IO : separate - Get checks High > Low
   type Limit_Type is private;
   --  Private to enforce High > Low

   function To_Limit (Low, High : in Real_Type) return Limit_Type;
   --  Raises Invalid_Limit if Low >= High.

   function High (Item : in Limit_Type) return Real_Type;
   function Low (Item : in Limit_Type) return Real_Type;
   pragma Inline (High, Low);

   function "and" (Left, Right : in Limit_Type) return Limit_Type;
   --  Return the intersection of Left and Right, if it is non-null.
   --
   --  Raises Invalid_Limit if there is no intersection; for example if
   --  Left.High < Right.Low

   procedure Clip
      (Item    : in out Real_Type;
       Limit   : in     Limit_Type;
       Clipped :    out Boolean);
   --  Item is clipped so that Limit.Low <= Item <= Limit.High. Clipped is
   --  TRUE if Item is changed, FALSE otherwise.

   function "<="
      (Item   : in     Real_Type;
       Limit  : in     Limit_Type)
       return Boolean;
   --  TRUE if Limit.Low <= Item <= Limit.High, FALSE otherwise.

   --------------
   --  Trig operations

   Pi : constant := 3.1415_92653_58979_32384_62643; -- [1]

   function First_Order_Trig return Real_Type;
   pragma Inline (First_Order_Trig);
   --  = Sqrt (Real_Type'Epsilon)
   --  A linear approximation is used for some operations when Trig.Sin < First_Order_Trig.

   --  Auto_Text_IO : separate - Put, Get as single radians value
   type Trig_Pair_Type is private;
   --  Sin and Cos of an angle. Private to enforce sin**2 + cos**2 = 1.

   function Sin_Cos (Angle : in Real_Type) return Trig_Pair_Type;
   --  Compute Sin and Cos - possibly faster than separately

   function Atan2 (Trig : in Trig_Pair_Type) return Real_Type;
   pragma Inline (Atan2);
   --  Return the angle in radians, range - pi < angle <= + pi.
   --
   --  Raises Ada.Numerics.Argument_Error if Trig has both elements 0.0, which can only happen
   --  thru abuse of Unchecked_Trig_Pair.

   function Sin (Trig : in Trig_Pair_Type) return Real_Type;
   function Cos (Trig : in Trig_Pair_Type) return Real_Type;
   pragma Inline (Sin, Cos);
   --  Return the indicated part

   function To_Trig_Pair (Sin, Cos : in Real_Type) return Trig_Pair_Type;
   --  Normalize so that Sin ** 2 + Cos ** 2 = 1.
   --
   --  Raises Non_Normalizable_Trig_Pair if Sin ** 2 + Cos ** 2 = 0.0

   function Unchecked_Trig_Pair (Sin, Cos : in Real_Type) return Trig_Pair_Type;
   --  Performs no normalization; for use in situations where the
   --  algorithm guarantees trig identity.

   pragma Inline (Unchecked_Trig_Pair);

   function "+" (Left : in Trig_Pair_Type; Right : in Trig_Pair_Type) return Trig_Pair_Type;
   --  Find Sin_Cos {Left_Angle + Right_Angle} from Sin_Cos {Left_Angle}, Sin_Cos {Right_Angle}

   function "-" (Left : in Trig_Pair_Type; Right : in Trig_Pair_Type) return Trig_Pair_Type;
   --  Find Sin_Cos {Left_Angle - Right_Angle} from Sin_Cos {Left_Angle}, Sin_Cos {Right_Angle}

   function Half_Trig (Trig : in Trig_Pair_Type) return Trig_Pair_Type;
   --  Find Cos{angle/2}, Sin{angle/2} from Sin{angle}, Cos{angle}

   function Double_Trig (Trig : in Trig_Pair_Type) return Trig_Pair_Type;
   --  Find Cos{2*angle}, Sin{2*angle} from Sin{angle}, Cos{angle}

private

   type Limit_Type is record
      Low  : Real_Type;
      High : Real_Type;
   end record;

   type Trig_Pair_Type is
   record
      Sin : Real_Type;
      Cos : Real_Type;
   end record;

end SAL.Gen_Math.Gen_Scalar;
