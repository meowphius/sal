-- Abstract:
--
-- See package spec.
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
package body Gen_Array_Text_IO_83 is

   use Text_IO;

   package body Integer_1D is

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Base              : in Text_IO.Field            := Default_Base;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is
      begin
         Put (File, '(');
         if Item'Length > 1 then
            for I in Index_Type'First .. Index_Type'Pred (Index_Type'Last) loop
               if Named_Association then
                  Put (File, Index_Type'Image (I) & " => ");

                  if not Single_Line then
                     New_Line (File);
                  end if;
               end if;

               Element_Put (File, Item (I), Width, Base);

               if Single_Line then
                  Put (File, ", ");
               else
                  Put_Line (File, ",");
               end if;

            end loop;
         end if;

         if Named_Association then
            Put (File, Index_Type'Image (Index_Type'Last) & " => ");

            if not Single_Line then
               New_Line (File);
            end if;
         end if;

         Element_Put (File, Item (Index_Type'Last), Width, Base);
         Put (File, ')');
      end Put;

      procedure Put
         (Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Base              : in Text_IO.Field            := Default_Base;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is
      begin
         Put (Text_IO.Current_Output, Item, Width, Base, Single_Line, Named_Association);
      end Put;

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (File, Item, Default_Width, Default_Base, Single_Line, Named_Association);
      end Put_Item;

   end Integer_1D;

   package body Unconstrained_Integer_1D is

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Base              : in Text_IO.Field            := Default_Base;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is
      begin
         Put (File, '(');
         if Item'Length > 0 then
            if Item'Length > 1 then
               for I in Item'First .. Index_Type'Pred (Item'Last) loop
                  if Named_Association then
                     Put (File, Index_Type'Image (I) & " => ");

                     if not Single_Line then
                        New_Line (File);
                     end if;
                  end if;

                  Element_Put (File, Item (I), Width, Base);

                  if Single_Line then
                     Put (File, ", ");
                  else
                     Put_Line (File, ",");
                  end if;

               end loop;
            end if;

            if Named_Association then
               Put (File, Index_Type'Image (Item'Last) & " => ");

               if not Single_Line then
                  New_Line (File);
               end if;
            end if;

            Element_Put (File, Item (Item'Last), Width, Base);
         end if;

         Put (File, ')');
      end Put;

      procedure Put
         (Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Base              : in Text_IO.Field            := Default_Base;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is
      begin
         Put (Text_IO.Current_Output, Item, Width, Base, Single_Line, Named_Association);
      end Put;

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (File, Item, Default_Width, Default_Base, Single_Line, Named_Association);
      end Put_Item;

   end Unconstrained_Integer_1D;

   package body Enumeration_1D is

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Setting           : in Text_IO.Type_Set         := Default_Setting;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (File, '(');
         if Item'Length > 1 then
            for I in Index_Type'First .. Index_Type'Pred (Index_Type'Last) loop
               if Named_Association then
                  Put (File, Index_Type'Image (I) & " => ");

                  if not Single_Line then
                     New_Line (File);
                  end if;
               end if;

               Element_Put (File, Item (I), Width, Setting);
               if Single_Line then
                  Put (File, ", ");
               else
                  Put_Line (File, ",");
               end if;

            end loop;
         end if;

         if Named_Association then
            Put (File, Index_Type'Image (Index_Type'Last) & " => ");

            if not Single_Line then
               New_Line (File);
            end if;
         end if;

         Element_Put (File, Item (Index_Type'Last), Width, Setting);
         Put (File, ')');
      end Put;

      procedure Put
         (Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Setting           : in Text_IO.Type_Set         := Default_Setting;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is
      begin
         Put (Text_IO.Current_Output, Item, Width, Setting, Single_Line, Named_Association);
      end Put;

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (File, Item, Default_Width, Default_Setting, Single_Line, Named_Association);
      end Put_Item;

   end Enumeration_1D;

   package body Unconstrained_Enumeration_1D is

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Setting           : in Text_IO.Type_Set         := Default_Setting;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (File, '(');
         if Item'Length > 0 then
            if Item'Length > 1 then
               for I in Item'First .. Index_Type'Pred (Item'Last) loop
                  if Named_Association then
                     Put (File, Index_Type'Image (I) & " => ");

                     if not Single_Line then
                        New_Line (File);
                     end if;
                  end if;

                  Element_Put (File, Item (I), Width, Setting);
                  if Single_Line then
                     Put (File, ", ");
                  else
                     Put_Line (File, ",");
                  end if;

               end loop;
            end if;

            if Named_Association then
               Put (File, Index_Type'Image (Item'Last) & " => ");

               if not Single_Line then
                  New_Line (File);
               end if;
            end if;

            Element_Put (File, Item (Item'Last), Width, Setting);
         end if;

         Put (File, ')');
      end Put;

      procedure Put
         (Item              : in Index_Array_Element_Type;
          Width             : in Text_IO.Field            := Default_Width;
          Setting           : in Text_IO.Type_Set         := Default_Setting;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is
      begin
         Put (Text_IO.Current_Output, Item, Width, Setting, Single_Line, Named_Association);
      end Put;

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (File, Item, Default_Width, Default_Setting, Single_Line, Named_Association);
      end Put_Item;

   end Unconstrained_Enumeration_1D;

   package body Float_1D is

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Fore              : in Text_IO.Field            := Default_Fore;
          Aft               : in Text_IO.Field            := Default_Aft;
          Exp               : in Text_IO.Field            := Default_Exp;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (File, '(');
         if Item'Length > 1 then
            for I in Index_Type'First .. Index_Type'Pred (Index_Type'Last) loop
               if Named_Association then
                  Put (File, Index_Type'Image (I) & " => ");

                  if not Single_Line then
                     New_Line (File);
                  end if;
               end if;

               Element_Put (File, Item (I), Fore, Aft, Exp);

               if Single_Line then
                  Put (File, ", ");
               else
                  Put_Line (File, ",");
               end if;

            end loop;
         end if;

         if Named_Association then
            Put (File, Index_Type'Image (Index_Type'Last) & " => ");

            if not Single_Line then
               New_Line (File);
            end if;
         end if;

         Element_Put (File, Item (Index_Type'Last), Fore, Aft, Exp);
         Put (File, ')');
      end Put;

      procedure Put
         (Item              : in Index_Array_Element_Type;
          Fore              : in Text_IO.Field            := Default_Fore;
          Aft               : in Text_IO.Field            := Default_Aft;
          Exp               : in Text_IO.Field            := Default_Exp;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (Text_IO.Current_Output, Item, Fore, Aft, Exp, Single_Line, Named_Association);
      end Put;

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (File, Item, Default_Fore, Default_Aft, Default_Exp, Single_Line, Named_Association);
      end Put_Item;

   end Float_1D;

   package body Unconstrained_Float_1D is

      procedure Put
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Fore              : in Text_IO.Field            := Default_Fore;
          Aft               : in Text_IO.Field            := Default_Aft;
          Exp               : in Text_IO.Field            := Default_Exp;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (File, '(');
         if Item'Length > 0 then
            if Item'Length > 1 then
               for I in Item'First .. Index_Type'Pred (Item'Last) loop
                  if Named_Association then
                     Put (File, Index_Type'Image (I) & " => ");

                     if not Single_Line then
                        New_Line (File);
                     end if;
                  end if;

                  Element_Put (File, Item (I), Fore, Aft, Exp);

                  if Single_Line then
                     Put (File, ", ");
                  else
                     Put_Line (File, ",");
                  end if;

               end loop;
            end if;

            if Named_Association then
               Put (File, Index_Type'Image (Item'Last) & " => ");

               if not Single_Line then
                  New_Line (File);
               end if;
            end if;

            Element_Put (File, Item (Item'Last), Fore, Aft, Exp);
         end if;

         Put (File, ')');
      end Put;

      procedure Put
         (Item              : in Index_Array_Element_Type;
          Fore              : in Text_IO.Field            := Default_Fore;
          Aft               : in Text_IO.Field            := Default_Aft;
          Exp               : in Text_IO.Field            := Default_Exp;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (Text_IO.Current_Output, Item, Fore, Aft, Exp, Single_Line, Named_Association);
      end Put;

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean                  := Default_Single_Line;
          Named_Association : in Boolean                  := Default_Named_Association)
      is begin
         Put (File, Item, Default_Fore, Default_Aft, Default_Exp, Single_Line, Named_Association);
      end Put_Item;

   end Unconstrained_Float_1D;

   package body Private_1D is

      procedure Put
         (File                      : in Text_IO.File_Type;
          Item                      : in Index_Array_Element_Type;
          Single_Line_Array         : in Boolean := Default_Single_Line_Array;
          Named_Association_Array   : in Boolean := Default_Named_Association_Array;
          Single_Line_Element       : in Boolean := Default_Single_Line_Element;
          Named_Association_Element : in Boolean := Default_Named_Association_Element)
      is begin
         Put (File, '(');

         if Item'Length > 1 then
            for I in Index_Type'First .. Index_Type'Pred (Item'Last) loop
               if Named_Association_Array then
                  Put (File, Index_Type'Image (I) & " => ");

                  if not Single_Line_Array then
                     if not Single_Line_Element then
                        New_Line (File);
                     end if;
                  end if;
               end if;

               Element_Put (File, Item (I), Single_Line_Element, Named_Association_Element);
               if Single_Line_Array then
                  Put (File, ", ");
               else
                  Put_Line (File, ",");
               end if;
            end loop;
         end if;

         if Named_Association_Array then
            Put (File, Index_Type'Image (Index_Type'Last) & " => ");

            if not Single_Line_Array then
               if not Single_Line_Element then
                  New_Line (File);
               end if;
            end if;
         end if;

         Element_Put (File, Item (Index_Type'Last), Single_Line_Element, Named_Association_Element);

         Put (File, ')');
      end Put;

      procedure Put
         (Item                      : in Index_Array_Element_Type;
          Single_Line_Array         : in Boolean := Default_Single_Line_Array;
          Named_Association_Array   : in Boolean := Default_Named_Association_Array;
          Single_Line_Element       : in Boolean := Default_Single_Line_Element;
          Named_Association_Element : in Boolean := Default_Named_Association_Element)
      is begin
         Put
            (Text_IO.Current_Output, Item,
             Single_Line_Array, Named_Association_Array,
             Single_Line_Element, Named_Association_Element);
      end Put;

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean := Default_Single_Line_Array;
          Named_Association : in Boolean := Default_Named_Association_Array)
      is begin
         Put (File, Item, Single_Line, Named_Association,
              Default_Single_Line_Element, Default_Named_Association_Element);
      end Put_Item;

   end Private_1D;

   package body Unconstrained_Private_1D is

      procedure Put
         (File                      : in Text_IO.File_Type;
          Item                      : in Index_Array_Element_Type;
          Single_Line_Array         : in Boolean := Default_Single_Line_Array;
          Named_Association_Array   : in Boolean := Default_Named_Association_Array;
          Single_Line_Element       : in Boolean := Default_Single_Line_Element;
          Named_Association_Element : in Boolean := Default_Named_Association_Element)
      is begin
         Put (File, '(');

         if Item'Length > 0 then
            if Item'Length > 1 then
               for I in Item'First .. Index_Type'Pred (Item'Last) loop
                  if Named_Association_Array then
                     Put (File, Index_Type'Image (I) & " => ");

                     if not Single_Line_Array then
                        if not Single_Line_Element then
                           New_Line (File);
                        end if;
                     end if;
                  end if;

                  Element_Put (File, Item (I), Single_Line_Element, Named_Association_Element);
                  if Single_Line_Array then
                     Put (File, ", ");
                  else
                     Put_Line (File, ",");
                  end if;
               end loop;
            end if;

            if Named_Association_Array then
               Put (File, Index_Type'Image (Item'Last) & " => ");

               if not Single_Line_Array then
                  if not Single_Line_Element then
                     New_Line (File);
                  end if;
               end if;
            end if;

            Element_Put (File, Item (Item'Last), Single_Line_Element, Named_Association_Element);
         end if;

         Put (File, ')');
      end Put;

      procedure Put
         (Item                      : in Index_Array_Element_Type;
          Single_Line_Array         : in Boolean := Default_Single_Line_Array;
          Named_Association_Array   : in Boolean := Default_Named_Association_Array;
          Single_Line_Element       : in Boolean := Default_Single_Line_Element;
          Named_Association_Element : in Boolean := Default_Named_Association_Element)
      is begin
         Put
            (Text_IO.Current_Output, Item, Single_Line_Array, Named_Association_Array,
             Single_Line_Element, Named_Association_Element);
      end Put;

      procedure Put_Item
         (File              : in Text_IO.File_Type;
          Item              : in Index_Array_Element_Type;
          Single_Line       : in Boolean := Default_Single_Line_Array;
          Named_Association : in Boolean := Default_Named_Association_Array)
      is begin
         Put (File, Item, Single_Line, Named_Association,
              Default_Single_Line_Element, Default_Named_Association_Element);
      end Put_Item;

   end Unconstrained_Private_1D;

end Gen_Array_Text_IO_83;
