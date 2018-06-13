--  Abstract :
--
--  Utilities for AUnit tests, including checks for types in Standard.
--
--  Copyright (C) 2004 - 2007, 2009, 2012, 2015, 2018 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

package AUnit.Checks is

   generic
      type Item_Type is (<>);
   procedure Gen_Check_Valid
     (Label    : in     String;
      Computed : access Item_Type);
   --  We need 'access' to avoid the 'Valid check on parameter passing.
   --  You will probably need 'Unrestricted_Access.

   generic
      type Item_Type (<>) is limited private;
      type Item_Access_Type is access all Item_Type;
   procedure Gen_Check_All_Access
     (Label    : in String;
      Computed : in Item_Access_Type;
      Expected : in Item_Access_Type);

   generic
      type Item_Type (<>) is limited private;
      type Item_Access_Type is access Item_Type;
   procedure Gen_Check_Access
     (Label    : in String;
      Computed : in Item_Access_Type;
      Expected : in Item_Access_Type);

   generic
      type Item_Type (<>) is abstract tagged limited private;
   procedure Gen_Check_Access_Class
     (Label    : in     String;
      Computed : access Item_Type'Class;
      Expected : access Item_Type'Class);

   generic
      type Item_Type is (<>);
   procedure Gen_Check_Discrete
     (Label    : in String;
      Computed : in Item_Type;
      Expected : in Item_Type);

   generic
      type Item_Type is range <>;
   procedure Gen_Check_Integer_Tolerance
     (Label     : in String;
      Computed  : in Item_Type;
      Expected  : in Item_Type;
      Tolerance : in Item_Type := 0);

   generic
      type Item_Type is delta <>;
   procedure Gen_Check_Fixed
     (Label     : in String;
      Computed  : in Item_Type;
      Expected  : in Item_Type;
      Tolerance : in Item_Type := 0.0);

   generic
      type Item_Type is private;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type) of Item_Type;
      with procedure Check_Index
        (Label    : in String;
         Computed : in Index_Type;
         Expected : in Index_Type);
      with procedure Check_Item
        (Label    : in String;
         Computed : in Item_Type;
         Expected : in Item_Type);
   procedure Gen_Check_Array
     (Label    : in String;
      Computed : in Array_Type;
      Expected : in Array_Type);

   generic
      type Item_Type is private;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type) of Item_Type;
      type Tolerance_Type is private;

      with procedure Check_Index
        (Label    : in String;
         Computed : in Index_Type;
         Expected : in Index_Type);
      with procedure Check_Item
        (Label     : in String;
         Computed  : in Item_Type;
         Expected  : in Item_Type;
         Tolerance : in Tolerance_Type);

      Default_Tolerance : in out Tolerance_Type;

   procedure Gen_Check_Array_Tolerance
     (Label     : in String;
      Computed  : in Array_Type;
      Expected  : in Array_Type;
      Tolerance : in Tolerance_Type := Default_Tolerance);

   generic
      type Item_Type is private;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Item_Type;
      with procedure Check_Index
        (Label    : in String;
         Computed : in Index_Type;
         Expected : in Index_Type);
      with procedure Check_Item
        (Label    : in String;
         Computed : in Item_Type;
         Expected : in Item_Type);
   procedure Gen_Check_Unconstrained_Array
     (Label          : in String;
      Computed       : in Array_Type;
      Expected       : in Array_Type;
      Strict_Indices : in Boolean    := True);

   generic
      type Item_Type is private;
      type Index_1_Type is (<>);
      type Index_2_Type is (<>);
      type Array_Type is array (Index_1_Type range <>, Index_2_Type range <>) of Item_Type;
      with procedure Check_Item
        (Label    : in String;
         Computed : in Item_Type;
         Expected : in Item_Type);
      with procedure Check_Index_1
        (Label    : in String;
         Computed : in Index_1_Type;
         Expected : in Index_1_Type);
      with procedure Check_Index_2
        (Label    : in String;
         Computed : in Index_2_Type;
         Expected : in Index_2_Type);
   procedure Gen_Check_Unconstrained_2D_Array
     (Label          : in String;
      Computed       : in Array_Type;
      Expected       : in Array_Type;
      Strict_Indices : in Boolean    := True);

   generic
      type Item_Type is private;
      Zero_Item : in Item_Type;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Item_Type;
      with procedure Check_Index
        (Label    : in String;
         Computed : in Index_Type;
         Expected : in Index_Type);
      with procedure Check_Item
        (Label     : in String;
         Computed  : in Item_Type;
         Expected  : in Item_Type;
         Tolerance : in Item_Type);
   procedure Gen_Check_Unconstrained_Array_Tolerance
     (Label          : in String;
      Computed       : in Array_Type;
      Expected       : in Array_Type;
      Tolerance      : in Item_Type  := Zero_Item;
      Strict_Indices : in Boolean    := True);

   procedure Check
     (Label    : in String;
      Computed : in Integer;
      Expected : in Integer);

   procedure Check_Range
     (Label         : in String;
      Computed      : in Integer;
      Expected_Low  : in Integer;
      Expected_High : in Integer);

   procedure Check
     (Label    : in String;
      Computed : in Boolean;
      Expected : in Boolean);

   procedure Check
     (Label    : in String;
      Computed : in String;
      Expected : in String);

   procedure Check
     (Label     : in String;
      Computed  : in Duration;
      Expected  : in Duration;
      Tolerance : in Duration := 0.0);

   procedure Check
     (Label     : in String;
      Computed  : in Float;
      Expected  : in Float;
      Tolerance : in Float := 0.0);

   procedure Check
     (Label     : in String;
      Computed  : in Long_Float;
      Expected  : in Long_Float;
      Tolerance : in Long_Float := 0.0);

end AUnit.Checks;
