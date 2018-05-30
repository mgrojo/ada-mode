--  Abstract :
--
--  see spec
--
--  Copyright (C) 2013, 2014, 2015, 2017, 2018 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

package body WisiToken.Wisi_Ada is
   use WisiToken.Productions;

   function Only (Item : in Token_ID) return WisiToken.Productions.Token_ID_Lists.List
   is begin
      return List : WisiToken.Productions.Token_ID_Lists.List do
         List.Append (Item);
      end return;
   end Only;

   function "&" (Left : in Token_ID; Right : in Token_ID) return WisiToken.Productions.Token_ID_Lists.List
   is begin
      return Result : WisiToken.Productions.Token_ID_Lists.List do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "&";

   function "+" (Tokens : in Token_ID_Lists.List; Action : in Syntax_Trees.Semantic_Action) return Right_Hand_Side
   is begin
      return (Tokens, Action, null, 0);
   end "+";

   function "+" (Tokens : in Token_ID; Action : in Syntax_Trees.Semantic_Action) return Right_Hand_Side
   is begin
      return (Only (Tokens), Action, null, 0);
   end "+";

   function "+" (Action : in Syntax_Trees.Semantic_Action) return Right_Hand_Side
   is begin
      return (Token_ID_Lists.Empty_List, Action, null, 0);
   end "+";

   function "+" (Tokens : in Token_ID_Lists.List; Index  : in Integer) return Right_Hand_Side
   is begin
      return (Tokens, null, null, Index);
   end "+";

   function "+" (Tokens : in Token_ID; Index  : in Integer) return Right_Hand_Side
   is begin
      return (Only (Tokens), null, null, Index);
   end "+";

   function "+" (Index  : in Integer) return Right_Hand_Side
   is begin
      return (Token_ID_Lists.Empty_List, null, null, Index);
   end "+";

   function "<=" (LHS : in Token_ID; RHS : in Right_Hand_Side) return Instance
   is begin
      return (LHS, RHS);
   end "<=";

   function Only (Subject : in Instance) return Arrays.Vector
   is begin
      return Result : Arrays.Vector do
         Result.Append (Subject);
      end return;
   end Only;

   function "and" (Left : in Instance; Right : in Instance) return Arrays.Vector
   is begin
      return Result : Arrays.Vector do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "and";

   function "and" (Left : in Arrays.Vector; Right : in Instance) return Arrays.Vector
   is begin
      return Result : Arrays.Vector := Left do
         Result.Append (Right);
      end return;
   end "and";

   function "and" (Left : in Arrays.Vector; Right : in Arrays.Vector) return Arrays.Vector
   is begin
      return Result : Arrays.Vector := Left do
         for P of Right loop
            Result.Append (P);
         end loop;
      end return;
   end "and";

end WisiToken.Wisi_Ada;
