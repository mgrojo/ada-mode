--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018, 2020 - 2022 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body WisiToken.Parse.Packrat is

   function Image_Pos
     (Tree    : in Syntax_Trees.Tree;
      Element : in Syntax_Trees.Stream_Index)
     return String
   is
      use Syntax_Trees;
   begin
      if Element = Invalid_Stream_Index then
         return "0";
      else
         return Tree.Get_Node_Index (Tree.Shared_Stream, Element)'Image;
      end if;
   end Image_Pos;

   function Image (Item : in Memo_Entry; Tree : in Syntax_Trees.Tree) return String
   is begin
      return
        (case Item.State is
         when No_Result => "",
         when Failure => "fail @" & Image_Pos (Tree, Item.Max_Examined_Pos),
         when Success => Tree.Image (Item.Result, Node_Numbers => True) &
           " @" & Image_Pos (Tree, Item.Max_Examined_Pos) &
           "," & Image_Pos (Tree, Item.Last_Pos));
   end Image;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      Pos       : in Syntax_Trees.Node_Index;
      Tree      : in Syntax_Trees.Tree)
     return String
   is
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
   begin
      return
        Syntax_Trees.Trimmed_Image (Pos) & ", " &
        (case Item.State is
         when No_Result => "",
         when Failure => Image (Nonterm, Descriptor) & " fail @" &
           Image_Pos (Tree, Item.Max_Examined_Pos),
         when Success => Tree.Image (Item.Result, Node_Numbers => True, RHS_Index => True) &
           "," & Image_Pos (Tree, Item.Max_Examined_Pos) &
           "," & Image_Pos (Tree, Item.Last_Pos));
   end Image;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      RHS_Index : in Natural;
      Pos       : in Syntax_Trees.Node_Index;
      Tree      : in Syntax_Trees.Tree)
     return String
   is
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
   begin
      return
        Pos'Image & ", " &
        (case Item.State is
         when No_Result => "",
         when Failure => Image (Production_ID'(Nonterm, RHS_Index), Descriptor) & " fail @" &
           Image_Pos (Tree, Item.Max_Examined_Pos),
         when Success => Tree.Image (Item.Result, Node_Numbers => True, RHS_Index => True) &
           "," & Image_Pos (Tree, Item.Max_Examined_Pos) &
           "," & Image_Pos (Tree, Item.Last_Pos));
   end Image;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      Pos       : in Syntax_Trees.Stream_Index;
      Tree      : in Syntax_Trees.Tree)
     return String
   is begin
      return Image (Item, Nonterm, Tree.Get_Node_Index (Tree.Shared_Stream, Pos), Tree);
   end Image;

   function Image
     (Item      : in Memo_Entry;
      Nonterm   : in Token_ID;
      RHS_Index : in Natural;
      Pos       : in Syntax_Trees.Stream_Index;
      Tree      : in Syntax_Trees.Tree)
     return String
   is begin
      return Image (Item, Nonterm, RHS_Index, Tree.Get_Node_Index (Tree.Shared_Stream, Pos), Tree);
   end Image;

   procedure Clear (Derivs : in out Packrat.Derivs)
   is begin
      for D of Derivs loop
         D.Clear (Free_Memory => True);
      end loop;
   end Clear;

   function Get_Deriv
     (Derivs  : in out Packrat.Derivs;
      Nonterm : in     Token_ID;
      Pos     : in     Positive_Node_Index)
     return Memo_Entry
   is begin
      if Pos in Derivs (Nonterm).First_Index .. Derivs (Nonterm).Last_Index then
         return Derivs (Nonterm)(Pos);
      else
         return No_Result_Memo;
      end if;
   end Get_Deriv;

   procedure Set_Deriv
     (Derivs  : in out Packrat.Derivs;
      Nonterm : in     Token_ID;
      Pos     : in     Positive_Node_Index;
      Memo    : in     Memo_Entry)
   is
      use all type WisiToken.Syntax_Trees.Node_Index;
   begin
      if Pos < Derivs (Nonterm).First_Index then
         Derivs (Nonterm).Set_First_Last (Pos, Derivs (Nonterm).Last_Index);

      elsif Pos > Derivs (Nonterm).Last_Index then
         Derivs (Nonterm).Set_First_Last (Derivs (Nonterm).First_Index, Pos);
      end if;

      Derivs (Nonterm).Replace_Element (Pos, Memo);
   end Set_Deriv;

end WisiToken.Parse.Packrat;
