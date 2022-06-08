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

with WisiToken.Parse.Parser;
package body WisiToken.Parse.Packrat is

   function Image_Pos
     (Tree    : in Syntax_Trees.Tree;
      Stream  : in Syntax_Trees.Stream_ID;
      Element : in Syntax_Trees.Stream_Index)
     return String
   is
      use Syntax_Trees;
   begin
      if Element = Invalid_Stream_Index then
         return "0";
      else
         return Tree.Get_Node_Index (Stream, Element)'Image;
      end if;
   end Image_Pos;

   function Image (Item : in Memo_Entry; Tree : in Syntax_Trees.Tree) return String
   is begin
      return
        (case Item.State is
         when No_Result => "",
         when Failure => "fail @" & Image_Pos (Tree, Tree.Shared_Stream, Item.Max_Examined_Pos),
         when Success => Tree.Image (Item.Result, Node_Numbers => True) &
           " @" & Image_Pos (Tree, Tree.Shared_Stream, Item.Max_Examined_Pos) &
           "," & Image_Pos (Tree, Tree.Shared_Stream, Item.Last_Pos));
   end Image;

   function Image
     (Item    : in Memo_Entry;
      Nonterm : in Token_ID;
      Pos     : in Syntax_Trees.Node_Index;
      Tree    : in Syntax_Trees.Tree)
     return String
   is
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
   begin
      return
        Pos'Image & ", " &
        (case Item.State is
         when No_Result => "",
         when Failure => Image (Nonterm, Descriptor) & " fail @" &
           Image_Pos (Tree, Tree.Shared_Stream, Item.Max_Examined_Pos),
         when Success => Tree.Image (Item.Result, Node_Numbers => True) &
           "," & Image_Pos (Tree, Tree.Shared_Stream, Item.Max_Examined_Pos) &
           "," & Image_Pos (Tree, Tree.Shared_Stream, Item.Last_Pos));
   end Image;

   function Image
     (Item    : in Memo_Entry;
      Nonterm : in Token_ID;
      Pos     : in Syntax_Trees.Stream_Index;
      Tree    : in Syntax_Trees.Tree)
     return String
   is begin
      return Image (Item, Nonterm, Tree.Get_Node_Index (Tree.Shared_Stream, Pos), Tree);
   end Image;

   procedure Clear (Derivs : in out Packrat.Derivs)
   is begin
      for D of Derivs loop
         D.Clear (Free_Memory => True);
      end loop;
   end Clear;

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

   procedure Finish_Parse (Parser : in out WisiToken.Parse.Parser.Parser'Class; Result : in out Memo_Entry)
   is
      use WisiToken.Syntax_Trees;
      Tree  : Syntax_Trees.Tree renames Parser.Tree;
      Trace : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;
   begin
      if Trace_Time then
         Trace.Put_Clock ("finish parse");
      end if;

      if Result.State = Packrat.Success then
         if Trace_Parse > Outline then
            Trace.Put_Line ("packrat parse succeed");
         end if;

         --  Clear copies of Stream_Index so Finish_Parse can clear the parse streams.
         Clear (Parser.Derivs);

         Tree.Set_Root (Result.Result);
         Result := (State => No_Result, Max_Examined_Pos => Invalid_Stream_Index, Recursive => False);
         Tree.Finish_Parse;

      else
         --  preserve Derivs for error recover
         declare
            Msg : constant String := Tree.Error_Message
              (Ref     => (Tree.Shared_Stream,
                           Result.Max_Examined_Pos,
                           Tree.Get_Node (Tree.Shared_Stream, Result.Max_Examined_Pos)),
               Message => "parse failed");
         begin
            if Trace_Parse > Outline then
               Tree.Lexer.Trace.Put_Line (Msg);
            end if;

            --  If we raise Syntax_Error, the caller assumes syntax error
            --  information is in the tree; not true for packrat (yet).
            raise WisiToken.Parse_Error with Msg;
            --  FIXME packrat: add "expecting: ..." based on last nonterm?
         end;
      end if;
   end Finish_Parse;

end WisiToken.Parse.Packrat;
