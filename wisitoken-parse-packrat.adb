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

   function Image (Tree : in Syntax_Trees.Tree; Item : in Memo_Entry) return String
   is begin
      return
        (case Item.State is
         when No_Result => "",
         when Failure => "fail " & Tree.Image
           (Tree.Get_Node (Tree.Shared_Stream, Item.Max_Examined_Pos), Node_Numbers => True),
         when Success => "success " & Tree.Image (Item.Result, Node_Numbers => True));
   end Image;

   procedure Finish_Parse (Parser : in out Packrat.Parser; Result : in out Memo_Entry)
   is
      use WisiToken.Syntax_Trees;
      Tree       : Syntax_Trees.Tree renames Parser.Tree;
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
      Trace      : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;
   begin
      if Trace_Time then
         Trace.Put_Clock ("finish parse");
      end if;

      if Result.State = Packrat.Success then
         --  Clear copies of Stream_Index so Clear_Parse_Streams can run.
         for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
            Parser.Derivs (Nonterm).Clear (Free_Memory => True);
         end loop;

         --  Do this before Clear_Parse_Streams so the root node is not deleted.
         Tree.Set_Root (Result.Result);
         Result := (State => No_Result, Max_Examined_Pos => Invalid_Stream_Index, Recursive => False);
         Tree.Clear_Parse_Streams; -- also frees excess tree nodes created by backtracking.

      else
         --  preserve Deriv for experimenting with error recover
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
