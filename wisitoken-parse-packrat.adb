--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

   overriding procedure Parse (Parser : aliased in out Packrat.Parser)
   is
      --  'aliased required for Base_Tree'Access. WORKAROUND: that was
      --  enough when Parser type was declared in generated Main; now that
      --  it's a derived type, it doesn't work. So we use Unchecked_Access.

      Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;

      Junk : WisiToken.Syntax_Trees.Valid_Node_Index;
      pragma Unreferenced (Junk);

      Result : WisiToken.Parse.Packrat.Memo_Entry;
   begin
      Parser.Base_Tree.Clear;
      Parser.Tree.Initialize (Parser.Base_Tree'Unchecked_Access, Flush => True);
      Parser.Lex_All;
      Parser.Derivs.Set_First (Descriptor.First_Nonterminal);
      Parser.Derivs.Set_Last (Descriptor.Last_Nonterminal);

      for Nonterm in Descriptor.First_Nonterminal .. Parser.Trace.Descriptor.Last_Nonterminal loop
         Parser.Derivs (Nonterm).Clear;
         Parser.Derivs (Nonterm).Set_First (Parser.Terminals.First_Index);
         Parser.Derivs (Nonterm).Set_Last (Parser.Terminals.Last_Index);
      end loop;

      for Token_Index in Parser.Terminals.First_Index .. Parser.Terminals.Last_Index loop
         Junk := Parser.Tree.Add_Terminal (Token_Index, Parser.Terminals);
         --  FIXME: move this into Lex_All, delete Terminals, just use Syntax_Tree
      end loop;

      Result := Parser.Parse_WisiToken_Accept (Parser, Parser.Terminals.First_Index - 1);

      if Result.State /= Success then
         raise Syntax_Error with "parse failed"; --  FIXME: need better error message!
      else
         Parser.Tree.Set_Root (Result.Result);
      end if;

   end Parse;

   overriding function Any_Errors (Parser : in Packrat.Parser) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      return Parser.Lexer.Errors.Length > 0;
   end Any_Errors;

   overriding procedure Put_Errors (Parser : in Packrat.Parser; Input_File_Name : in String)
   is
      use Ada.Text_IO;
   begin
      for Item of Parser.Lexer.Errors loop
         Put_Line
           (Current_Error,
            Input_File_Name & ":0:0: lexer unrecognized character at" & Buffer_Pos'Image (Item.Char_Pos));
      end loop;

      --  FIXME: Packrat parser does not report errors yet.
   end Put_Errors;

   overriding procedure Execute_Actions (Parser : in out Packrat.Parser)
   is
      Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;

      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Syntax_Trees.Valid_Node_Index)
      is
         use all type Syntax_Trees.Node_Label;
      begin
         if Tree.Label (Node) /= Nonterm then
            return;
         end if;

         declare
            use all type Syntax_Trees.Semantic_Action;
            Tree_Children : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Node);
         begin
            Parser.User_Data.Reduce (Tree, Node, Tree_Children);

            if Tree.Action (Node) /= null then
               Tree.Action (Node) (Parser.User_Data.all, Tree, Node, Tree_Children);
            end if;
         end;
      end Process_Node;

   begin
      if Trace_Action > Outline then
         Parser.Trace.Put_Line ("root node: " & Parser.Tree.Image (Parser.Tree.Root, Descriptor));
      end if;

      Parser.Tree.Process_Tree (Process_Node'Access);
   end Execute_Actions;

end WisiToken.Parse.Packrat;
