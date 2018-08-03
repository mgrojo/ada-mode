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

with Ada_Process_Actions;
package body Wisi.Libadalang is

   function To_Byte_Region (First, Last : in Integer) return WisiToken.Buffer_Region
   is begin
      --  Libadalang buffer indices are for Wide_Wide_Character, not bytes.
      --  We just ignore that, and copy them.
      return (WisiToken.Buffer_Pos (First), WisiToken.Buffer_Pos (Last));
   end To_Byte_Region;

   procedure To_WisiToken_Tree
     (Ast       : in     Standard.Libadalang.Analysis.Ada_Node;
      Unit      : in     Standard.Libadalang.Analysis.Analysis_Unit; --  FIXME: only need tdh?
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      User_Data : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use Standard.Libadalang.Analysis;
      procedure Process_Ast_Node
        (Ast_Node    : in     Ada_Node;
         Ast_Parent  : in     Ada_Node;
         Terminals   : in out WisiToken.Base_Token_Arrays.Vector;
         Tree        : in out WisiToken.Syntax_Trees.Tree;
         Tree_Parent : in     WisiToken.Syntax_Trees.Valid_Node_Index)
      is
         --  Create a child of Tree_Node_Parent matching ast_node
         Ast_Token_Index : Standard.Libadalang.Lexer.Token_Data_Handlers.Token_Index := Token_Start (Ast).Index.Token;
         Ast_Token       : Standard.Libadalang.Lexer.Token_Data_Type                 :=
           Standard.Libadalang.Lexer.Token_Data_Handlers.Get_Token (Unit.Tdh, Ast_Token_Index);

         Token_Index : WisiToken.Token_Index := WisiToken.Token_Index (Ast_Token_Index);
      begin
         case Ast.Kind is
         when Ada_Char_Literal | Ada_Identifier | Ada_String_Literal | Ada_Null_Literal | Ada_Int_Literal |
           Ada_Real_Literal =>
            Tree.Add_Terminal (Token_Index, Terminals);

         when =>
            declare
               Ast_Children  : Children_Array := Ast_Node.Children_With_Trivia;
               Tree_Children : Valid_Node_Index_Array (Ast_Children'First .. Ast_Children'Last);
               Parent        : Valid_Node_Index;
            begin
               for I in Ast.Children'Range loop
                  Process_Ast_Node (Ast_Children (I), Tree, Tree_Children (I));
               end loop;
               Parent := Tree.Add_Nonterm
              (Production      => Invalid_Production_ID, -- FIXME: only used for debug messages?, add later if needed
               Children        => (1 .. 0 => <>),
               Action          => null,
                  Default_Virtual => False);
         end case;
      end Process_Ast_Node;

   begin
      --  The operations in WisiToken.Syntax_Trees support building the tree
      --  bottom up, so we walk thru Ast that way.

      if Ast.Kind = Ada_Compilation_Unit then
         declare
            Root : WisiToken.Syntax_Trees.Valid_Node_Index := Tree.Add_Nonterm
              (Production      => Invalid_Production_ID, -- FIXME: only used for debug messages?, add later if needed
               Children        => (1 .. 0 => <>),
               Action          => null,
               Default_Virtual => False);
         begin
            for Child of Ast.Children loop
               Process_Ast_Node (Child, Tree, Root);
            end loop;
         end;
      else
         raise Programmer_Error;
      end if;
   end To_WisiToken_Tree;

   ----------
   --  Public subprograms

   procedure Reset_With_String (Lexer : in out Wisi.Libadalang.Lexer; Input : in String)
   is begin
      raise SAL.Not_Implemented;
   end Reset_With_String;

   procedure Reset_With_String_Access
     (Lexer : in out Wisi.Libadalang.Lexer;
      Input : in     Ada.Strings.Unbounded.String_Access)
   is begin
      raise SAL.Not_Implemented;
   end Reset_With_String_Access;

   procedure Reset_With_File (Lexer : in out Wisi.Libadalang.Lexer; File_Name : in String)
   is begin
      raise SAL.Not_Implemented;
   end Reset_With_File;

   procedure Reset (Lexer : in out Wisi.Libadalang.Lexer)
   is begin
      raise SAL.Not_Implemented;
   end Reset;

   procedure Discard_Rest_Of_Input (Lexer : in out Wisi.Libadalang.Lexer)
   is begin
      raise SAL.Not_Implemented;
   end Discard_Rest_Of_Input;

   function Buffer_Text (Lexer : in Wisi.Libadalang.Lexer; Byte_Region : in Buffer_Region) return String
   is begin
      return Langkit_Support.Text.Transcode
        (Text => Libadalang.Lexer.Token_Data_Handlers.Text
           ((Kind         => Ada_Comment,
             Source_First => Byte_Region.First,
             Source_Last  => Byte_Region.Last,
             Symbol       => null,
             Sloc_Range   => Langkit_Support.Slocs.No_Source_Location_Range)),
        Charset => "UTF-8");
   end Buffer_Text;

   overriding procedure Parse (Parser : aliased in out Wisi.Libadalang.Parser)
   is
      use Standard.Libadalang.Analysis;
      Ctx : Analysis_Context := Create (With_Trivia => True);
   begin
      Parser.Unit := Get_From_File (Ctx, -Parser.Source_File_Name, "", True, Rule => Compilation_Rule);

      Parser.Lexer.TDH := Unit.First_Token.TDH;

      Parser.Terminals.Set_First_Last (1, Parser.Lexer.TDH.Last_Token);

      declare
         use Libadalang.Lexer.Token_Data_Handlers;
         use Libadalang.Lexer;
         TDH : Token_Data_Handler renames Parser.Lexer.TDH.all;
      begin
         for I in Parser.Terminals.First_Index .. Parser.Terminals.Last_Index loop
            declare
               Token_Data : Token_Data_Type : Get_Token (I);
               Trivia : Token_Index_Vectors.Elements_Array := Get_Trivias (TDH, I);
            begin
               Parser.Terminals (I) :=
                 (ID => To_Wisi_ID ()
                 Byte_Region => To_Byte_Region (Token_Data_type);

               User_Data.Augmented_Terminals (I) :=
                 ();

               Next (I, TDH);
            end;
         end loop;
      end;
      To_WisiToken_Tree (Root (Parser.Unit), Parser.User_Data);
   end Parse;

   overriding function Any_Errors (Parser : in Wisi.Libadalang.Parser) return Boolean
   is begin
      return Standard.Libadalang.Analysis.Has_Diagnostics (Parser.Unit);
   end Any_Errors;

   overriding procedure Put_Errors (Parser : in Wisi.Libadalang.Parser; Input_File_Name : in String)
   is begin
      for D of Standard.Libadalang.Analysis.Diagnostics (Parser.Unit) loop
         --  FIXME: convert to Parser.Errors, let main put thru Wisi_Runtime for elisp
         Ada.Text_IO.Put_Line (Standard.Libadalang.Analysis.Format_GNU_Diagnostic (Parser.Unit, D));
      end loop;
   end Put_Errors;

   overriding procedure Execute_Actions (Parser : in out Wisi.Libadalang.Parser)
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

end Wisi.Libadalang;
