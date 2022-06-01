--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2022 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Exceptions;
with GNAT.Traceback.Symbolic;
package body WisiToken.Parse.Parser is

   ----------
   --  Package public subprograms, declaration order

   procedure Process_Grammar_Token
     (Parser : in out WisiToken.Parse.Parser.Parser'Class;
      Token  : in     Lexer.Token;
      Node   : in     Syntax_Trees.Valid_Node_Access)
   is
      use all type Syntax_Trees.User_Data_Access;
   begin
      if Parser.User_Data /= null then
         Parser.User_Data.Lexer_To_Augmented (Parser.Tree, Token, Node);
      end if;
   end Process_Grammar_Token;

   procedure Process_Non_Grammar_Token
     (Parser       : in out WisiToken.Parse.Parser.Parser'Class;
      Grammar_Node : in     Syntax_Trees.Valid_Node_Access;
      Token        : in     Lexer.Token)
   is
      use all type Syntax_Trees.Node_Access;
      use all type Syntax_Trees.User_Data_Access;
   begin
      Parser.Tree.Non_Grammar_Var (Grammar_Node).Append (Token);
      if Parser.User_Data /= null then
         Parser.User_Data.Lexer_To_Augmented (Parser.Tree, Token, Grammar_Node);
      end if;
   end Process_Non_Grammar_Token;

   function Next_Grammar_Token
     (Parser            : in out WisiToken.Parse.Parser.Parser'Class;
      Last_Grammar_Node : in out WisiToken.Syntax_Trees.Node_Access)
     return Token_ID
   is
      use Syntax_Trees;

      Tree  : Syntax_Trees.Tree renames Parser.Tree;
      Lexer : WisiToken.Lexer.Handle renames Parser.Tree.Lexer;
   begin
      loop
         declare
            Token        : WisiToken.Lexer.Token;
            Error_Count  : constant Natural := Lexer.Find_Next (Token);
            Lexer_Errors : Error_Data_Lists.List;
         begin

            if Trace_Lexer > Outline then
               Tree.Lexer.Trace.Put_Line (WisiToken.Lexer.Full_Image (Token, Tree.Lexer.Descriptor.all));
            end if;

            if Error_Count > 0 then
               declare
                  Cur : WisiToken.Lexer.Error_Lists.Cursor := Lexer.Errors.Last;
               begin
                  for I in 1 .. Error_Count - 1 loop
                     WisiToken.Lexer.Error_Lists.Previous (Cur);
                  end loop;
                  for I in 1 .. Error_Count loop
                     Lexer_Errors.Append (Lexer_Error'(Error => Lexer.Errors (Cur)));
                     WisiToken.Lexer.Error_Lists.Next (Cur);
                  end loop;
               end;
            end if;

            if Token.ID >= Lexer.Descriptor.First_Terminal then
               declare
                  Ref : constant Terminal_Ref := Tree.Add_Terminal (Parser.Tree.Shared_Stream, Token, Lexer_Errors);
               begin
                  Process_Grammar_Token (Parser, Token, Ref.Node);
                  Last_Grammar_Node := Ref.Node;
               end;
            else
               if Trace_Lexer > Detail then
                  Tree.Lexer.Trace.Put_Line ("non-grammar in " & Parser.Tree.Image (Last_Grammar_Node));
               end if;
               if Error_Count > 0 then
                  --  test_incremental.adb Lexer_Errors_04, _05
                  Tree.Add_Errors (Tree.Shared_Stream, Last_Grammar_Node, Lexer_Errors);
               end if;

               Process_Non_Grammar_Token (Parser, Last_Grammar_Node, Token);
            end if;

            if Error_Count > 0 and Trace_Lexer > Detail then
               Tree.Lexer.Trace.Put_Line
                 ("lexer error" & (if Error_Count > 1 then "s" else "") &
                    " in " & Parser.Tree.Image (Last_Grammar_Node));
            end if;

            if Token.ID >= Lexer.Descriptor.First_Terminal then
               return Token.ID;
            end if;
         end;
      end loop;
   end Next_Grammar_Token;

   procedure Lex_All (Parser : in out WisiToken.Parse.Parser.Parser'Class)
   is
      EOI_ID : constant Token_ID := Parser.Tree.Lexer.Descriptor.EOI_ID;

      Last_Grammar_Node : WisiToken.Syntax_Trees.Node_Access;
   begin
      Parser.Tree.Start_Lex;

      Last_Grammar_Node := Parser.Tree.SOI;

      loop
         exit when EOI_ID = Next_Grammar_Token (Parser, Last_Grammar_Node);
      end loop;
      if Trace_Parse > Outline then
         Parser.Tree.Lexer.Trace.Put_Line (Syntax_Trees.Get_Node_Index (Last_Grammar_Node)'Image & " tokens lexed");
      end if;
   end Lex_All;

   procedure LR_Parse
     (Parser     : in out WisiToken.Parse.Parser.Parser'Class;
      Log_File   : in     Ada.Text_IO.File_Type;
      Edits      : in     KMN_Lists.List := KMN_Lists.Empty_List;
      Pre_Edited : in     Boolean        := False)
   is separate;

   function Get_In_Parse_Action
     (Parser : in WisiToken.Parse.Parser.Parser;
      ID     : in Production_ID)
     return Syntax_Trees.In_Parse_Actions.In_Parse_Action
   is begin
      if Parser.Productions.Is_Empty then
         return null;
      elsif Parser.Productions (ID.LHS).RHSs.Is_Empty then
         return null;
      else
         return Parser.Productions (ID.LHS).RHSs (ID.RHS).In_Parse_Action;
      end if;
   end Get_In_Parse_Action;

   function Get_Post_Parse_Action
     (Productions : in Syntax_Trees.Production_Info_Trees.Vector;
      ID          : in Production_ID)
     return Syntax_Trees.Post_Parse_Action
   is begin
      if Productions.Is_Empty then
         return null;
      elsif Productions (ID.LHS).RHSs.Is_Empty then
         return null;
      else
         return Productions (ID.LHS).RHSs (ID.RHS).Post_Parse_Action;
      end if;
   end Get_Post_Parse_Action;

   function Get_Post_Parse_Action
     (Parser : in WisiToken.Parse.Parser.Parser;
      ID     : in Production_ID)
     return Syntax_Trees.Post_Parse_Action
   is begin
      return Get_Post_Parse_Action (Parser.Productions, ID);
   end Get_Post_Parse_Action;

   procedure Put_Errors (Parser : in WisiToken.Parse.Parser.Parser'Class)
   is
      use WisiToken.Syntax_Trees;
      Tree : Syntax_Trees.Tree renames Parser.Tree;
   begin
      for Err in Tree.Error_Iterate loop
         declare
            Error_Node : constant Valid_Node_Access := Tree.Error_Node (Err);
         begin
            Ada.Text_IO.Put_Line
              (Tree.Error_Message
               (Error_Node, Error (Err).Image (Tree, Error_Node)));
         end;
      end loop;
   end Put_Errors;

   procedure Put_Errors (Parser : in WisiToken.Parse.Parser.Parser'Class; Stream : in Syntax_Trees.Stream_ID)
   is
      use WisiToken.Syntax_Trees;
      Tree : Syntax_Trees.Tree renames Parser.Tree;
   begin
      for Cur in Tree.Stream_Error_Iterate (Stream) loop
         declare
            Error_Ref  : constant Stream_Error_Ref  := Error (Cur);
            Error_Node : constant Valid_Node_Access := Tree.Error_Node (Error_Ref).Node;
         begin
            for Err of Tree.Error_List (Error_Node) loop
               Ada.Text_IO.Put_Line
                 (Tree.Error_Message
                    (Ref     => Error_Ref.Ref.Ref, -- For line, column
                     Message => Err.Image (Tree, Error_Node)));
            end loop;
         end;
      end loop;
   end Put_Errors;

   procedure Execute_Actions
     (Tree                : in out Syntax_Trees.Tree;
      Productions         : in     Syntax_Trees.Production_Info_Trees.Vector;
      User_Data           : in     Syntax_Trees.User_Data_Access;
      Action_Region_Bytes : in     WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region)
   is
      use all type WisiToken.Syntax_Trees.Node_Access;
      use all type Syntax_Trees.Post_Parse_Action;
      use all type Syntax_Trees.User_Data_Access;

      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Syntax_Trees.Valid_Node_Access)
      is
         use all type Syntax_Trees.Node_Label;
         Node_Byte_Region : constant Buffer_Region := Tree.Byte_Region
           (Node, Trailing_Non_Grammar => True);
      begin
         if Tree.Label (Node) /= Nonterm or else
           not (Node_Byte_Region = Null_Buffer_Region or
                  Action_Region_Bytes = Null_Buffer_Region or
                  Overlaps (Node_Byte_Region, Action_Region_Bytes))
         then
            return;
         end if;

         for Child of Tree.Children (Node) loop
            if Child /= Syntax_Trees.Invalid_Node_Access then
               --  Child can be null in an edited tree
               Process_Node (Tree, Child);
            end if;
         end loop;

         User_Data.Reduce (Tree, Node);
         declare
            Post_Parse_Action : constant Syntax_Trees.Post_Parse_Action := Get_Post_Parse_Action
              (Productions, Tree.Production_ID (Node));
         begin
            if Post_Parse_Action /= null then
               begin
                  Post_Parse_Action (User_Data.all, Tree, Node);
               exception
               when E : others =>
                  if WisiToken.Debug_Mode then
                     Tree.Lexer.Trace.Put_Line
                       (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
                     Tree.Lexer.Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
                     Tree.Lexer.Trace.New_Line;
                  end if;

                  raise WisiToken.Parse_Error with Tree.Error_Message
                    (Node,
                     "action raised exception " & Ada.Exceptions.Exception_Name (E) & ": " &
                       Ada.Exceptions.Exception_Message (E));
               end;
            end if;
         end;
      end Process_Node;

   begin
      if User_Data = null then
         return;
      end if;

      if Tree.Root = Syntax_Trees.Invalid_Node_Access then
         --  No code in file, and error recovery failed to insert valid code.
         --  Or ambiguous parse; Finish_Parse not called.
         return;
      end if;

      User_Data.Initialize_Actions (Tree);

      Process_Node (Tree, Tree.Root);
   exception
   when WisiToken.Parse_Error =>
      raise;

   when E : others =>
      if Debug_Mode then
         Tree.Lexer.Trace.Put_Line
           (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
         Tree.Lexer.Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         Tree.Lexer.Trace.New_Line;
      end if;
      raise;
   end Execute_Actions;

   procedure Execute_Actions
     (Parser              : in out WisiToken.Parse.Parser.Parser'Class;
      Action_Region_Bytes : in     WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region)
   is begin
      Execute_Actions (Parser.Tree, Parser.Productions, Parser.User_Data, Action_Region_Bytes);
   end Execute_Actions;

end WisiToken.Parse.Parser;
