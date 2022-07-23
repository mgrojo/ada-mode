--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 - 2022 Free Software Foundation, Inc.
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

with WisiToken.Parse.LR.Parser;
separate (WisiToken.Parse.Parser)
procedure LR_Parse
  (Parser     : in out WisiToken.Parse.Parser.Parser;
   Log_File   : in     Ada.Text_IO.File_Type;
   Edits      : in     KMN_Lists.List := KMN_Lists.Empty_List;
   Pre_Edited : in     Boolean        := False)
is
   use WisiToken.Parse.LR.Parser;
   use WisiToken.Parse.LR;
   use WisiToken.Syntax_Trees;
   use all type KMN_Lists.List;
   use all type SAL.Base_Peek_Type;

   Shared_Parser : WisiToken.Parse.Parser.Parser renames Parser;
   Trace         : WisiToken.Trace'Class renames Shared_Parser.Tree.Lexer.Trace.all;

begin
   if Trace_Time then
      Trace.Put_Clock ("start");
   end if;

   if Shared_Parser.User_Data /= null then
      Shared_Parser.User_Data.Reset;
   end if;

   Shared_Parser.String_Quote_Checked := Invalid_Line_Number;

   if Edits /= KMN_Lists.Empty_List then
      if not Shared_Parser.Tree.Editable then
         --  previous parse failed, left tree in uncertain state
         raise WisiToken.Parse_Error with "previous parse failed, can't edit tree";
      end if;

      if Trace_Parse > Detail or Trace_Incremental_Parse > Outline then
         Trace.New_Line;
         Trace.Put_Line ("pre edit tree:");
         Shared_Parser.Tree.Print_Tree (Line_Numbers => True, Non_Grammar => True);
         Trace.New_Line;
      end if;

      Edit_Tree (Shared_Parser, Edits);

      if Trace_Time then
         Trace.Put_Clock ("post edit tree");
      end if;

      if Trace_Memory > Detail then
         Trace.Put_Line ("post edit tree");
         Report_Memory (Trace, Prefix => True);
      end if;
      if Trace_Parse > Outline or Trace_Incremental_Parse > Outline then
         Trace.New_Line;
         --  Parents not set, can't get Line_Numbers
         Trace.Put_Line ("edited stream:");
         Trace.Put_Line
           (Shared_Parser.Tree.Image
              (Shared_Parser.Tree.Shared_Stream,
               Children    => Trace_Parse > Extra or Trace_Incremental_Parse > Extra,
               Non_Grammar => Trace_Parse > Extra or Trace_Incremental_Parse > Extra));
         Trace.New_Line;
      end if;

      if Shared_Parser.Tree.Stream_Length (Shared_Parser.Tree.Shared_Stream) = 3 and then Shared_Parser.Tree.ID
        (Shared_Parser.Tree.Stream_First (Shared_Parser.Tree.Shared_Stream, Skip_SOI => True).Node) =
        Shared_Parser.Tree.Lexer.Descriptor.Accept_ID
      then
         if Trace_Parse > Outline then
            Trace.Put_Line ("edited tree does not need parse; no or only non_grammar changes");
         end if;
         Shared_Parser.Tree.Finish_Parse;
         Shared_Parser.Parsers.Clear;
         return;
      end if;

   elsif Pre_Edited then
      --  Unit test providing an edited stream; see test_syntax_trees.adb
      --  Breakdown_Optimized_List_01. We assume this is the same as a tree
      --  resulting from Edit_Tree.
      null;

   else
      --  Normal initial parse
      Shared_Parser.Tree.Clear;
      Shared_Parser.Lex_All;
      if Trace_Memory > Detail then
         Trace.Put_Line ("post lex");
         Report_Memory (Trace, Prefix => True);
      end if;
   end if;

   Shared_Parser.Parsers := Parser_Lists.New_List (Shared_Parser.Tree);

   Shared_Parser.Tree.Start_Parse (Shared_Parser.Parsers.First.State_Ref.Stream, Shared_Parser.Table.State_First);

   LR_Core_Parse (Shared_Parser, Log_File, Recover_Only => False);

   if Trace_Parse > Outline then
      Trace.Put_Line (" " & Shared_Parser.Tree.Trimmed_Image (Shared_Parser.Parsers.First.Stream) & ": succeed");
   end if;

   Finish_Parse (Shared_Parser, Incremental_Parse => Pre_Edited or Edits /= KMN_Lists.Empty_List);

   if Trace_Time then
      Trace.Put_Clock ("finish parse");
   end if;

   --  We don't raise Syntax_Error for lexer errors, since they are all
   --  recovered, either by inserting a quote, or by ignoring the
   --  character.
exception
when Partial_Parse =>
   Finish_Parse (Shared_Parser, Incremental_Parse => False);
   if Trace_Time then
      Trace.Put_Clock ("finish partial parse");
   end if;

when Syntax_Error | WisiToken.Parse_Error =>
   if Trace_Time then
      Trace.Put_Clock ("finish - error");
   end if;
   raise;

when E : others =>
   declare
      Msg : constant String := Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E);
   begin
      if Debug_Mode then
         --  If this is from a McKenzie task, that also outputs a stack trace.
         Trace.Put_Line ("exception: " & Msg);
         Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E)); -- includes Prefix
         Trace.New_Line;
      end if;

      --  Emacs displays the exception message in the echo area; easy to miss
      raise WisiToken.Parse_Error with Msg;
   end;
end LR_Parse;
