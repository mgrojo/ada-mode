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

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body WisiToken.Parse.Packrat.Parser is

   function Image (Item : in Recover_Op_Nodes; Tree : in Syntax_Trees.Tree) return String
   is
      use WisiToken.Syntax_Trees;
   begin
      return
        "(" & Image (Item.Op) & ", " &
        Image (Item.ID, Tree.Lexer.Descriptor.all) &
        (if Item.Node = Invalid_Node_Access
         then ""
         else Tree.Image (Item.Node)) & "," &
        Item.Pos'Image & "," &
        Item.Error_Pos'Image & ")";
   end Image;

   overriding procedure Finalize (Object : in out Parser)
   is begin
      --  Derivs holds references to Tree, so Derivs must be cleared before
      --  Tree is finalized.
      WisiToken.Parse.Packrat.Clear (Object.Derivs);

      WisiToken.Parse.Parser.Finalize (WisiToken.Parse.Parser.Parser (Object));
   end Finalize;

   function Delete_Valid (Parser : in Packrat.Parser.Parser; Pos : in Syntax_Trees.Stream_Index) return Boolean
   is
      use Recover_Op_Nodes_Trees;
      Found : constant Cursor := Parser.Insert_Delete.Find
        (Parser.Tree.Get_Node_Index (Parser.Tree.Shared_Stream, Pos));
   begin
      return Found /= No_Element and then
        (for some Op of Parser.Insert_Delete.Constant_Ref (Found) => Op.Op = Delete);
   end Delete_Valid;

   function Has_Input (Parser : in Packrat.Parser.Parser; Pos : in Syntax_Trees.Stream_Index) return Boolean
   is
      use Recover_Op_Nodes_Trees;
      Found : constant Cursor := Parser.Insert_Delete.Find
        (Parser.Tree.Get_Node_Index (Parser.Tree.Shared_Stream, Pos));
   begin
      return Found /= No_Element and then
        (for some Op of Parser.Insert_Delete.Constant_Ref (Found) => Op.Op = Insert);
   end Has_Input;

   function Input_Op
     (Parser    : in Packrat.Parser.Parser;
      Pos       : in Syntax_Trees.Stream_Index;
      Prev_Node : in Syntax_Trees.Node_Access)
     return ID_Node_Type
   is
      use all type WisiToken.Syntax_Trees.Node_Access;
      use Recover_Op_Nodes_Lists;

      List : Recover_Op_Nodes_Lists.List renames Parser.Insert_Delete.Constant_Ref
        (Parser.Tree.Get_Node_Index (Parser.Tree.Shared_Stream, Pos));

      Cur      : Cursor := List.Last;
      Prev_Cur : Cursor := Previous (Cur);
   begin
      return Result : ID_Node_Type do
         if List (Cur).Node = Prev_Node then
            return;

         else
            loop
               if Prev_Cur = No_Element or else
                 (List (Prev_Cur).Op = Delete or else
                    List (Prev_Cur).Node = Prev_Node)
               then
                  declare
                     Op : Recover_Op_Nodes renames Constant_Ref (Cur);
                  begin
                     Result := (ID => Op.ID, Node => Op.Node);
                  end;
                  exit;

               end if;
               Cur      := Prev_Cur;
               Prev_Cur := Previous (Cur);
            end loop;
         end if;
      end return;
   end Input_Op;

   procedure Packrat_Parse
     (Shared_Parser : in out Parser;
      Log_File      : in     Ada.Text_IO.File_Type)
   is separate;

   procedure Finish_Parse (Parser : in out WisiToken.Parse.Packrat.Parser.Parser'Class; Result : in out Memo_Entry)
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
         Result := No_Result_Memo;
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

   procedure Print_Derivs (Parser : in WisiToken.Parse.Packrat.Parser.Parser)
   is
      Tree   : Syntax_Trees.Tree renames Parser.Tree;
      Derivs : WisiToken.Parse.Packrat.Derivs renames Parser.Derivs;
      Trace  : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;
      Count  : Integer := 0;
   begin
      for Nonterm in Derivs'Range loop
         for Pos in Derivs (Nonterm).First_Index .. Derivs (Nonterm).Last_Index loop

            case Derivs (Nonterm)(Pos).State is
            when No_Result =>
               null;

            when Failure | Success =>
               Count := @ + 1;
               Trace.Put_Line (Packrat.Image (Derivs (Nonterm)(Pos), Nonterm, Pos, Tree));

            end case;
         end loop;
      end loop;
      Trace.Put_Line ("... failed + success count:" & Count'Image);
   end Print_Derivs;

end WisiToken.Parse.Packrat.Parser;
