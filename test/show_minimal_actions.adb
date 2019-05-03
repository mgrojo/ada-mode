--  Show various things related to Minimal_Actions for a grammar.

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Generate.LR.LALR_Generate;
with WisiToken.Generate.LR.LR1_Generate;
with WisiToken.Generate.LR1_Items;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Actions;
with Wisitoken_Grammar_Main;
procedure Show_Minimal_Actions
is
   use WisiToken;
   use WisiToken.Generate.LR;

   procedure Put_Usage
   is
   begin
      Put_Line ("show_minimal_actions {grammar_file} {LALR | LR1} {trace_generate}");
   end Put_Usage;

   Input_File_Name : Ada.Strings.Unbounded.Unbounded_String;
   Gen_Alg         : WisiToken.BNF.Generate_Algorithm;
   Trace_Generate  : Integer;

   Trace          : aliased WisiToken.Text_IO_Trace.Trace (Wisitoken_Grammar_Actions.Descriptor'Access);
   Input_Data     : aliased WisiToken_Grammar_Runtime.User_Data_Type;
   Grammar_Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser;

begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count = 3 then
         Input_File_Name := +Argument (1);
         Gen_Alg         := WisiToken.BNF.Generate_Algorithm'Value (Argument (2));
         Trace_Generate  := Integer'Value (Argument (3));
      else
         Put_Usage;
         return;
      end if;
   exception
   when Constraint_Error =>
      Put_Usage;
      return;
   end;

   WisiToken.Trace_Generate := 0; -- Only trace the part we are interested in.

   Wisitoken_Grammar_Main.Create_Parser (Grammar_Parser, Trace'Unchecked_Access, Input_Data'Unchecked_Access);

   begin
      Grammar_Parser.Lexer.Reset_With_File (-Input_File_Name);
      Grammar_Parser.Parse;

      Input_Data.Phase       := WisiToken_Grammar_Runtime.Meta;
      Input_Data.User_Parser := Gen_Alg;
      Input_Data.User_Lexer  := WisiToken.BNF.re2c_Lexer;
      Grammar_Parser.Execute_Actions;

      case Input_Data.Meta_Syntax is
      when WisiToken_Grammar_Runtime.Unknown =>
         Input_Data.Meta_Syntax := WisiToken_Grammar_Runtime.BNF_Syntax;

      when WisiToken_Grammar_Runtime.BNF_Syntax =>
         null;

      when WisiToken_Grammar_Runtime.EBNF_Syntax =>
         declare
            Tree  : WisiToken.Syntax_Trees.Tree renames Grammar_Parser.Parsers.First_State_Ref.Tree;
         begin
            WisiToken_Grammar_Runtime.Translate_EBNF_To_BNF (Tree, Input_Data);
            if WisiToken.Generate.Error then
               raise WisiToken.Grammar_Error with "errors during translating EBNF to BNF: aborting";
            end if;
         end;
      end case;
      Input_Data.Phase := WisiToken_Grammar_Runtime.Other;
      Grammar_Parser.Execute_Actions;
   exception
   when Syntax_Error =>
      Grammar_Parser.Put_Errors;
      return;
   end;


   declare
      Generate_Data  : constant WisiToken.BNF.Generate_Utils.Generate_Data :=
        WisiToken.BNF.Generate_Utils.Initialize (Input_Data);

      Descriptor : WisiToken.Descriptor renames Generate_Data.Descriptor.all;

      Has_Empty_Production : constant Token_ID_Set := Generate.Has_Empty_Production (Generate_Data.Grammar);

      First_Nonterm_Set : constant Token_Array_Token_Set := Generate.First
        (Generate_Data.Grammar, Has_Empty_Production, Generate_Data.Descriptor.First_Terminal);

      First_Terminal_Sequence : constant Token_Sequence_Arrays.Vector :=
        WisiToken.Generate.To_Terminal_Sequence_Array (First_Nonterm_Set, Descriptor);

      Item_Sets : Generate.LR1_Items.Item_Set_List :=
        (case Gen_Alg is
         when WisiToken.BNF.LALR =>
            Generate.LR.LALR_Generate.LALR_Kernels
              (Generate_Data.Grammar, First_Nonterm_Set, Descriptor),
         when WisiToken.BNF.LR1 =>
            Generate.LR.LR1_Generate.LR1_Item_Sets
              (Has_Empty_Production, First_Terminal_Sequence, Generate_Data.Grammar, Descriptor),
         when others => raise WisiToken.User_Error);

      Conflict_Counts   : Conflict_Count_Lists.List;
      Unknown_Conflicts : Generate.LR.Conflict_Lists.List;

      Table : Parse.LR.Parse_Table
        (State_First       => Item_Sets.First_Index,
         State_Last        => Item_Sets.Last_Index,
         First_Terminal    => Generate_Data.Descriptor.First_Terminal,
         Last_Terminal     => Generate_Data.Descriptor.Last_Terminal,
         First_Nonterminal => Generate_Data.Descriptor.First_Nonterminal,
         Last_Nonterminal  => Generate_Data.Descriptor.Last_Nonterminal);

   begin
      case Gen_Alg is
      when WisiToken.BNF.LALR =>
         Generate.LR.LALR_Generate.Fill_In_Lookaheads
           (Generate_Data.Grammar, Has_Empty_Production, First_Terminal_Sequence, Item_Sets,
            Descriptor);
         Generate.LR.LALR_Generate.Add_Actions
           (Item_Sets, Generate_Data.Grammar, Has_Empty_Production, First_Nonterm_Set, First_Terminal_Sequence,
            Conflict_Counts, Unknown_Conflicts, Table, Descriptor);
      when WisiToken.BNF.LR1 =>
         Generate.LR.LR1_Generate.Add_Actions
           (Item_Sets, Generate_Data.Grammar, Has_Empty_Production, First_Nonterm_Set, Conflict_Counts,
            Unknown_Conflicts, Table, Descriptor);
      when others =>
         raise WisiToken.User_Error;
      end case;

      WisiToken.Trace_Generate := Trace_Generate; -- Only trace the part we are interested in.
      declare
         Minimal_Terminal_Sequences : constant Minimal_Sequence_Array :=
           Generate.LR.Compute_Minimal_Terminal_Sequences (Descriptor, Generate_Data.Grammar);
         Minimal_Terminal_First : constant WisiToken.Token_Array_Token_ID :=
           Generate.LR.Compute_Minimal_Terminal_First (Descriptor, Minimal_Terminal_Sequences);

      begin
         Put_Line ("Minimal_Terminal_Sequences:");
         for ID in Minimal_Terminal_Sequences'Range loop
            Put_Line
              (Image (ID, Descriptor) & " => " &
                 Image (Minimal_Terminal_Sequences (ID), Descriptor, Association => True));
         end loop;
         New_Line;

         for State in Table.States'Range loop
            if Trace_Generate > Extra then
               Ada.Text_IO.Put_Line ("Set_Minimal_Complete_Actions:" & State_Index'Image (State));
            end if;

            Set_Minimal_Complete_Actions
              (Table.States (State),
               (case Gen_Alg is
                when WisiToken.BNF.LALR =>
                   Item_Sets (State),
                when WisiToken.BNF.LR1 =>
                   WisiToken.Generate.LR1_Items.Filter
                     (Item_Sets (State), Generate_Data.Grammar, Descriptor,
                      WisiToken.Generate.LR1_Items.In_Kernel'Access),
                when others => raise WisiToken.User_Error),
               Descriptor, Generate_Data.Grammar, Minimal_Terminal_Sequences, Minimal_Terminal_First);
         end loop;
         New_Line;
      end;
   end;
exception
when E : others =>
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Show_Minimal_Actions;
