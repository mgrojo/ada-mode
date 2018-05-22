--  generated by WisiToken Wisi from wisi_grammar.wy
--  with command line: wisi-generate.exe -v 1 wisi_grammar.wy
--
with WisiToken.Wisi_Grammar_Runtime; use WisiToken.Wisi_Grammar_Runtime;

with WisiToken.Lexer.re2c;
with wisi_grammar_re2c_c;
package body Wisi_Grammar is

   package Lexer is new WisiToken.Lexer.re2c
     (wisi_grammar_re2c_c.New_Lexer,
      wisi_grammar_re2c_c.Free_Lexer,
      wisi_grammar_re2c_c.Reset_Lexer,
      wisi_grammar_re2c_c.Next_Token);

   procedure declaration_0
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Add_Declaration (User_Data, Tree, Tokens);
   end declaration_0;

   procedure declaration_1
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Add_Declaration (User_Data, Tree, Tokens);
   end declaration_1;

   procedure declaration_2
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Add_Declaration (User_Data, Tree, Tokens);
   end declaration_2;

   procedure declaration_3
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Start_If (User_Data, Tree, Tokens);
   end declaration_3;

   procedure declaration_4
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Tree, Nonterm, Tokens);
   begin
      End_If (User_Data);
   end declaration_4;

   procedure nonterminal_0
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Add_Nonterminal (User_Data, Tree, Tokens);
   end nonterminal_0;

   procedure compilation_unit_0
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Index;
     Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Add_Preamble (User_Data, Tree, Tokens);
   end compilation_unit_0;

   procedure Create_Parser
     (Parser                       :    out WisiToken.LR.Parser_No_Recover.Parser;
      Algorithm                    : in     WisiToken.Generator_Algorithm_Type;
      Trace                        : not null access WisiToken.Trace'Class;
      User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use WisiToken.LR;
      use all type WisiToken.Generator_Algorithm_Type;
      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 58,
         First_Terminal    => Descriptor.First_Terminal,
         Last_Terminal     => Descriptor.Last_Terminal,
         First_Nonterminal => Descriptor.First_Nonterminal,
         Last_Nonterminal  => Descriptor.Last_Nonterminal);
      pragma Unreferenced (Algorithm);
   begin
      Table.McKenzie_Param :=
        (First_Terminal    => 3,
         Last_Terminal     => 25,
         First_Nonterminal => 26,
         Last_Nonterminal  => 36,
         Insert =>
           (0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0),
         Delete =>
           (0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0),
         Push_Back =>
           (0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0),
         Task_Count  => 0,
         Cost_Limit  => 2147483647,
         Check_Limit => 2147483647,
         Check_Delta_Limit => 2147483647);


      Table.Productions.Set_Length (37);
      Set_Production (Table.Productions (1), 26, (36, 25));
      Set_Production (Table.Productions (2), 27, (17, 28, 22, 29));
      Set_Production (Table.Productions (3), 27, (17, 22, 29));
      Set_Production (Table.Productions (4), 27, (17, 22));
      Set_Production (Table.Productions (5), 27, (17, 4, 22, 14, 22));
      Set_Production (Table.Productions (6), 27, (17, 3, 4));
      Set_Production (Table.Productions (7), 28, (1 => 5));
      Set_Production (Table.Productions (8), 28, (6, 16, 22, 15));
      Set_Production (Table.Productions (9), 28, (7, 16, 22, 15));
      Set_Production (Table.Productions (10), 29, (1 => 30));
      Set_Production (Table.Productions (11), 29, (29, 30));
      Set_Production (Table.Productions (12), 30, (1 => 13));
      Set_Production (Table.Productions (13), 30, (1 => 22));
      Set_Production (Table.Productions (14), 30, (1 => 14));
      Set_Production (Table.Productions (15), 30, (1 => 21));
      Set_Production (Table.Productions (16), 30, (1 => 9));
      Set_Production (Table.Productions (17), 30, (1 => 20));
      Set_Production (Table.Productions (18), 30, (1 => 23));
      Set_Production (Table.Productions (19), 30, (1 => 24));
      Set_Production (Table.Productions (20), 30, (1 => 7));
      Set_Production (Table.Productions (21), 31, (22, 12, 32, 19));
      Set_Production (Table.Productions (22), 32, (1 => 33));
      Set_Production (Table.Productions (23), 32, (32, 11, 33));
      Set_Production (Table.Productions (24), 32, (32, 17, 4, 22, 14, 22));
      Set_Production (Table.Productions (25), 32, (32, 17, 3, 4));
      Set_Production (Table.Productions (26), 33, (1 .. 0 => <>));
      Set_Production (Table.Productions (27), 33, (1 => 34));
      Set_Production (Table.Productions (28), 33, (34, 10));
      Set_Production (Table.Productions (29), 33, (34, 10, 10));
      Set_Production (Table.Productions (30), 34, (1 => 22));
      Set_Production (Table.Productions (31), 34, (34, 22));
      Set_Production (Table.Productions (32), 35, (1 => 8));
      Set_Production (Table.Productions (33), 35, (1 => 27));
      Set_Production (Table.Productions (34), 35, (1 => 18));
      Set_Production (Table.Productions (35), 35, (1 => 31));
      Set_Production (Table.Productions (36), 36, (1 => 35));
      Set_Production (Table.Productions (37), 36, (36, 35));

      Table.Minimal_Terminal_Sequences.Set_First (26);
      Table.Minimal_Terminal_Sequences.Set_Last (36);
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (26), (18, 25));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (27), (17, 22));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (28), (1 => 5));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (29), (1 => 7));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (30), (1 => 7));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (31), (22, 12, 19));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (32), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (33), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (34), (1 => 22));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (35), (1 => 18));
      Set_Token_Sequence (Table.Minimal_Terminal_Sequences (36), (1 => 18));

      Table.States (0).Productions := WisiToken.LR.To_Vector ((1 => 1));
      Add_Action (Table.States (0), (1 => 32), 8, 1);
      Add_Action (Table.States (0), (2, 3, 4, 5, 6), 17, 2);
      Add_Action (Table.States (0), (1 => 34), 18, 3);
      Add_Action (Table.States (0), (1 => 21), 22, 4);
      Add_Error (Table.States (0));
      Add_Goto (Table.States (0), 33, 27, 5);
      Add_Goto (Table.States (0), 35, 31, 6);
      Add_Goto (Table.States (0), 36, 35, 7);
      Add_Goto (Table.States (0), 1, 36, 8);
      Table.States (1).Productions := WisiToken.LR.To_Vector ((1 => 32));
      Add_Action (Table.States (1), 8, Reduce, 32, 35, 1, 0, compilation_unit_0'Access, null);
      Add_Action (Table.States (1), 17, Reduce, 32, 35, 1, 0, compilation_unit_0'Access, null);
      Add_Action (Table.States (1), 18, Reduce, 32, 35, 1, 0, compilation_unit_0'Access, null);
      Add_Action (Table.States (1), 22, Reduce, 32, 35, 1, 0, compilation_unit_0'Access, null);
      Add_Action (Table.States (1), 25, Reduce, 32, 35, 1, 0, compilation_unit_0'Access, null);
      Add_Error (Table.States (1));
      Table.States (2).Productions := WisiToken.LR.To_Vector ((2, 3, 4, 5, 6));
      Add_Action (Table.States (2), (1 => 6), 3, 11);
      Add_Action (Table.States (2), (1 => 5), 4, 12);
      Add_Action (Table.States (2), (1 => 7), 5, 13);
      Add_Action (Table.States (2), (1 => 8), 6, 14);
      Add_Action (Table.States (2), (1 => 9), 7, 15);
      Add_Action (Table.States (2), (3, 4), 22, 16);
      Add_Error (Table.States (2));
      Add_Goto (Table.States (2), 2, 28, 17);
      Table.States (3).Productions := WisiToken.LR.To_Vector ((1 => 34));
      Add_Action (Table.States (3), 8, Reduce, 34, 35, 1, 2, null, null);
      Add_Action (Table.States (3), 17, Reduce, 34, 35, 1, 2, null, null);
      Add_Action (Table.States (3), 18, Reduce, 34, 35, 1, 2, null, null);
      Add_Action (Table.States (3), 22, Reduce, 34, 35, 1, 2, null, null);
      Add_Action (Table.States (3), 25, Reduce, 34, 35, 1, 2, null, null);
      Add_Error (Table.States (3));
      Table.States (4).Productions := WisiToken.LR.To_Vector ((1 => 21));
      Add_Action (Table.States (4), (1 => 21), 12, 10);
      Add_Error (Table.States (4));
      Table.States (5).Productions := WisiToken.LR.To_Vector ((1 => 33));
      Add_Action (Table.States (5), 8, Reduce, 33, 35, 1, 1, null, null);
      Add_Action (Table.States (5), 17, Reduce, 33, 35, 1, 1, null, null);
      Add_Action (Table.States (5), 18, Reduce, 33, 35, 1, 1, null, null);
      Add_Action (Table.States (5), 22, Reduce, 33, 35, 1, 1, null, null);
      Add_Action (Table.States (5), 25, Reduce, 33, 35, 1, 1, null, null);
      Add_Error (Table.States (5));
      Table.States (6).Productions := WisiToken.LR.To_Vector ((1 => 35));
      Add_Action (Table.States (6), 8, Reduce, 35, 35, 1, 3, null, null);
      Add_Action (Table.States (6), 17, Reduce, 35, 35, 1, 3, null, null);
      Add_Action (Table.States (6), 18, Reduce, 35, 35, 1, 3, null, null);
      Add_Action (Table.States (6), 22, Reduce, 35, 35, 1, 3, null, null);
      Add_Action (Table.States (6), 25, Reduce, 35, 35, 1, 3, null, null);
      Add_Error (Table.States (6));
      Table.States (7).Productions := WisiToken.LR.To_Vector ((1 => 36));
      Add_Action (Table.States (7), 8, Reduce, 36, 36, 1, 0, null, null);
      Add_Action (Table.States (7), 17, Reduce, 36, 36, 1, 0, null, null);
      Add_Action (Table.States (7), 18, Reduce, 36, 36, 1, 0, null, null);
      Add_Action (Table.States (7), 22, Reduce, 36, 36, 1, 0, null, null);
      Add_Action (Table.States (7), 25, Reduce, 36, 36, 1, 0, null, null);
      Add_Error (Table.States (7));
      Table.States (8).Productions := WisiToken.LR.To_Vector ((1, 37));
      Add_Action (Table.States (8), (1 => 32), 8, 1);
      Add_Action (Table.States (8), (2, 3, 4, 5, 6), 17, 2);
      Add_Action (Table.States (8), (1 => 34), 18, 3);
      Add_Action (Table.States (8), (1 => 21), 22, 4);
      Add_Action (Table.States (8), 25, Accept_It, 1, 26, 1, 0, null, null);
      Add_Error (Table.States (8));
      Add_Goto (Table.States (8), 33, 27, 5);
      Add_Goto (Table.States (8), 35, 31, 6);
      Add_Goto (Table.States (8), 37, 35, 9);
      Table.States (9).Productions := WisiToken.LR.To_Vector ((1 => 37));
      Add_Action (Table.States (9), 8, Reduce, 37, 36, 2, 1, null, null);
      Add_Action (Table.States (9), 17, Reduce, 37, 36, 2, 1, null, null);
      Add_Action (Table.States (9), 18, Reduce, 37, 36, 2, 1, null, null);
      Add_Action (Table.States (9), 22, Reduce, 37, 36, 2, 1, null, null);
      Add_Action (Table.States (9), 25, Reduce, 37, 36, 2, 1, null, null);
      Add_Error (Table.States (9));
      Table.States (10).Productions := WisiToken.LR.To_Vector ((1 => 21));
      Add_Action (Table.States (10), 11, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (10), 17, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (10), 19, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (10), (1 => 30), 22, 34);
      Add_Error (Table.States (10));
      Add_Goto (Table.States (10), 21, 32, 35);
      Add_Goto (Table.States (10), 22, 33, 36);
      Add_Goto (Table.States (10), 27, 34, 37);
      Table.States (11).Productions := WisiToken.LR.To_Vector ((1 => 6));
      Add_Action (Table.States (11), (1 => 6), 4, 33);
      Add_Error (Table.States (11));
      Table.States (12).Productions := WisiToken.LR.To_Vector ((1 => 5));
      Add_Action (Table.States (12), (1 => 5), 22, 32);
      Add_Error (Table.States (12));
      Table.States (13).Productions := WisiToken.LR.To_Vector ((1 => 7));
      Add_Action (Table.States (13), 22, Reduce, 7, 28, 1, 0, null, null);
      Add_Error (Table.States (13));
      Table.States (14).Productions := WisiToken.LR.To_Vector ((1 => 8));
      Add_Action (Table.States (14), (1 => 8), 16, 31);
      Add_Error (Table.States (14));
      Table.States (15).Productions := WisiToken.LR.To_Vector ((1 => 9));
      Add_Action (Table.States (15), (1 => 9), 16, 30);
      Add_Error (Table.States (15));
      Table.States (16).Productions := WisiToken.LR.To_Vector ((3, 4));
      Add_Action (Table.States (16), (1 => 20), 7, 19);
      Add_Action (Table.States (16), 8, Reduce, 4, 27, 2, 2, declaration_2'Access, null);
      Add_Action (Table.States (16), (1 => 16), 9, 20);
      Add_Action (Table.States (16), (1 => 12), 13, 21);
      Add_Action (Table.States (16), (1 => 14), 14, 22);
      Add_Action (Table.States (16), 17, Reduce, 4, 27, 2, 2, declaration_2'Access, null);
      Add_Action (Table.States (16), 18, Reduce, 4, 27, 2, 2, declaration_2'Access, null);
      Add_Action (Table.States (16), (1 => 17), 20, 23);
      Add_Action (Table.States (16), (1 => 15), 21, 24);
      Add_Action (Table.States (16), (1 => 13), 22, 25, 4, 27, 2, 2, declaration_2'Access, null);
      Add_Action (Table.States (16), (1 => 18), 23, 26);
      Add_Action (Table.States (16), (1 => 19), 24, 27);
      Add_Action (Table.States (16), 25, Reduce, 4, 27, 2, 2, declaration_2'Access, null);
      Add_Error (Table.States (16));
      Add_Goto (Table.States (16), 3, 29, 28);
      Add_Goto (Table.States (16), 10, 30, 29);
      Table.States (17).Productions := WisiToken.LR.To_Vector ((1 => 2));
      Add_Action (Table.States (17), (1 => 2), 22, 18);
      Add_Error (Table.States (17));
      Table.States (18).Productions := WisiToken.LR.To_Vector ((1 => 2));
      Add_Action (Table.States (18), (1 => 20), 7, 19);
      Add_Action (Table.States (18), (1 => 16), 9, 20);
      Add_Action (Table.States (18), (1 => 12), 13, 21);
      Add_Action (Table.States (18), (1 => 14), 14, 22);
      Add_Action (Table.States (18), (1 => 17), 20, 23);
      Add_Action (Table.States (18), (1 => 15), 21, 24);
      Add_Action (Table.States (18), (1 => 13), 22, 25);
      Add_Action (Table.States (18), (1 => 18), 23, 26);
      Add_Action (Table.States (18), (1 => 19), 24, 27);
      Add_Error (Table.States (18));
      Add_Goto (Table.States (18), 2, 29, 47);
      Add_Goto (Table.States (18), 10, 30, 29);
      Table.States (19).Productions := WisiToken.LR.To_Vector ((1 => 20));
      Add_Action (Table.States (19), 7, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 8, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 9, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 13, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 14, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 17, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 18, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 20, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 21, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 22, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 23, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 24, Reduce, 20, 30, 1, 8, null, null);
      Add_Action (Table.States (19), 25, Reduce, 20, 30, 1, 8, null, null);
      Add_Error (Table.States (19));
      Table.States (20).Productions := WisiToken.LR.To_Vector ((1 => 16));
      Add_Action (Table.States (20), 7, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 8, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 9, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 13, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 14, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 17, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 18, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 20, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 21, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 22, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 23, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 24, Reduce, 16, 30, 1, 4, null, null);
      Add_Action (Table.States (20), 25, Reduce, 16, 30, 1, 4, null, null);
      Add_Error (Table.States (20));
      Table.States (21).Productions := WisiToken.LR.To_Vector ((1 => 12));
      Add_Action (Table.States (21), 7, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 8, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 9, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 13, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 14, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 17, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 18, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 20, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 21, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 22, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 23, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 24, Reduce, 12, 30, 1, 0, null, null);
      Add_Action (Table.States (21), 25, Reduce, 12, 30, 1, 0, null, null);
      Add_Error (Table.States (21));
      Table.States (22).Productions := WisiToken.LR.To_Vector ((1 => 14));
      Add_Action (Table.States (22), 7, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 8, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 9, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 13, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 14, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 17, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 18, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 20, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 21, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 22, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 23, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 24, Reduce, 14, 30, 1, 2, null, null);
      Add_Action (Table.States (22), 25, Reduce, 14, 30, 1, 2, null, null);
      Add_Error (Table.States (22));
      Table.States (23).Productions := WisiToken.LR.To_Vector ((1 => 17));
      Add_Action (Table.States (23), 7, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 8, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 9, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 13, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 14, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 17, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 18, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 20, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 21, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 22, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 23, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 24, Reduce, 17, 30, 1, 5, null, null);
      Add_Action (Table.States (23), 25, Reduce, 17, 30, 1, 5, null, null);
      Add_Error (Table.States (23));
      Table.States (24).Productions := WisiToken.LR.To_Vector ((1 => 15));
      Add_Action (Table.States (24), 7, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 8, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 9, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 13, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 14, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 17, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 18, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 20, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 21, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 22, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 23, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 24, Reduce, 15, 30, 1, 3, null, null);
      Add_Action (Table.States (24), 25, Reduce, 15, 30, 1, 3, null, null);
      Add_Error (Table.States (24));
      Table.States (25).Productions := WisiToken.LR.To_Vector ((1 => 13));
      Add_Action (Table.States (25), 7, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 8, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 9, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 13, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 14, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 17, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 18, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 20, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 21, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 22, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 23, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 24, Reduce, 13, 30, 1, 1, null, null);
      Add_Action (Table.States (25), 25, Reduce, 13, 30, 1, 1, null, null);
      Add_Error (Table.States (25));
      Table.States (26).Productions := WisiToken.LR.To_Vector ((1 => 18));
      Add_Action (Table.States (26), 7, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 8, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 9, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 13, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 14, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 17, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 18, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 20, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 21, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 22, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 23, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 24, Reduce, 18, 30, 1, 6, null, null);
      Add_Action (Table.States (26), 25, Reduce, 18, 30, 1, 6, null, null);
      Add_Error (Table.States (26));
      Table.States (27).Productions := WisiToken.LR.To_Vector ((1 => 19));
      Add_Action (Table.States (27), 7, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 8, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 9, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 13, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 14, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 17, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 18, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 20, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 21, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 22, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 23, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 24, Reduce, 19, 30, 1, 7, null, null);
      Add_Action (Table.States (27), 25, Reduce, 19, 30, 1, 7, null, null);
      Add_Error (Table.States (27));
      Table.States (28).Productions := WisiToken.LR.To_Vector ((3, 11));
      Add_Action (Table.States (28), (1 => 20), 7, 19);
      Add_Action (Table.States (28), 8, Reduce, 3, 27, 3, 1, declaration_1'Access, null);
      Add_Action (Table.States (28), (1 => 16), 9, 20);
      Add_Action (Table.States (28), (1 => 12), 13, 21);
      Add_Action (Table.States (28), (1 => 14), 14, 22);
      Add_Action (Table.States (28), 17, Reduce, 3, 27, 3, 1, declaration_1'Access, null);
      Add_Action (Table.States (28), 18, Reduce, 3, 27, 3, 1, declaration_1'Access, null);
      Add_Action (Table.States (28), (1 => 17), 20, 23);
      Add_Action (Table.States (28), (1 => 15), 21, 24);
      Add_Action (Table.States (28), (1 => 13), 22, 25, 3, 27, 3, 1, declaration_1'Access, null);
      Add_Action (Table.States (28), (1 => 18), 23, 26);
      Add_Action (Table.States (28), (1 => 19), 24, 27);
      Add_Action (Table.States (28), 25, Reduce, 3, 27, 3, 1, declaration_1'Access, null);
      Add_Error (Table.States (28));
      Add_Goto (Table.States (28), 11, 30, 46);
      Table.States (29).Productions := WisiToken.LR.To_Vector ((1 => 10));
      Add_Action (Table.States (29), 7, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 8, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 9, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 13, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 14, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 17, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 18, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 20, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 21, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 22, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 23, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 24, Reduce, 10, 29, 1, 0, null, null);
      Add_Action (Table.States (29), 25, Reduce, 10, 29, 1, 0, null, null);
      Add_Error (Table.States (29));
      Table.States (30).Productions := WisiToken.LR.To_Vector ((1 => 9));
      Add_Action (Table.States (30), (1 => 9), 22, 45);
      Add_Error (Table.States (30));
      Table.States (31).Productions := WisiToken.LR.To_Vector ((1 => 8));
      Add_Action (Table.States (31), (1 => 8), 22, 44);
      Add_Error (Table.States (31));
      Table.States (32).Productions := WisiToken.LR.To_Vector ((1 => 5));
      Add_Action (Table.States (32), (1 => 5), 14, 43);
      Add_Error (Table.States (32));
      Table.States (33).Productions := WisiToken.LR.To_Vector ((1 => 6));
      Add_Action (Table.States (33), 8, Reduce, 6, 27, 3, 4, declaration_4'Access, null);
      Add_Action (Table.States (33), 17, Reduce, 6, 27, 3, 4, declaration_4'Access, null);
      Add_Action (Table.States (33), 18, Reduce, 6, 27, 3, 4, declaration_4'Access, null);
      Add_Action (Table.States (33), 22, Reduce, 6, 27, 3, 4, declaration_4'Access, null);
      Add_Action (Table.States (33), 25, Reduce, 6, 27, 3, 4, declaration_4'Access, null);
      Add_Error (Table.States (33));
      Table.States (34).Productions := WisiToken.LR.To_Vector ((1 => 30));
      Add_Action (Table.States (34), 10, Reduce, 30, 34, 1, 0, null, null);
      Add_Action (Table.States (34), 11, Reduce, 30, 34, 1, 0, null, null);
      Add_Action (Table.States (34), 17, Reduce, 30, 34, 1, 0, null, null);
      Add_Action (Table.States (34), 19, Reduce, 30, 34, 1, 0, null, null);
      Add_Action (Table.States (34), 22, Reduce, 30, 34, 1, 0, null, null);
      Add_Error (Table.States (34));
      Table.States (35).Productions := WisiToken.LR.To_Vector ((21, 23, 24, 25));
      Add_Action (Table.States (35), (1 => 23), 11, 40);
      Add_Action (Table.States (35), (24, 25), 17, 41);
      Add_Action (Table.States (35), (1 => 21), 19, 42);
      Add_Error (Table.States (35));
      Table.States (36).Productions := WisiToken.LR.To_Vector ((1 => 22));
      Add_Action (Table.States (36), 11, Reduce, 22, 32, 1, 0, null, null);
      Add_Action (Table.States (36), 17, Reduce, 22, 32, 1, 0, null, null);
      Add_Action (Table.States (36), 19, Reduce, 22, 32, 1, 0, null, null);
      Add_Error (Table.States (36));
      Table.States (37).Productions := WisiToken.LR.To_Vector ((27, 28, 29, 31));
      Add_Action (Table.States (37), (28, 29), 10, 38);
      Add_Action (Table.States (37), 11, Reduce, 27, 33, 1, 1, null, null);
      Add_Action (Table.States (37), 17, Reduce, 27, 33, 1, 1, null, null);
      Add_Action (Table.States (37), 19, Reduce, 27, 33, 1, 1, null, null);
      Add_Action (Table.States (37), (1 => 31), 22, 39);
      Add_Error (Table.States (37));
      Table.States (38).Productions := WisiToken.LR.To_Vector ((28, 29));
      Add_Action (Table.States (38), (1 => 29), 10, 54);
      Add_Action (Table.States (38), 11, Reduce, 28, 33, 2, 2, null, null);
      Add_Action (Table.States (38), 17, Reduce, 28, 33, 2, 2, null, null);
      Add_Action (Table.States (38), 19, Reduce, 28, 33, 2, 2, null, null);
      Add_Error (Table.States (38));
      Table.States (39).Productions := WisiToken.LR.To_Vector ((1 => 31));
      Add_Action (Table.States (39), 10, Reduce, 31, 34, 2, 1, null, null);
      Add_Action (Table.States (39), 11, Reduce, 31, 34, 2, 1, null, null);
      Add_Action (Table.States (39), 17, Reduce, 31, 34, 2, 1, null, null);
      Add_Action (Table.States (39), 19, Reduce, 31, 34, 2, 1, null, null);
      Add_Action (Table.States (39), 22, Reduce, 31, 34, 2, 1, null, null);
      Add_Error (Table.States (39));
      Table.States (40).Productions := WisiToken.LR.To_Vector ((1 => 23));
      Add_Action (Table.States (40), 11, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (40), 17, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (40), 19, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (40), (1 => 30), 22, 34);
      Add_Error (Table.States (40));
      Add_Goto (Table.States (40), 23, 33, 53);
      Add_Goto (Table.States (40), 27, 34, 37);
      Table.States (41).Productions := WisiToken.LR.To_Vector ((24, 25));
      Add_Action (Table.States (41), (1 => 25), 3, 51);
      Add_Action (Table.States (41), (1 => 24), 4, 52);
      Add_Error (Table.States (41));
      Table.States (42).Productions := WisiToken.LR.To_Vector ((1 => 21));
      Add_Action (Table.States (42), 8, Reduce, 21, 31, 4, 0, nonterminal_0'Access, null);
      Add_Action (Table.States (42), 17, Reduce, 21, 31, 4, 0, nonterminal_0'Access, null);
      Add_Action (Table.States (42), 18, Reduce, 21, 31, 4, 0, nonterminal_0'Access, null);
      Add_Action (Table.States (42), 22, Reduce, 21, 31, 4, 0, nonterminal_0'Access, null);
      Add_Action (Table.States (42), 25, Reduce, 21, 31, 4, 0, nonterminal_0'Access, null);
      Add_Error (Table.States (42));
      Table.States (43).Productions := WisiToken.LR.To_Vector ((1 => 5));
      Add_Action (Table.States (43), (1 => 5), 22, 50);
      Add_Error (Table.States (43));
      Table.States (44).Productions := WisiToken.LR.To_Vector ((1 => 8));
      Add_Action (Table.States (44), (1 => 8), 15, 49);
      Add_Error (Table.States (44));
      Table.States (45).Productions := WisiToken.LR.To_Vector ((1 => 9));
      Add_Action (Table.States (45), (1 => 9), 15, 48);
      Add_Error (Table.States (45));
      Table.States (46).Productions := WisiToken.LR.To_Vector ((1 => 11));
      Add_Action (Table.States (46), 7, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 8, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 9, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 13, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 14, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 17, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 18, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 20, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 21, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 22, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 23, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 24, Reduce, 11, 29, 2, 1, null, null);
      Add_Action (Table.States (46), 25, Reduce, 11, 29, 2, 1, null, null);
      Add_Error (Table.States (46));
      Table.States (47).Productions := WisiToken.LR.To_Vector ((2, 11));
      Add_Action (Table.States (47), (1 => 20), 7, 19);
      Add_Action (Table.States (47), 8, Reduce, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Action (Table.States (47), (1 => 16), 9, 20);
      Add_Action (Table.States (47), (1 => 12), 13, 21);
      Add_Action (Table.States (47), (1 => 14), 14, 22);
      Add_Action (Table.States (47), 17, Reduce, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Action (Table.States (47), 18, Reduce, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Action (Table.States (47), (1 => 17), 20, 23);
      Add_Action (Table.States (47), (1 => 15), 21, 24);
      Add_Action (Table.States (47), (1 => 13), 22, 25, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Action (Table.States (47), (1 => 18), 23, 26);
      Add_Action (Table.States (47), (1 => 19), 24, 27);
      Add_Action (Table.States (47), 25, Reduce, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Error (Table.States (47));
      Add_Goto (Table.States (47), 11, 30, 46);
      Table.States (48).Productions := WisiToken.LR.To_Vector ((1 => 9));
      Add_Action (Table.States (48), 22, Reduce, 9, 28, 4, 2, null, null);
      Add_Error (Table.States (48));
      Table.States (49).Productions := WisiToken.LR.To_Vector ((1 => 8));
      Add_Action (Table.States (49), 22, Reduce, 8, 28, 4, 1, null, null);
      Add_Error (Table.States (49));
      Table.States (50).Productions := WisiToken.LR.To_Vector ((1 => 5));
      Add_Action (Table.States (50), 8, Reduce, 5, 27, 5, 3, declaration_3'Access, null);
      Add_Action (Table.States (50), 17, Reduce, 5, 27, 5, 3, declaration_3'Access, null);
      Add_Action (Table.States (50), 18, Reduce, 5, 27, 5, 3, declaration_3'Access, null);
      Add_Action (Table.States (50), 22, Reduce, 5, 27, 5, 3, declaration_3'Access, null);
      Add_Action (Table.States (50), 25, Reduce, 5, 27, 5, 3, declaration_3'Access, null);
      Add_Error (Table.States (50));
      Table.States (51).Productions := WisiToken.LR.To_Vector ((1 => 25));
      Add_Action (Table.States (51), (1 => 25), 4, 56);
      Add_Error (Table.States (51));
      Table.States (52).Productions := WisiToken.LR.To_Vector ((1 => 24));
      Add_Action (Table.States (52), (1 => 24), 22, 55);
      Add_Error (Table.States (52));
      Table.States (53).Productions := WisiToken.LR.To_Vector ((1 => 23));
      Add_Action (Table.States (53), 11, Reduce, 23, 32, 3, 1, null, null);
      Add_Action (Table.States (53), 17, Reduce, 23, 32, 3, 1, null, null);
      Add_Action (Table.States (53), 19, Reduce, 23, 32, 3, 1, null, null);
      Add_Error (Table.States (53));
      Table.States (54).Productions := WisiToken.LR.To_Vector ((1 => 29));
      Add_Action (Table.States (54), 11, Reduce, 29, 33, 3, 3, null, null);
      Add_Action (Table.States (54), 17, Reduce, 29, 33, 3, 3, null, null);
      Add_Action (Table.States (54), 19, Reduce, 29, 33, 3, 3, null, null);
      Add_Error (Table.States (54));
      Table.States (55).Productions := WisiToken.LR.To_Vector ((1 => 24));
      Add_Action (Table.States (55), (1 => 24), 14, 57);
      Add_Error (Table.States (55));
      Table.States (56).Productions := WisiToken.LR.To_Vector ((1 => 25));
      Add_Action (Table.States (56), 11, Reduce, 25, 32, 4, 3, null, null);
      Add_Action (Table.States (56), 17, Reduce, 25, 32, 4, 3, null, null);
      Add_Action (Table.States (56), 19, Reduce, 25, 32, 4, 3, null, null);
      Add_Error (Table.States (56));
      Table.States (57).Productions := WisiToken.LR.To_Vector ((1 => 24));
      Add_Action (Table.States (57), (1 => 24), 22, 58);
      Add_Error (Table.States (57));
      Table.States (58).Productions := WisiToken.LR.To_Vector ((1 => 24));
      Add_Action (Table.States (58), 11, Reduce, 24, 32, 6, 2, null, null);
      Add_Action (Table.States (58), 17, Reduce, 24, 32, 6, 2, null, null);
      Add_Action (Table.States (58), 19, Reduce, 24, 32, 6, 2, null, null);
      Add_Error (Table.States (58));

      WisiToken.LR.Parser_No_Recover.New_Parser
        (Parser,
         Trace,
         Lexer.New_Lexer (Trace),
         Table,
         User_Data,
         Max_Parallel         => 15,
         First_Parser_Label   => 0,
         Terminate_Same_State => True);
   end Create_Parser;
end Wisi_Grammar;
