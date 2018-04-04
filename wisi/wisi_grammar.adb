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
     (Parser         :    out WisiToken.LR.Parser.Parser;
      Algorithm      : in     WisiToken.Parser_Algorithm_Type;
      Trace          : not null access WisiToken.Trace'Class;
      Language_Fixes : in     WisiToken.LR.Language_Fixes_Access;
      User_Data      : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use WisiToken.LR;
      use all type WisiToken.Parser_Algorithm_Type;
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
           (2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            1, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            2, 2),
         Delete =>
           (2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 4,
            4, 4, 4, 4, 4, 4, 4, 4,
            4, 4),
         Push_Back =>
           (1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1,
            1, 1),
         Undo_Reduce =>
           (1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1),
         Cost_Limit  => 5,
         Check_Limit => 4);


      Table.Productions.Set_Length (37);
      Set_Token_Sequence (Table.Productions (1), (36, 25));
      Set_Token_Sequence (Table.Productions (2), (17, 28, 22, 29));
      Set_Token_Sequence (Table.Productions (3), (17, 22, 29));
      Set_Token_Sequence (Table.Productions (4), (17, 22));
      Set_Token_Sequence (Table.Productions (5), (17, 4, 22, 14, 22));
      Set_Token_Sequence (Table.Productions (6), (17, 3, 4));
      Set_Token_Sequence (Table.Productions (7), (1 => 5));
      Set_Token_Sequence (Table.Productions (8), (6, 16, 22, 15));
      Set_Token_Sequence (Table.Productions (9), (7, 16, 22, 15));
      Set_Token_Sequence (Table.Productions (10), (1 => 30));
      Set_Token_Sequence (Table.Productions (11), (29, 30));
      Set_Token_Sequence (Table.Productions (12), (1 => 13));
      Set_Token_Sequence (Table.Productions (13), (1 => 22));
      Set_Token_Sequence (Table.Productions (14), (1 => 14));
      Set_Token_Sequence (Table.Productions (15), (1 => 21));
      Set_Token_Sequence (Table.Productions (16), (1 => 9));
      Set_Token_Sequence (Table.Productions (17), (1 => 20));
      Set_Token_Sequence (Table.Productions (18), (1 => 23));
      Set_Token_Sequence (Table.Productions (19), (1 => 24));
      Set_Token_Sequence (Table.Productions (20), (1 => 7));
      Set_Token_Sequence (Table.Productions (21), (22, 12, 32, 19));
      Set_Token_Sequence (Table.Productions (22), (1 => 33));
      Set_Token_Sequence (Table.Productions (23), (32, 11, 33));
      Set_Token_Sequence (Table.Productions (24), (32, 17, 4, 22, 14, 22));
      Set_Token_Sequence (Table.Productions (25), (32, 17, 3, 4));
      Set_Token_Sequence (Table.Productions (26), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Productions (27), (1 => 34));
      Set_Token_Sequence (Table.Productions (28), (34, 10));
      Set_Token_Sequence (Table.Productions (29), (34, 10, 10));
      Set_Token_Sequence (Table.Productions (30), (1 => 22));
      Set_Token_Sequence (Table.Productions (31), (34, 22));
      Set_Token_Sequence (Table.Productions (32), (1 => 8));
      Set_Token_Sequence (Table.Productions (33), (1 => 27));
      Set_Token_Sequence (Table.Productions (34), (1 => 18));
      Set_Token_Sequence (Table.Productions (35), (1 => 31));
      Set_Token_Sequence (Table.Productions (36), (1 => 35));
      Set_Token_Sequence (Table.Productions (37), (36, 35));

      Table.Terminal_Sequences.Set_First (26);
      Table.Terminal_Sequences.Set_Last (36);
      Set_Token_Sequence (Table.Terminal_Sequences (26), (18, 25));
      Set_Token_Sequence (Table.Terminal_Sequences (27), (17, 22));
      Set_Token_Sequence (Table.Terminal_Sequences (28), (1 => 5));
      Set_Token_Sequence (Table.Terminal_Sequences (29), (1 => 7));
      Set_Token_Sequence (Table.Terminal_Sequences (30), (1 => 7));
      Set_Token_Sequence (Table.Terminal_Sequences (31), (22, 12, 19));
      Set_Token_Sequence (Table.Terminal_Sequences (32), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Terminal_Sequences (33), (1 .. 0 => <>));
      Set_Token_Sequence (Table.Terminal_Sequences (34), (1 => 22));
      Set_Token_Sequence (Table.Terminal_Sequences (35), (1 => 18));
      Set_Token_Sequence (Table.Terminal_Sequences (36), (1 => 18));

      Add_Action (Table.States (0), 8, 1);
      Add_Action (Table.States (0), 17, 2);
      Add_Action (Table.States (0), 18, 3);
      Add_Action (Table.States (0), 22, 4);
      Add_Error (Table.States (0));
      Add_Goto (Table.States (0), 27, 5);
      Add_Goto (Table.States (0), 31, 6);
      Add_Goto (Table.States (0), 35, 7);
      Add_Goto (Table.States (0), 36, 8);
      Add_Action (Table.States (1), 8, Reduce, 32, 35, 1, 0, compilation_unit_0'Access, null);
      Add_Action (Table.States (1), 17, Reduce, 32, 35, 1, 0, compilation_unit_0'Access, null);
      Add_Action (Table.States (1), 18, Reduce, 32, 35, 1, 0, compilation_unit_0'Access, null);
      Add_Action (Table.States (1), 22, Reduce, 32, 35, 1, 0, compilation_unit_0'Access, null);
      Add_Action (Table.States (1), 25, Reduce, 32, 35, 1, 0, compilation_unit_0'Access, null);
      Add_Error (Table.States (1));
      Add_Action (Table.States (2), 3, 11);
      Add_Action (Table.States (2), 4, 12);
      Add_Action (Table.States (2), 5, 13);
      Add_Action (Table.States (2), 6, 14);
      Add_Action (Table.States (2), 7, 15);
      Add_Action (Table.States (2), 22, 16);
      Add_Error (Table.States (2));
      Add_Goto (Table.States (2), 28, 17);
      Add_Action (Table.States (3), 8, Reduce, 34, 35, 1, 2, null, null);
      Add_Action (Table.States (3), 17, Reduce, 34, 35, 1, 2, null, null);
      Add_Action (Table.States (3), 18, Reduce, 34, 35, 1, 2, null, null);
      Add_Action (Table.States (3), 22, Reduce, 34, 35, 1, 2, null, null);
      Add_Action (Table.States (3), 25, Reduce, 34, 35, 1, 2, null, null);
      Add_Error (Table.States (3));
      Add_Action (Table.States (4), 12, 10);
      Add_Error (Table.States (4));
      Add_Action (Table.States (5), 8, Reduce, 33, 35, 1, 1, null, null);
      Add_Action (Table.States (5), 17, Reduce, 33, 35, 1, 1, null, null);
      Add_Action (Table.States (5), 18, Reduce, 33, 35, 1, 1, null, null);
      Add_Action (Table.States (5), 22, Reduce, 33, 35, 1, 1, null, null);
      Add_Action (Table.States (5), 25, Reduce, 33, 35, 1, 1, null, null);
      Add_Error (Table.States (5));
      Add_Action (Table.States (6), 8, Reduce, 35, 35, 1, 3, null, null);
      Add_Action (Table.States (6), 17, Reduce, 35, 35, 1, 3, null, null);
      Add_Action (Table.States (6), 18, Reduce, 35, 35, 1, 3, null, null);
      Add_Action (Table.States (6), 22, Reduce, 35, 35, 1, 3, null, null);
      Add_Action (Table.States (6), 25, Reduce, 35, 35, 1, 3, null, null);
      Add_Error (Table.States (6));
      Add_Action (Table.States (7), 8, Reduce, 36, 36, 1, 0, null, null);
      Add_Action (Table.States (7), 17, Reduce, 36, 36, 1, 0, null, null);
      Add_Action (Table.States (7), 18, Reduce, 36, 36, 1, 0, null, null);
      Add_Action (Table.States (7), 22, Reduce, 36, 36, 1, 0, null, null);
      Add_Action (Table.States (7), 25, Reduce, 36, 36, 1, 0, null, null);
      Add_Error (Table.States (7));
      Add_Action (Table.States (8), 8, 1);
      Add_Action (Table.States (8), 17, 2);
      Add_Action (Table.States (8), 18, 3);
      Add_Action (Table.States (8), 22, 4);
      Add_Action (Table.States (8), 25, Accept_It, 1, 26, 1, 0, null, null);
      Add_Error (Table.States (8));
      Add_Goto (Table.States (8), 27, 5);
      Add_Goto (Table.States (8), 31, 6);
      Add_Goto (Table.States (8), 35, 9);
      Add_Action (Table.States (9), 8, Reduce, 37, 36, 2, 1, null, null);
      Add_Action (Table.States (9), 17, Reduce, 37, 36, 2, 1, null, null);
      Add_Action (Table.States (9), 18, Reduce, 37, 36, 2, 1, null, null);
      Add_Action (Table.States (9), 22, Reduce, 37, 36, 2, 1, null, null);
      Add_Action (Table.States (9), 25, Reduce, 37, 36, 2, 1, null, null);
      Add_Error (Table.States (9));
      Add_Action (Table.States (10), 11, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (10), 17, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (10), 19, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (10), 22, 34);
      Add_Error (Table.States (10));
      Add_Goto (Table.States (10), 32, 35);
      Add_Goto (Table.States (10), 33, 36);
      Add_Goto (Table.States (10), 34, 37);
      Add_Action (Table.States (11), 4, 33);
      Add_Error (Table.States (11));
      Add_Action (Table.States (12), 22, 32);
      Add_Error (Table.States (12));
      Add_Action (Table.States (13), 22, Reduce, 7, 28, 1, 0, null, null);
      Add_Error (Table.States (13));
      Add_Action (Table.States (14), 16, 31);
      Add_Error (Table.States (14));
      Add_Action (Table.States (15), 16, 30);
      Add_Error (Table.States (15));
      Add_Action (Table.States (16), 7, 19);
      Add_Action (Table.States (16), 8, Reduce, 4, 27, 2, 2, declaration_2'Access, null);
      Add_Action (Table.States (16), 9, 20);
      Add_Action (Table.States (16), 13, 21);
      Add_Action (Table.States (16), 14, 22);
      Add_Action (Table.States (16), 17, Reduce, 4, 27, 2, 2, declaration_2'Access, null);
      Add_Action (Table.States (16), 18, Reduce, 4, 27, 2, 2, declaration_2'Access, null);
      Add_Action (Table.States (16), 20, 23);
      Add_Action (Table.States (16), 21, 24);
      Add_Action (Table.States (16), 22, 25, 4, 27, 2, 2, declaration_2'Access, null);
      Add_Action (Table.States (16), 23, 26);
      Add_Action (Table.States (16), 24, 27);
      Add_Action (Table.States (16), 25, Reduce, 4, 27, 2, 2, declaration_2'Access, null);
      Add_Error (Table.States (16));
      Add_Goto (Table.States (16), 29, 28);
      Add_Goto (Table.States (16), 30, 29);
      Add_Action (Table.States (17), 22, 18);
      Add_Error (Table.States (17));
      Add_Action (Table.States (18), 7, 19);
      Add_Action (Table.States (18), 9, 20);
      Add_Action (Table.States (18), 13, 21);
      Add_Action (Table.States (18), 14, 22);
      Add_Action (Table.States (18), 20, 23);
      Add_Action (Table.States (18), 21, 24);
      Add_Action (Table.States (18), 22, 25);
      Add_Action (Table.States (18), 23, 26);
      Add_Action (Table.States (18), 24, 27);
      Add_Error (Table.States (18));
      Add_Goto (Table.States (18), 29, 47);
      Add_Goto (Table.States (18), 30, 29);
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
      Add_Action (Table.States (28), 7, 19);
      Add_Action (Table.States (28), 8, Reduce, 3, 27, 3, 1, declaration_1'Access, null);
      Add_Action (Table.States (28), 9, 20);
      Add_Action (Table.States (28), 13, 21);
      Add_Action (Table.States (28), 14, 22);
      Add_Action (Table.States (28), 17, Reduce, 3, 27, 3, 1, declaration_1'Access, null);
      Add_Action (Table.States (28), 18, Reduce, 3, 27, 3, 1, declaration_1'Access, null);
      Add_Action (Table.States (28), 20, 23);
      Add_Action (Table.States (28), 21, 24);
      Add_Action (Table.States (28), 22, 25, 3, 27, 3, 1, declaration_1'Access, null);
      Add_Action (Table.States (28), 23, 26);
      Add_Action (Table.States (28), 24, 27);
      Add_Action (Table.States (28), 25, Reduce, 3, 27, 3, 1, declaration_1'Access, null);
      Add_Error (Table.States (28));
      Add_Goto (Table.States (28), 30, 46);
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
      Add_Action (Table.States (30), 22, 45);
      Add_Error (Table.States (30));
      Add_Action (Table.States (31), 22, 44);
      Add_Error (Table.States (31));
      Add_Action (Table.States (32), 14, 43);
      Add_Error (Table.States (32));
      Add_Action (Table.States (33), 8, Reduce, 6, 27, 3, 4, declaration_4'Access, null);
      Add_Action (Table.States (33), 17, Reduce, 6, 27, 3, 4, declaration_4'Access, null);
      Add_Action (Table.States (33), 18, Reduce, 6, 27, 3, 4, declaration_4'Access, null);
      Add_Action (Table.States (33), 22, Reduce, 6, 27, 3, 4, declaration_4'Access, null);
      Add_Action (Table.States (33), 25, Reduce, 6, 27, 3, 4, declaration_4'Access, null);
      Add_Error (Table.States (33));
      Add_Action (Table.States (34), 10, Reduce, 30, 34, 1, 0, null, null);
      Add_Action (Table.States (34), 11, Reduce, 30, 34, 1, 0, null, null);
      Add_Action (Table.States (34), 17, Reduce, 30, 34, 1, 0, null, null);
      Add_Action (Table.States (34), 19, Reduce, 30, 34, 1, 0, null, null);
      Add_Action (Table.States (34), 22, Reduce, 30, 34, 1, 0, null, null);
      Add_Error (Table.States (34));
      Add_Action (Table.States (35), 11, 40);
      Add_Action (Table.States (35), 17, 41);
      Add_Action (Table.States (35), 19, 42);
      Add_Error (Table.States (35));
      Add_Action (Table.States (36), 11, Reduce, 22, 32, 1, 0, null, null);
      Add_Action (Table.States (36), 17, Reduce, 22, 32, 1, 0, null, null);
      Add_Action (Table.States (36), 19, Reduce, 22, 32, 1, 0, null, null);
      Add_Error (Table.States (36));
      Add_Action (Table.States (37), 10, 38);
      Add_Action (Table.States (37), 11, Reduce, 27, 33, 1, 1, null, null);
      Add_Action (Table.States (37), 17, Reduce, 27, 33, 1, 1, null, null);
      Add_Action (Table.States (37), 19, Reduce, 27, 33, 1, 1, null, null);
      Add_Action (Table.States (37), 22, 39);
      Add_Error (Table.States (37));
      Add_Action (Table.States (38), 10, 54);
      Add_Action (Table.States (38), 11, Reduce, 28, 33, 2, 2, null, null);
      Add_Action (Table.States (38), 17, Reduce, 28, 33, 2, 2, null, null);
      Add_Action (Table.States (38), 19, Reduce, 28, 33, 2, 2, null, null);
      Add_Error (Table.States (38));
      Add_Action (Table.States (39), 10, Reduce, 31, 34, 2, 1, null, null);
      Add_Action (Table.States (39), 11, Reduce, 31, 34, 2, 1, null, null);
      Add_Action (Table.States (39), 17, Reduce, 31, 34, 2, 1, null, null);
      Add_Action (Table.States (39), 19, Reduce, 31, 34, 2, 1, null, null);
      Add_Action (Table.States (39), 22, Reduce, 31, 34, 2, 1, null, null);
      Add_Error (Table.States (39));
      Add_Action (Table.States (40), 11, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (40), 17, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (40), 19, Reduce, 26, 33, 0, 0, null, null);
      Add_Action (Table.States (40), 22, 34);
      Add_Error (Table.States (40));
      Add_Goto (Table.States (40), 33, 53);
      Add_Goto (Table.States (40), 34, 37);
      Add_Action (Table.States (41), 3, 51);
      Add_Action (Table.States (41), 4, 52);
      Add_Error (Table.States (41));
      Add_Action (Table.States (42), 8, Reduce, 21, 31, 4, 0, nonterminal_0'Access, null);
      Add_Action (Table.States (42), 17, Reduce, 21, 31, 4, 0, nonterminal_0'Access, null);
      Add_Action (Table.States (42), 18, Reduce, 21, 31, 4, 0, nonterminal_0'Access, null);
      Add_Action (Table.States (42), 22, Reduce, 21, 31, 4, 0, nonterminal_0'Access, null);
      Add_Action (Table.States (42), 25, Reduce, 21, 31, 4, 0, nonterminal_0'Access, null);
      Add_Error (Table.States (42));
      Add_Action (Table.States (43), 22, 50);
      Add_Error (Table.States (43));
      Add_Action (Table.States (44), 15, 49);
      Add_Error (Table.States (44));
      Add_Action (Table.States (45), 15, 48);
      Add_Error (Table.States (45));
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
      Add_Action (Table.States (47), 7, 19);
      Add_Action (Table.States (47), 8, Reduce, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Action (Table.States (47), 9, 20);
      Add_Action (Table.States (47), 13, 21);
      Add_Action (Table.States (47), 14, 22);
      Add_Action (Table.States (47), 17, Reduce, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Action (Table.States (47), 18, Reduce, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Action (Table.States (47), 20, 23);
      Add_Action (Table.States (47), 21, 24);
      Add_Action (Table.States (47), 22, 25, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Action (Table.States (47), 23, 26);
      Add_Action (Table.States (47), 24, 27);
      Add_Action (Table.States (47), 25, Reduce, 2, 27, 4, 0, declaration_0'Access, null);
      Add_Error (Table.States (47));
      Add_Goto (Table.States (47), 30, 46);
      Add_Action (Table.States (48), 22, Reduce, 9, 28, 4, 2, null, null);
      Add_Error (Table.States (48));
      Add_Action (Table.States (49), 22, Reduce, 8, 28, 4, 1, null, null);
      Add_Error (Table.States (49));
      Add_Action (Table.States (50), 8, Reduce, 5, 27, 5, 3, declaration_3'Access, null);
      Add_Action (Table.States (50), 17, Reduce, 5, 27, 5, 3, declaration_3'Access, null);
      Add_Action (Table.States (50), 18, Reduce, 5, 27, 5, 3, declaration_3'Access, null);
      Add_Action (Table.States (50), 22, Reduce, 5, 27, 5, 3, declaration_3'Access, null);
      Add_Action (Table.States (50), 25, Reduce, 5, 27, 5, 3, declaration_3'Access, null);
      Add_Error (Table.States (50));
      Add_Action (Table.States (51), 4, 56);
      Add_Error (Table.States (51));
      Add_Action (Table.States (52), 22, 55);
      Add_Error (Table.States (52));
      Add_Action (Table.States (53), 11, Reduce, 23, 32, 3, 1, null, null);
      Add_Action (Table.States (53), 17, Reduce, 23, 32, 3, 1, null, null);
      Add_Action (Table.States (53), 19, Reduce, 23, 32, 3, 1, null, null);
      Add_Error (Table.States (53));
      Add_Action (Table.States (54), 11, Reduce, 29, 33, 3, 3, null, null);
      Add_Action (Table.States (54), 17, Reduce, 29, 33, 3, 3, null, null);
      Add_Action (Table.States (54), 19, Reduce, 29, 33, 3, 3, null, null);
      Add_Error (Table.States (54));
      Add_Action (Table.States (55), 14, 57);
      Add_Error (Table.States (55));
      Add_Action (Table.States (56), 11, Reduce, 25, 32, 4, 3, null, null);
      Add_Action (Table.States (56), 17, Reduce, 25, 32, 4, 3, null, null);
      Add_Action (Table.States (56), 19, Reduce, 25, 32, 4, 3, null, null);
      Add_Error (Table.States (56));
      Add_Action (Table.States (57), 22, 58);
      Add_Error (Table.States (57));
      Add_Action (Table.States (58), 11, Reduce, 24, 32, 6, 2, null, null);
      Add_Action (Table.States (58), 17, Reduce, 24, 32, 6, 2, null, null);
      Add_Action (Table.States (58), 19, Reduce, 24, 32, 6, 2, null, null);
      Add_Error (Table.States (58));

      WisiToken.LR.Parser.New_Parser
        (Parser,
         Trace,
         Lexer.New_Lexer (Trace),
         Table,
         Language_Fixes,
         User_Data,
         Max_Parallel         => 15,
         First_Parser_Label   => 0,
         Terminate_Same_State => True);
   end Create_Parser;
end Wisi_Grammar;
