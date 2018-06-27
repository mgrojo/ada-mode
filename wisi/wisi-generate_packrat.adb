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

with Ada.Text_IO; use Ada.Text_IO;
with Wisi.Utils;  use Wisi.Utils;
with WisiToken.Generate.Packrat;
with WisiToken.Productions;
procedure Wisi.Generate_Packrat
  (Grammar      : in WisiToken.Productions.Prod_Arrays.Vector;
   Action_Names : in Names_Array_Array;
   Check_Names  : in Names_Array_Array;
   Descriptor   : in WisiToken.Descriptor)
is
   pragma Unreferenced (Check_Names);

   use WisiToken;

   subtype Terminal is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;
   subtype Nonterminal is Token_ID range Grammar.First_Index .. Grammar.Last_Index;

   Empty : constant Token_ID_Set (Nonterminal) := WisiToken.Generate.Has_Empty_Production (Grammar, Descriptor);

   Left_Recursive : constant Token_ID_Set (Nonterminal) := WisiToken.Generate.Packrat.Potential_Direct_Left_Recursive
     (Grammar, Empty);
   Right_Recursive : constant Token_ID_Set (Nonterminal) := WisiToken.Generate.Packrat.Potential_Direct_Right_Recursive
     (Grammar, Empty);

   --  FIXME: optimize memoizing? small productions not worth the memory cost?
   --  or just use langkit space optimization.

   function Parser_Name (Nonterm : in Token_ID) return String
   is begin
      return "Parse_" & Image (Nonterm, Descriptor);
   end Parser_Name;

   function Parser_Spec (Name : in String) return String
   is
   begin
      return "function " & Name & " (Parser : in out Parser_Type; Last_Pos : in Base_Token_Index) return Result_Type";
   end Parser_Spec;

   function Var_Suffix (I, J : in Integer) return String
   is begin
      return Trimmed_Image (I) & '_' & Trimmed_Image (J);
   end Var_Suffix;

   procedure Generate_Parser_Body (Prod : in Productions.Instance)
   is
      Result_ID : constant String := Trimmed_Image (Prod.LHS);
   begin
      --  We use gotos and function scope vars rather than nested if/declare
      --  to avoid excessive indenting for long productions.

      Indent_Line (Parser_Spec (Parser_Name (Prod.LHS)));
      Indent_Line ("is");
      Indent := Indent + 3;

      Indent_Line ("Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;");
      Indent_Line ("Start_Pos  : constant Token_Index := Last_Pos + 1;"); --  first token in current nonterm
      Indent_Line ("Pos        : Base_Token_Index := Last_Pos;"); --  last token parsed.

      for RHS_Index in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
         declare
            RHS : Productions.Right_Hand_Side renames Prod.RHSs (RHS_Index);
         begin
            for Token_Index in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
               if RHS.Tokens (Token_Index) in Descriptor.First_Terminal .. Descriptor.Last_Terminal then
                  Indent_Line ("Pos_" & Var_Suffix (RHS_Index, Token_Index) & "  : Token_Index;");
               else
                  Indent_Line ("Memo_" & Var_Suffix (RHS_Index, Token_Index) & " : Memo_Entry;");
               end if;
            end loop;
         end;
      end loop;

      if Left_Recursive (Prod.LHS) then
         Indent_Line ("Pos_Recurse_Last : Base_Token_Index := Last_Pos;");
         Indent_Line ("Result_Recurse   : Memo_Entry;");
      end if;

      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;

      Indent_Line ("if Pos = Parser.Terminals.Last_Index then");
      Indent_Line ("   return (State => Failure);");
      Indent_Line ("end if;");
      Indent_Line ("declare");
      Indent_Line ("   Memo : Memo_Entry renames Parser.Derivs (" & Result_ID & ")(Start_Pos);");
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("case Memo.State is");
      Indent_Line ("when Success =>");
      Indent_Line ("   return Parser.Derivs (" & Result_ID & ")(Start_Pos);");
      Indent_Line ("when Failure =>");
      Indent_Line ("   goto RHS_" & Trimmed_Image (Prod.RHSs.Last_Index) & "_Fail;");
      Indent_Line ("when No_Result =>");
      Indent_Line ("   if Memo.Recursive then");
      Indent_Start ("      raise Recursive with Image (" & Result_ID & ", Descriptor) &");
      Put_Line (" Token_Index'Image (Start_Pos) & "": recursive"";");
      Indent_Line ("   end if;");
      Indent_Line ("   Memo.Recursive := True;");
      Indent_Line ("end case;");
      Indent := Indent - 3;
      Indent_Line ("end;");
      New_Line;

      if Left_Recursive (Prod.LHS) then
         --  This is the top of the 'while' loop in [warth 2008] figure 3 Grow-LR.
         Indent_Line ("Parser.Derivs (" & Result_ID & ").Replace_Element (Start_Pos, (State => Failure));");
         Indent_Line ("<<Recurse_Start>>");
      end if;

      for RHS_Index in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
         declare
            RHS : Productions.Right_Hand_Side renames Prod.RHSs (RHS_Index);

            procedure Finish
            is
               use all type Standard.Ada.Containers.Count_Type;
            begin
               if Left_Recursive (Prod.LHS) then
                  Indent_Line ("Result_Recurse :=");
                  Indent := Indent + 2;
               else
                  Indent_Line ("Parser.Derivs (" & Result_ID & ").Replace_Element");
                  Indent_Line ("  (Start_Pos,");
                  Indent := Indent + 3;
               end if;
               Indent_Line ("(State              => Success,");
               Indent_Line (" Result             => Parser.Tree.Add_Nonterm");
               Indent_Line ("   (Production      => (" & Result_ID & ", " & Trimmed_Image (RHS_Index) & "),");
               Indent_Line
                 ("    Action          => " &
                    (if Action_Names (Prod.LHS) = null or else Action_Names (Prod.LHS)(RHS_Index) = null
                     then "null,"
                     else Action_Names (Prod.LHS)(RHS_Index).all & "'Access,"));

               Indent_Start ("    Children        => (");

               if RHS.Tokens.Length = 1 then
                  if RHS.Tokens (RHS.Tokens.First_Index) in Terminal then
                     Put ("1 => Tree_Index (Pos_" & Var_Suffix (RHS_Index, RHS.Tokens.First_Index) & ")),");
                  else
                     Put ("1 => Memo_" & Var_Suffix (RHS_Index, RHS.Tokens.First_Index) & ".Result),");
                  end if;

               else
                  for Token_Index in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                     if RHS.Tokens (Token_Index) in Terminal then
                        Put ("Tree_Index (Pos_" & Var_Suffix (RHS_Index, Token_Index) & ")");
                     else
                        Put ("Memo_" & Var_Suffix (RHS_Index, Token_Index) & ".Result");
                     end if;
                     if Token_Index = RHS.Tokens.Last_Index then
                        Put_Line ("),");
                     else
                        Put (", ");
                     end if;
                  end loop;
               end if;

               Indent_Line ("    Default_Virtual => False),");
               Indent_Start (" Last_Token      => Pos)");

               if Left_Recursive (Prod.LHS) then
                  Put_Line (";");
                  Indent := Indent - 2;
                  Indent_Line ("goto Finish;");
               else
                  Put_Line (");");
                  Indent := Indent - 3;
                  Indent_Line ("goto Succeed;");
               end if;
            end Finish;

         begin
            Indent_Line ("--  " & Productions.Image (Prod.LHS, RHS_Index, RHS.Tokens, Descriptor));
            Indent_Line ("Pos := Last_Pos;");

            for Token_Index in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
               declare
                  ID      : constant String := Trimmed_Image (RHS.Tokens (Token_Index));
                  Var_Suf : constant String := Var_Suffix (RHS_Index, Token_Index);
               begin
                  if RHS.Tokens (Token_Index) in Terminal then
                     Indent_Line ("if Parser.Terminals (Pos + 1).ID = " & ID & " then");
                     Indent := Indent + 3;
                     Indent_Line ("Pos := Pos + 1;");
                     Indent_Line ("Pos_" & Var_Suf & " := Pos;");
                     if Token_Index = RHS.Tokens.Last_Index then
                        Finish;
                     end if;
                     Indent := Indent - 3;
                     Indent_Line ("else");
                     Indent_Line ("   goto RHS_" & Trimmed_Image (RHS_Index) & "_Fail;");
                     Indent_Line ("end if;");

                  else -- nonterminal
                     Indent_Line
                       ("Memo_" & Var_Suf & " := Parse_" & Image (RHS.Tokens (Token_Index), Descriptor) &
                          " (Parser, Pos);");
                     Indent_Line ("case Result_States'(Memo_" & Var_Suf & ".State) is");
                     Indent_Line ("when Success =>");
                     Indent_Line ("   Pos := Memo_" & Var_Suf & ".Last_Token;");
                     if Token_Index = RHS.Tokens.Last_Index then
                        Finish;
                     end if;
                     Indent_Line ("when Failure =>");
                     Indent_Line ("   goto RHS_" & Trimmed_Image (RHS_Index) & "_Fail;");
                     Indent_Line ("end case;");
                  end if;
               end;
            end loop;

            Indent_Line ("<<RHS_" & Trimmed_Image (RHS_Index) & "_Fail>>");
            New_Line;
         end;
      end loop;

      --  We get here if the last alternative fails.
      if Left_Recursive (Prod.LHS) then
         Indent_Line ("Result_Recurse := (State => Failure);");
      else
         Indent_Line ("Parser.Derivs (" & Result_ID & ").Replace_Element (Start_Pos, (State => Failure));");

         Indent_Line ("if WisiToken.Trace_Parse > Detail then");
         Indent_Line
           ("   Parser.Trace.Put_Line (""" & Image (Prod.LHS, Descriptor) &
              """ & Token_Index'Image (Start_Pos) & "": fail"");");
         Indent_Line ("end if;");

         Indent_Line ("return Parser.Derivs (" & Result_ID & ")(Start_Pos);");
      end if;

      if Left_Recursive (Prod.LHS) then
         Indent_Line ("<<Finish>>");
         Indent_Line ("if Pos > Pos_Recurse_Last and Result_Recurse.State = Success then");
         --  made progress, keep looping
         Indent := Indent + 3;
         Indent_Line ("Parser.Derivs (" & Result_ID & ").Replace_Element (Start_Pos, Result_Recurse);");
         Indent_Line ("Pos_Recurse_Last := Pos;");
         Indent_Line ("if WisiToken.Trace_Parse > Detail then");
         Indent_Line
           ("   Parser.Trace.Put_Line (""" & Image (Prod.LHS, Descriptor) &
              """ & Token_Index'Image (Start_Pos) & "": recurse "" &");
         Indent_Line ("     Parser.Tree.Image (Result_Recurse.Result, Descriptor, Include_Children => True));");
         Indent_Line ("end if;");
         Indent_Line ("goto Recurse_Start;");
         Indent := Indent - 3;
         Indent_Line ("end if;");
      end if;
      New_Line;

      if not Left_Recursive (Prod.LHS) then
         Indent_Line ("<<Succeed>>");
      end if;

      Indent_Line ("if WisiToken.Trace_Parse > Detail then");
      Indent := Indent + 3;
      if Left_Recursive (Prod.LHS) then
         Indent_Line ("if Parser.Derivs (" & Result_ID & ")(Start_Pos).State = Failure then");
         Indent_Line
           ("   Parser.Trace.Put_Line (""" & Image (Prod.LHS, Descriptor) &
              """ & Token_Index'Image (Start_Pos) & "": fail"");");

         Indent_Line ("else");
         Indent := Indent + 3;

         Indent_Line ("Parser.Trace.Put_Line");
         Indent_Line
           ("  (""" & Image (Prod.LHS, Descriptor) & """ & Token_Index'Image (Start_Pos) & "": "" & Parser.Tree.Image");
         Indent_Line
           ("    (Parser.Derivs (" & Result_ID & ")(Start_Pos).Result, Descriptor, Include_Children => True));");

         Indent := Indent - 3;
         Indent_Line ("end if;");
      else
         Indent_Line ("Parser.Trace.Put_Line");
         Indent_Line
           ("  (""" & Image (Prod.LHS, Descriptor) & """ & Token_Index'Image (Start_Pos) & "": "" & Parser.Tree.Image");
         Indent_Line
           ("    (Parser.Derivs (" & Result_ID & ")(Start_Pos).Result, Descriptor, Include_Children => True));");
      end if;
      Indent := Indent - 3;
      Indent_Line ("end if;");

      Indent_Line ("return Parser.Derivs (" & Result_ID & ")(Start_Pos);");
      Indent := Indent - 3;
      Indent_Line ("end " & Parser_Name (Prod.LHS) & ";");
      New_Line;
   end Generate_Parser_Body;

begin
   Indent_Line ("use WisiToken;");
   Indent_Line ("use WisiToken.Parse.Packrat;");

   for Prod of Grammar loop
      if Left_Recursive (Prod.LHS) and Right_Recursive (Prod.LHS) then
         --  We only implement the simplest left recursion solution ([warth
         --  2008] figure 3); [tratt 2010] section 6.3 gives this condition for
         --  that to be valid. Indirect left recursion is detected at runtime.
         raise Grammar_Error with
           "'" & Image (Prod.LHS, Descriptor) & "' is both left and right recursive; not supported.";
      end if;
      Indent_Line (Parser_Spec (Parser_Name (Prod.LHS)) & ";");
   end loop;
   New_Line;

   for Prod of Grammar loop
      Generate_Parser_Body (Prod);
   end loop;
end Wisi.Generate_Packrat;
