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
with WisiToken.Productions;
with Wisi.Utils;  use Wisi.Utils;
procedure Wisi.Generate_Packrat_Parser
  (Grammar      : in WisiToken.Productions.Prod_Arrays.Vector;
   Action_Names : in Names_Array_Array;
   Check_Names  : in Names_Array_Array;
   Descriptor   : in WisiToken.Descriptor)
is
   pragma Unreferenced (Action_Names, Check_Names);

   use WisiToken;

   subtype Terminal is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;

   function Parser_Name (Nonterm : in Token_ID) return String
   is begin
      return "Parse_" & Image (Nonterm, Descriptor);
   end Parser_Name;

   function Parser_Spec (Name : in String) return String
   is
   begin
      return "function " & Name & " (Parser : in out Parser_Type; Start_Pos : in Token_Index) return Result_Type";
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

      Indent_Line ("Pos : Token_Index := Start_Pos;");

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

      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;

      Indent_Line ("if Pos > Parser.Terminals.Last_Index then");
      Indent_Line ("   return (State => Failure);");
      Indent_Line ("end if;");
      Indent_Line ("case Parser.Derivs (" & Result_ID & ")(Pos).State is");
      Indent_Line ("when Success | Failure =>");
      Indent_Line ("   return Parser.Derivs (" & Result_ID & ")(Pos);");
      Indent_Line ("when No_Result =>");
      Indent_Line ("   null;");
      Indent_Line ("end case;");
      New_Line;

      for RHS_Index in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
         declare
            RHS : Productions.Right_Hand_Side renames Prod.RHSs (RHS_Index);

            procedure Finish
            is
               use all type Standard.Ada.Containers.Count_Type;
            begin
               Indent_Line ("Parser.Derivs (" & Result_ID & ").Replace_Element");
               Indent_Line ("  (Start_Pos,");
               Indent_Line ("   (State              => Success,");
               Indent_Line ("    Result             => Parser.Tree.Add_Nonterm");
               Indent_Line ("      (Production      => (" & Result_ID & ", " & Trimmed_Image (RHS_Index) & "),");
               Indent_Line ("       Action          => null,"); --  FIXME: Action_Names
               Indent_Start ("       Children        => (");

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

               Indent_Line ("       Default_Virtual => False),");
               Indent_Line ("    Last_Token         => Pos));");
               Indent_Line ("return Parser.Derivs (" & Result_ID & ")(Start_Pos);");
            end Finish;
         begin
            Indent_Line ("--  " & Productions.Image (Prod.LHS, RHS_Index, RHS.Tokens, Descriptor));
            Indent_Line ("Pos := Start_Pos;");

            for Token_Index in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
               declare
                  ID      : constant String := Trimmed_Image (RHS.Tokens (Token_Index));
                  Var_Suf : constant String := Var_Suffix (RHS_Index, Token_Index);
               begin
                  if RHS.Tokens (Token_Index) in Terminal then
                     Indent_Line ("if Parser.Terminals (Pos).ID = " & ID & " then");
                     Indent := Indent + 3;
                     Indent_Line ("Pos_" & Var_Suf & " := Pos;");
                     Indent_Line ("Pos := Pos + 1;");
                     if Token_Index = RHS.Tokens.Last_Index then
                        Finish;
                     else
                        Indent_Line ("Pos := Pos + 1;");
                     end if;
                     Indent := Indent - 3;
                     Indent_Line ("else");
                     Indent := Indent + 3;
                     Indent_Line ("goto RHS_" & Trimmed_Image (RHS_Index) & "_Fail;");
                     Indent := Indent - 3;
                     Indent_Line ("end if;");

                  else -- nonterminal
                     Indent_Line
                       ("Memo_" & Var_Suf & " := Parse_" & Image (RHS.Tokens (Token_Index), Descriptor) &
                          " (Parser, Pos);");
                     Indent_Line ("case Result_States'(Memo_" & Var_Suf & ".State) is");
                     Indent_Line ("when Success =>");
                     Indent := Indent + 3;
                     Indent_Line ("Pos := Memo_" & Var_Suf & ".Last_Token + 1;");
                     if Token_Index = RHS.Tokens.Last_Index then
                        Finish;
                     else
                        Indent_Line ("Pos := Pos + 1;");
                     end if;
                     Indent := Indent - 3;
                     New_Line;

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

      Indent_Line ("Parser.Derivs (" & Result_ID & ").Replace_Element (Start_Pos, (State => Failure));");
      Indent_Line ("return Parser.Derivs (" & Result_ID & ")(Start_Pos);");

      Indent := Indent - 3;
      Indent_Line ("end " & Parser_Name (Prod.LHS) & ";");
      New_Line;
   end Generate_Parser_Body;

begin
   Indent_Line ("use WisiToken;");
   Indent_Line ("use WisiToken.Packrat;");

   for Prod of Grammar loop
      Indent_Line (Parser_Spec (Parser_Name (Prod.LHS)) & ";");
   end loop;
   New_Line;

   for Prod of Grammar loop
      Generate_Parser_Body (Prod);
   end loop;
end Wisi.Generate_Packrat_Parser;
