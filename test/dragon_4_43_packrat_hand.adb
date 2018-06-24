--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Checks;
with Ada.Characters.Latin_1;
with Ada.Characters;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with WisiToken.Gen_Token_Enum;
with WisiToken.Lexer.Regexp;
with WisiToken.Packrat;
with WisiToken.Parse;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
package body Dragon_4_43_Packrat_Hand is

   --  grammar in eqn (4.21) example 4.42 pg 231

   type Token_Enum_ID is
     (
      --  terminals
      Lower_C_ID,
      Lower_D_ID,
      EOF_ID,

      --  non-terminals
      Accept_ID,
      Upper_S_ID,
      Upper_C_ID);

   package Token_Enum is new WisiToken.Gen_Token_Enum
     (Token_Enum_ID     => Token_Enum_ID,
      First_Terminal    => Lower_C_ID,
      Last_Terminal     => EOF_ID,
      First_Nonterminal => Accept_ID,
      Last_Nonterminal  => Upper_C_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => Accept_ID,
      Case_Insensitive  => False);
   use Token_Enum;

   Null_Action : WisiToken.Syntax_Trees.Semantic_Action renames WisiToken.Syntax_Trees.Null_Action;

   --  Grammar : constant WisiToken.Productions.Arrays.Vector :=
   --    Accept_ID <= Upper_S_ID & EOF_ID + Null_Action -- 1
   --    and
   --    Upper_S_ID <= Upper_C_ID & Upper_C_ID + Null_Action -- 2
   --    and
   --    Upper_C_ID <= Lower_C_ID & Upper_C_ID + Null_Action -- 3
   --    and
   --    Upper_C_ID <= Lower_D_ID + Null_Action -- 4
   --    ;

   Syntax : constant WisiToken.Lexer.Regexp.Syntax := To_Syntax
     ((
       Lower_C_ID => WisiToken.Lexer.Regexp.Get ("c"),
       Lower_D_ID => WisiToken.Lexer.Regexp.Get ("d"),
       EOF_ID     => WisiToken.Lexer.Regexp.Get ("" & Ada.Characters.Latin_1.EOT)
      ));

   Trace : aliased WisiToken.Text_IO_Trace.Trace (LR1_Descriptor'Access);

   Lexer : constant WisiToken.Lexer.Handle := WisiToken.Lexer.Regexp.New_Lexer (Trace'Access, Syntax);

   use WisiToken;
   use WisiToken.Packrat;

   type Derivs_Type is array (Nonterminal_Enum_ID) of Memos.Vector;

   type Parser_Type is record
      Derivs           : Derivs_Type;
      Terminals        : Base_Token_Arrays.Vector;
      Line_Begin_Token : Line_Begin_Token_Vectors.Vector;
      Base_Tree        : aliased Syntax_Trees.Base_Tree; --  FIXME: only need Base_Tree, unless for error handling?
      Tree             : Syntax_Trees.Tree;
      User_Data        : Syntax_Trees.User_Data_Access := null;
   end record;

   function Parse_Upper_S (Parser : in out Parser_Type; Pos : in Token_Index) return Result_Type;
   function Parse_Upper_C (Parser : in out Parser_Type; Pos : in Token_Index) return Result_Type;

   function Parse_Accept (Parser : in out Parser_Type; Pos : in Token_Index) return Result_Type
   is begin
      case Parser.Derivs (Accept_ID)(Pos).State is
      when Success | Failure =>
         raise Programmer_Error with "called Parse_Accept when already parsed";

      when No_Result =>
         null;
      end case;

      declare
         Memo_1 : constant Result_Type := Parse_Upper_S (Parser, Pos);
      begin
         case Result_States'(Memo_1.State) is
         when Success =>
            if Parser.Terminals (Memo_1.Last_Token + 1).ID = +EOF_ID then
               Parser.Derivs (Accept_ID).Replace_Element
                 (Pos,
                  (State              => Success,
                   Result             => Parser.Tree.Add_Nonterm
                     (Production      => (+Accept_ID, 0),
                      Action          => Null_Action,
                      Children        =>
                        (1            => Memo_1.Result,
                         2            => Syntax_Trees.Valid_Node_Index (Memo_1.Last_Token + 1)),
                      Default_Virtual => False),
                   Last_Token         => Memo_1.Last_Token + 1));
            else
               Parser.Derivs (Accept_ID).Replace_Element (Pos, (State => Failure));
            end if;

         when Failure =>
            Parser.Derivs (Accept_ID).Replace_Element (Pos, (State => Failure));
         end case;

         return Parser.Derivs (Accept_ID).Constant_Ref (Pos);
      end;
   end Parse_Accept;

   function Parse_Upper_S (Parser : in out Parser_Type; Pos : in Token_Index) return Result_Type
   is begin
      case Parser.Derivs (Upper_S_ID)(Pos).State is
      when Success | Failure =>
         return Parser.Derivs (Upper_S_ID).Constant_Ref (Pos);

      when No_Result =>
         null;
      end case;

      declare
         Memo_1 : Memos.Constant_Reference_Type renames Parse_Upper_C (Parser, Pos);
      begin
         case Result_States'(Memo_1.State) is
         when Success =>
            declare
               Memo_2 : Memos.Constant_Reference_Type renames Parse_Upper_C (Parser, Memo_1.Last_Token + 1);
            begin
               case Result_States'(Memo_2.State) is
               when Success             =>
                  Parser.Derivs (Upper_S_ID).Replace_Element
                    (Pos,
                     (State              => Success,
                      Result             => Parser.Tree.Add_Nonterm
                        (Production      => (+Upper_S_ID, 0),
                         Action          => Null_Action,
                         Children        => (Memo_1.Result, Memo_2.Result),
                         Default_Virtual => False),
                      Last_Token         => Memo_2.Last_Token));

               when Failure =>
                  Parser.Derivs (Upper_S_ID).Replace_Element (Pos, (State => Failure));
               end case;
            end;

         when Failure =>
            Parser.Derivs (Upper_S_ID).Replace_Element (Pos, (State => Failure));
         end case;

         return Parser.Derivs (Upper_S_ID).Constant_Ref (Pos);
      end;
   end Parse_Upper_S;

   function Parse_Upper_C (Parser : in out Parser_Type; Pos : in Token_Index) return Result_Type
   is begin
      case Parser.Derivs (Upper_C_ID)(Pos).State is
      when Success | Failure =>
         return Parser.Derivs (Upper_C_ID).Constant_Ref (Pos);

      when No_Result =>
         null;
      end case;

      declare
         Memo_Or_1 : Memo_Entry;
      begin
         --  Upper_C_ID <= Lower_C_ID & Upper_C_ID
         if Parser.Terminals (Pos + 1).ID = +Lower_C_ID then
            declare
               Tree_Index_1 : constant Syntax_Trees.Valid_Node_Index := Syntax_Trees.Valid_Node_Index (Pos + 1);

               Memo_2 : Memos.Constant_Reference_Type renames Parse_Upper_C (Parser, Pos + 2);
            begin
               case Result_States'(Memo_2.State) is
               when Success             =>
                  Memo_Or_1 :=
                    (State              => Success,
                     Result             => Parser.Tree.Add_Nonterm
                       (Production      => (+Upper_C_ID, 0),
                        Action          => Null_Action,
                        Children        => (Tree_Index_1, Memo_2.Result),
                        Default_Virtual => False),
                     Last_Token         => Memo_2.Last_Token);

               when Failure =>
                  Memo_Or_1 := (State => Failure);
               end case;
            end;

         else
            Memo_Or_1 := (State => Failure);
         end if;

         if Memo_Or_1.State = Success then
            Parser.Derivs (Upper_C_ID).Replace_Element (Pos, Memo_Or_1);

         elsif Parser.Terminals (Pos + 1).ID = +Lower_D_ID then
            --  Upper_C_ID <= Lower_D_ID
            Parser.Derivs (Upper_C_ID).Replace_Element
              (Pos,
               (State              => Success,
                Result             => Parser.Tree.Add_Nonterm
                  (Production      => (+Upper_C_ID, 1),
                   Action          => Null_Action,
                   Children        => (1 => Syntax_Trees.Valid_Node_Index (Pos + 1)),
                   Default_Virtual => False),
                Last_Token         => Pos + 1));

         else
            Parser.Derivs (Upper_C_ID).Replace_Element (Pos, (State => Failure));
         end if;

         return Parser.Derivs (Upper_C_ID).Constant_Ref (Pos);
      end;
   end Parse_Upper_C;

   Parser : Parser_Type;

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Memo_State);

   ----------
   --  Test procedures

   procedure Test_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Execute_Parse
        (Input    : in String;
         Expected : in Result_States)
      is
         Junk : Syntax_Trees.Valid_Node_Index;
         pragma Unreferenced (Junk);
      begin
         Lexer.Reset_With_String (Input);
         Parser.Tree.Initialize (Parser.Base_Tree'Access, Flush => True);
         Parse.Lex_All (Lexer, Parser.Terminals, Parser.Line_Begin_Token, Parser.User_Data, Trace'Access);

         for Nonterm in Nonterminal_Enum_ID loop
            Parser.Derivs (Nonterm).Set_First (Parser.Terminals.First_Index);
            Parser.Derivs (Nonterm).Set_Last (Parser.Terminals.Last_Index);
         end loop;

         for Token_Index in Parser.Terminals.First_Index .. Parser.Terminals.Last_Index loop
            Junk := Parser.Tree.Add_Terminal (Token_Index, Parser.Terminals);
            --  FIXME: move this into Lex_All, delete Terminals, just use Syntax_Tree
         end loop;

         declare
            Result : constant Result_Type := Parse_Accept (Parser, Parser.Terminals.First_Index);
         begin
            Check (Input, Result.State, Expected);
            --  FIXME: check syntax_tree
         end;
      exception
      when E : others =>
         Ada.Text_IO.Put (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         AUnit.Assertions.Assert (False, "'" & Input & "': " & Ada.Exceptions.Exception_Message (E));
      end Execute_Parse;

   begin
      Execute_Parse ("cdcd", Success);
      Execute_Parse ("ccd", Failure);
   end Test_Parse;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("dragon_4_43_packrat_hand.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parse'Access, "Test_Parse");
   end Register_Tests;

end Dragon_4_43_Packrat_Hand;
