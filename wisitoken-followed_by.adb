--  Abstract :
--
--  Show productions where a token is followed by another token, to
--  track down the cause of grammar conflicts.
--
--  Design:
--
--  Consider test/bnf/optimized_conflict_01.wy as a simple example. State 19 is in part:
--
--  State 19:
--       14.0:subtype_indication <= IDENTIFIER RANGE simple_expression DOT_DOT simple_expression ^
--       15.1:simple_expression <= simple_expression ^ binary_adding_operator term
--       15.2:simple_expression <= simple_expression ^ binary_adding_operator simple_expression
--
--     PLUS                   => shift and goto state 14 18.0,
--                               reduce 5 tokens to subtype_indication 14.0
--
--  This says there is a legal derivation where a subtype_indication
--  is followed by a '+'. We want to find an example of that
--  derivation. To find it, we can follow these steps:
--
--  occurrences of PLUS in an RHS:               A: binary_adding_operator : '+' | '-' ;
--  occurrences of subtype_indication in an RHS: B: allocator : 'new' subtype_indication ;
--
--  connect A to B:
--  contains LHS of A: C: simple_expression : primary {binary_adding_operator primary} ;
--  contains LHS of B: D: primary : allocator ;
--  => C contains LHS of D.
--  expand C             : C1: simple_expression : allocator + primary ;
--  expand               : C2: simple_expression : 'new' subtype_indication + primary ;
--  that's the derivation we are looking for.

--  Copyright (C) 2020, 2022 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Command_Line;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Generate;
with WisiToken.Productions;
with WisiToken.Text_IO_Trace;
with WisiToken_Grammar_Runtime;
procedure WisiToken.Followed_By
is
   procedure Put_Usage
   is
      use Ada.Text_IO;
   begin
      Put_Line ("wisitoken-followed_by <grammar file> <token a> <token b> [verbosity]");
   end Put_Usage;

   Verbosity : Integer := 0;

   function Immediate_Last
     (Grammar              : in Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First_Terminal       : in Token_ID)
     return Token_Array_Token_Set
   --  Result (LHS) is the set of terminals and nonterminals that directly appear as
   --  the last token in any production for LHS.
   is
      function Last
        (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
         Has_Empty_Production : in Token_ID_Set;
         First_Terminal       : in Token_ID;
         Non_Terminal         : in Token_ID)
        return Token_ID_Set
      --  Result is the set of terminals and nonterminals that directly appear as
      --  the last token in any production for Non_Terminal.
      is begin
         return Result : Token_ID_Set := (First_Terminal .. Grammar.Last_Index => False) do
            declare
               Prod : WisiToken.Productions.Instance renames Grammar (Non_Terminal);
            begin
               for RHS of Prod.RHSs loop
                  for ID of reverse RHS.Tokens loop
                     Result (ID) := True;

                     if ID in Has_Empty_Production'Range and then Has_Empty_Production (ID) then
                        null;
                     else
                        exit;
                     end if;
                  end loop;
               end loop;
            end;
         end return;
      end Last;

      procedure Set_Slice (Result : in out Token_Array_Token_Set; I : Token_ID; Value : in Token_ID_Set)
      is begin
         for J in Result'Range (2) loop
            Result (I, J) := Value (J);
         end loop;
      end Set_Slice;

   begin
      return Result : Token_Array_Token_Set :=
        (Grammar.First_Index .. Grammar.Last_Index =>
           (First_Terminal .. Grammar.Last_Index => False))
      do
         for I in Result'Range loop
            declare
               Slice : constant Token_ID_Set := Last (Grammar, Has_Empty_Production, First_Terminal, I);
            begin
               Set_Slice (Result, I, Slice);
            end;
         end loop;
      end return;
   end Immediate_Last;

   function Transitive_Last
     (Grammar              : in Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First_Terminal       : in Token_ID)
     return Token_Array_Token_Set
   --  Result (LHS) is the set of terminals and nonterminals that may be
   --  the last token in any production for LHS.
   is
      function Last
        (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
         Has_Empty_Production : in Token_ID_Set;
         First_Terminal       : in Token_ID;
         Non_Terminal         : in Token_ID)
        return Token_ID_Set
      --  Result is the set of terminals and nonterminals that may be
      --  the last token in any production for Non_Terminal.
      is
         Search_Tokens : Token_ID_Set := (Grammar.First_Index .. Grammar.Last_Index => False);
      begin
         Search_Tokens (Non_Terminal) := True;

         return Result : Token_ID_Set := (First_Terminal .. Grammar.Last_Index => False) do
            while Any (Search_Tokens) loop
               declare
                  Added_Tokens   : Token_ID_Set := (First_Terminal .. Grammar.Last_Index      => False);
                  Added_Nonterms : Token_ID_Set := (Grammar.First_Index .. Grammar.Last_Index => False);
               begin
                  for Prod of Grammar loop
                     if Search_Tokens (Prod.LHS) then
                        for RHS of Prod.RHSs loop
                           for ID of reverse RHS.Tokens loop
                              if not Result (ID) then
                                 Added_Tokens (ID) := True;
                                 if ID in Added_Nonterms'Range then
                                    Added_Nonterms (ID) := True;
                                 end if;
                              end if;

                              if ID in Has_Empty_Production'Range and then Has_Empty_Production (ID) then
                                 null;
                              else
                                 exit;
                              end if;
                           end loop;
                        end loop;
                     end if;
                  end loop;

                  Result        := Result or Added_Tokens;
                  Search_Tokens := Added_Nonterms;
               end;
            end loop;
         end return;
      end Last;

      procedure Set_Slice (Result : in out Token_Array_Token_Set; I : Token_ID; Value : in Token_ID_Set)
      is begin
         for J in Result'Range (2) loop
            Result (I, J) := Value (J);
         end loop;
      end Set_Slice;

   begin
      return Result : Token_Array_Token_Set :=
        (Grammar.First_Index .. Grammar.Last_Index =>
           (First_Terminal .. Grammar.Last_Index => False))
      do
         for I in Result'Range loop
            declare
               Slice : constant Token_ID_Set := Last (Grammar, Has_Empty_Production, First_Terminal, I);
            begin
               Set_Slice (Result, I, Slice);
            end;
         end loop;
      end return;
   end Transitive_Last;

   Grammar_File_Name : Ada.Strings.Unbounded.Unbounded_String;
   Token_A_Name      : Ada.Strings.Unbounded.Unbounded_String;
   Token_B_Name      : Ada.Strings.Unbounded.Unbounded_String;
begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count not in 3 .. 4 then
         Put_Usage;
      end if;

      Grammar_File_Name := +Argument (1);
      Token_A_Name      := +Argument (2);
      Token_B_Name      := +Argument (3);

      if Argument_Count > 3 then
         Verbosity := Integer'Value (Argument (4));
      end if;
   end;

   declare
      use Ada.Text_IO;

      Input_Data : aliased WisiToken_Grammar_Runtime.User_Data_Type;
      Trace : WisiToken.Text_IO_Trace.Trace;

      Generate_Data : aliased WisiToken.BNF.Generate_Utils.Generate_Data :=
        WisiToken.BNF.Generate_Utils.Parse_Grammar_File
          (-Grammar_File_Name, Input_Data'Unchecked_Access, BNF.LALR, BNF.re2c_Lexer, Trace, Ignore_Conflicts => True);
      --  Builds Generate_Data.Descriptor, Generate_Data.Grammar

      Token_A : constant Token_ID := BNF.Generate_Utils.Find_Token_ID (Generate_Data, -Token_A_Name);
      Token_B : constant Token_ID := BNF.Generate_Utils.Find_Token_ID (Generate_Data, -Token_B_Name);

      Descriptor : WisiToken.Descriptor renames Generate_Data.Descriptor.all;

      Nullable : constant Token_Array_Production_ID := WisiToken.Generate.Nullable (Generate_Data.Grammar);
      Has_Empty_Production : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Nullable);

      First_Set : constant Token_Array_Token_Set := WisiToken.Generate.First
        (Generate_Data.Grammar, Has_Empty_Production, Descriptor.First_Terminal);

      Immediate_Last_Set : constant Token_Array_Token_Set := Immediate_Last
        (Generate_Data.Grammar, Has_Empty_Production, Descriptor.First_Terminal);

      Transitive_Last_Set : constant Token_Array_Token_Set := Transitive_Last
        (Generate_Data.Grammar, Has_Empty_Production, Descriptor.First_Terminal);

      function Followed_By (Token_A, Token_B : in Token_ID) return Production_ID
      is begin
         for LHS in Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index loop
            declare
               use WisiToken.Productions;
               Prod : Instance renames Generate_Data.Grammar (LHS);
            begin
               for I in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
                  declare
                     Tokens : Token_ID_Arrays.Vector renames Prod.RHSs (I).Tokens;
                  begin
                     for J in Tokens.First_Index .. Tokens.Last_Index loop
                        if Tokens (J) = Token_A or
                          (Tokens (J) in Transitive_Last_Set'Range (1) and then
                             Transitive_Last_Set (Tokens (J), Token_A))
                        then
                           if J < Tokens.Last_Index then
                              if Tokens (J + 1) in First_Set'Range (1) then
                                 if First_Set (Tokens (J + 1), Token_B) then
                                    return (LHS, I);
                                 end if;
                              elsif Tokens (J + 1) = Token_B then
                                 return (LHS, I);
                              end if;
                           end if;
                        end if;
                     end loop;
                  end;
               end loop;
            end;
         end loop;
         return Invalid_Production_ID;
      end Followed_By;

      Need_Comma : Boolean := False;

      procedure Put_Comma
      is begin
         if Need_Comma then
            Put (", ");
         else
            Need_Comma := True;
         end if;
      end Put_Comma;

   begin
      if Verbosity > 0 then
         Put_Line ("Immediate_Last_Set:");
         for I in Immediate_Last_Set'Range (1) loop
            Put (Image (I, Descriptor) & " =>");
            for J in Immediate_Last_Set'Range (2) loop
               if Immediate_Last_Set (I, J) then
                  Put (" " & Image (J, Descriptor));
               end if;
            end loop;
            New_Line;
         end loop;
         New_Line;
         Put_Line ("Transitive_Last_Set:");
         for I in Transitive_Last_Set'Range (1) loop
            Put (Image (I, Descriptor) & " =>");
            for J in Transitive_Last_Set'Range (2) loop
               if Transitive_Last_Set (I, J) then
                  Put (" " & Image (J, Descriptor));
               end if;
            end loop;
            New_Line;
         end loop;
      end if;

      New_Line;
      if Is_Terminal (Token_B, Descriptor) then
         Put_Line ("nonterminals where FIRST contains " & Image (Token_B, Descriptor) & ":");
         for I in First_Set'Range (1) loop
            if First_Set (I, Token_B) then
               Put (Image (I, Descriptor) & " ");
            end if;
         end loop;
         New_Line (2);
      end if;

      Put_Line ("Last path of " & Image (Token_A, Descriptor) & " " & Image (Token_B, Descriptor) & ":");
      declare
         Last_Set     : Token_ID_Set (Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index)
           := (others => False);
         New_Last_Set : Token_ID_Set (Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index)
           := (others => False);
         Prod         : Production_ID := Invalid_Production_ID;
      begin
         Last_Set (Token_A) := True;
         loop
            New_Last_Set := (others => False);

            for LHS in Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index loop
               declare
                  F : constant Production_ID := Followed_By (LHS, Token_B);
               begin
                  if F /= Invalid_Production_ID and then
                    (for some L in Last_Set'Range =>
                       Last_Set (L) and Immediate_Last_Set (LHS, L))
                  then
                     New_Last_Set (LHS) := True;
                     Put_Comma;
                     Put
                       ((if Prod = Invalid_Production_ID or F /= Prod
                         then Image (F.LHS, Descriptor) & "." & Trimmed_Image (F.RHS) & ": "
                         else "") &
                          Image (LHS, Descriptor));

                     if Prod = Invalid_Production_ID then
                        --  So far, there is mostly only one production_id involved
                        Prod := F;
                     else
                        raise SAL.Not_Implemented;
                     end if;
                  end if;
               end;
            end loop;
            New_Line;
            Need_Comma := False;
            exit when (for all I of New_Last_Set => not I);
            exit when New_Last_Set (Token_A);

            Last_Set := New_Last_Set;
         end loop;
      end;
   end;
end WisiToken.Followed_By;
