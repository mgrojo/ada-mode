--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

pragma License (GPL);

package body FastToken.Token_Wisi is

   ----------
   --  body subprograms

      function To_Code (Nonterm : in Nonterminal_ID) return String
   is begin
      return Int_Image (Token_ID'Pos (Nonterm));
   end To_Code;

   function To_Codes (Tokens : in Token.List.Instance) return String
   is
      use Ada.Text_IO; -- Count
      use Token.List;
      --  A typical token image consists of:
      --
      --  ID           - int
      --  Region       - 2 ints
      --  Nonterminal  - t | nil
      --  Line         - int | nil
      --  First        - t | int | nil
      --  Comment_Line - int | nil
      --  Comment_End  - int | nil
      --  syntax : ( ( . ) )
      Chars_Per_Token : constant Integer := 13 + 7 * Integer'Width;

      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 18 + Tokens.Length * Chars_Per_Token);
      use Bounded;

      I          : List_Iterator  := Tokens.First;
      Token_Line : Bounded_String := To_Bounded_String ("[");

      function Append (Item : in Count) return String
      is begin
         Token_Line := Token_Line & (if Item = Null_Line then " nil" else Count'Image (Item));
      end Append;

   begin
      loop
         exit when I = Null_Iterator;
         declare
            use type Buffer_Region;
            Token : Instance renames Current (I);
         begin
            Token_Line := Token_Line & '(' & Int_Image (Token_ID'Pos (ID (I)));

            if Token.Region = Null_Buffer_Region then
               Token_Line := Token_Line & " nil";
            else
               Token_Line := Token_Line & " (" & Int_Image (Token.Region.Begin_Pos) & " . " &
                 --  Elisp region end is one past the last character
                 Int_Image (Token.Region.End_Pos + 1) & ")";
            end if;

            Token_Line := Token_Line & (if Token.Nonterminal then " t" else " nil");
            Append (Token.Line);
            Append (Token.First);
            Append (Token.Comment_Line);
            Token_Line := Token_Line & (if Token.Comment_End = Null_Position then " nil" else Integer'Image (Item));

            Token_Line := Token_Line & ")";
         end;

         Next (I);
         if I = Null_Iterator then
            Token_Line := Token_Line & "]";
         end if;
      end loop;
      return To_String (Token_Line);
   end To_Codes;

   ----------
   --  Spec visible subrograms

   procedure Push_Token
     (Token : in     Token_Pkg.Terminal_ID;
      State : access State_Type);

   function Merge_Tokens
     (Nonterm : in Token.Nonterminal_ID;
      Index   : in Natural;
      Tokens  : in Token.List.Instance;
      Action  : in Semantic_Action)
   is
      use List;
      use all type Ada.Text_IO.Count;
      I      : List_Iterator := Tokens.First;
      Result : Instance      := Default_Token;
   begin
      Result.ID          := Nonterm;
      Result.Nonterminal := True;

      if I = Null_Iterator then
         return Result;
      end if;

      loop
         exit when I = Null_Iterator;
         declare
            Token : Instance renames Current (I);
         begin
            if Result.Region.Begin_Pos > Token.Region.Begin_Pos then
               Result.Region.Begin_Pos := Token.Region.Begin_Pos;
            end if;

            if Result.Region.End_Pos < Token.Region.End_Pos then
               Result.Region.End_Pos := Token.Region.End_Pos;
            end if;

            if Result.Line = Null_Line then
               Result.Line := Token.Line;
            end if;

            if Result.First = Null_Line then
               if Token.Nonterminal then
                  Result.First := Token.First;
               else
                  Result.First := (if Token.First = Null_Line then Token.Comment_Line else Token.First);
               end if;
            end if;

            if Result.Comment_End = Null_Position then
               Result.Comment_End := Token.Comment_End;
            end if;
         end;

         Next (I);
      end loop;
      return Result;
   end Merge_Tokens;

   procedure Recover
     (Popped_Tokens  : in     Token_Pkg.List.Instance;
      Skipped_Tokens : in     Token_Pkg.List.Instance;
      Pushed_Token   : in     Token_Pkg.Nonterminal_ID;
      State          : access State_Type);

end FastToken.Token_Wisi;
