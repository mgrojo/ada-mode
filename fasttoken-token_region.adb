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

with Ada.Characters.Handling;
package body FastToken.Token_Region is

   function Image (Item : in Token; ID_Only : in Boolean) return String
   is
      Name : constant String := Token_Pkg.Image (Item.ID);
   begin
      if ID_Only then
         return Name;

      elsif Item.Region = Null_Buffer_Region then
         return "(" & Name & ")";

      else
         --  For backward test result compatiblity:
         --  We don't call Image (Item.Region) here
         return "(" & Name &
           Integer'Image (Item.Region.Begin_Pos) & " ." & Integer'Image (Item.Region.End_Pos) & ")";
      end if;
   end Image;

   function Get (ID : in Token_Pkg.Grammar_ID) return Token
   is
      Result : Token := Default_Token;
   begin
      Result.ID := ID;
      return Result;
   end Get;

   procedure Push_Token
     (Token : in Token_Pkg.Grammar_ID;
      State : in State_Access;
      Lexer : in Token_Region.Lexer.Handle)
   is begin
      State.Stack.Append ((Token, Lexer.Bounds));
   end Push_Token;

   procedure Put_Trace (Nonterm : in Token; Index : in Natural; Tokens : in Token_List_Type)
   is
      use Ada.Characters.Handling;
      use Token_Stacks;

      Action_Name : constant String := To_Lower (Token_Pkg.Token_Image (Nonterm.ID)) &
        "_" & FastToken.Int_Image (Index);
      I : Cursor := Tokens.First;
   begin
      Token_Pkg.Put_Trace (Action_Name & ": " & Image (Nonterm, ID_Only => False) & " <= ");
      loop
         exit when I = No_Element;

         Token_Pkg.Put_Trace (Image (Tokens (I), ID_Only => False));
         Next (I);
         if I /= No_Element then
            Token_Pkg.Put_Trace (", ");
         end if;
      end loop;
   end Put_Trace;

   procedure Merge_Tokens
     (Nonterm : in Token_Pkg.Nonterminal_ID;
      Index   : in Natural;
      Tokens  : in Token_Pkg.List.Instance;
      Action  : in Semantic_Action;
      State   : in State_Access)
   is
      use all type Ada.Containers.Count_Type;
      use all type Token_Stacks.Cursor;
      use all type Token_Pkg.List.List_Iterator;

      ID_I        : Token_Pkg.List.List_Iterator := Tokens.First;
      Aug_Nonterm : Token                        := Default_Token;
      Stack_I     : Token_Stacks.Cursor          := State.Stack.To_Cursor (State.Stack.Length - Tokens.Length + 1);
      Aug_Tokens  : Token_List_Type;
   begin
      Aug_Nonterm.ID := Nonterm;

      loop
         exit when Is_Null (ID_I);
         declare
            use all type Token_Pkg.Token_ID;
            ID : Token_Pkg.Token_ID renames Current (ID_I);
         begin
            if ID /= State.Stack (Stack_I).ID then
               raise Programmer_Error;
            end if;

            if Action /= null then
               Aug_Tokens.Append (State.Stack (Stack_I));
            end if;

            if Aug_Nonterm.Region.Begin_Pos > State.Stack (Stack_I).Region.Begin_Pos then
               Aug_Nonterm.Region.Begin_Pos := State.Stack (Stack_I).Region.Begin_Pos;
            end if;

            if Aug_Nonterm.Region.End_Pos < State.Stack (Stack_I).Region.End_Pos then
               Aug_Nonterm.Region.End_Pos := State.Stack (Stack_I).Region.End_Pos;
            end if;
         end;

         Next (ID_I);
         Next (Stack_I);
      end loop;

      if Action /= null then
         Action (Aug_Nonterm, Index, Aug_Tokens);
         if Trace_Parse > 1 then
            Put_Trace (Aug_Nonterm, Index, Aug_Tokens);
         end if;
      end if;

      for I in 1 .. Tokens.Length loop
         State.Stack.Delete_Last;
      end loop;

      State.Stack.Append (Aug_Nonterm);
   end Merge_Tokens;

end FastToken.Token_Region;
