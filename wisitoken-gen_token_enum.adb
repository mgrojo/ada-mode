--  Abstract :
--
--  See spec
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
package body WisiToken.Gen_Token_Enum is

   function Token_Enum_Image return Token_Array_String
   is
      Result : Token_Array_String (Token_ID'First .. +Last_Nonterminal);
   begin
      for I in Token_Enum_ID loop
         Result (+I) := new String'(Token_Enum_ID'Image (I));
      end loop;
      return Result;
   end Token_Enum_Image;

   function To_Syntax (Item : in Enum_Syntax) return WisiToken.Lexer.Regexp.Syntax
   is
      Result : WisiToken.Lexer.Regexp.Syntax (Token_ID'First .. +Last_Terminal);
   begin
      for I in Result'Range loop
         Result (I) := Item (-I);
      end loop;
      return Result;
   end To_Syntax;

   function "&" (Left, Right : in Token_Enum_ID) return WisiToken.Token.List.Instance
   is begin
      return WisiToken.Token.List."&" (+Left, +Right);
   end "&";

   function "&"
     (Left  : in WisiToken.Token.List.Instance;
      Right : in Token_Enum_ID)
     return WisiToken.Token.List.Instance
   is begin
      return WisiToken.Token.List."&" (Left, +Right);
   end "&";

   function "+" (Left : in Token_Enum_ID; Right : in Semantic_Action) return WisiToken.Production.Right_Hand_Side
   is begin
      return WisiToken.Production."+" (+Left, Right);
   end "+";

   function "<="
     (Left  : in Token_Enum_ID;
      Right : in WisiToken.Production.Right_Hand_Side)
     return WisiToken.Production.Instance
   is begin
      return WisiToken.Production."<=" (+Left, Right);
   end "<=";

   procedure Put
     (Trace        : in out WisiToken.Trace'Class;
      Nonterm      : in     Token_ID;
      Index        : in     Natural;
      Tokens       : in     Token.List.Instance;
      Include_Name : in     Boolean)
   is
      use Ada.Characters.Handling;
      use Token.List;

      Action_Name : constant String :=
        (if Include_Name
         then To_Lower (Image (Trace.Descriptor.all, Nonterm)) &
            "_" & Int_Image (Index) & ": "
         else "");
   begin
      Trace.Put (Action_Name & Image (Trace.Descriptor.all, Nonterm) & " <= ");
      Put (Trace, Tokens);
      Trace.New_Line;
   end Put;

   overriding procedure Error
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      Expecting : in              Token_ID_Set)
   is
      pragma Unreferenced (Parser_ID);
   begin
      --  Good enough for current unit tests.
      State.Expecting := Expecting;
   end Error;

   overriding procedure Reduce_Stack
     (State   : not null access State_Type;
      Nonterm : in              Token_ID;
      Index   : in              Natural;
      Tokens  : in              Token.List.Instance;
      Action  : in              Semantic_Action)
   is
      function To_Augmented (Item : in Token.List.Instance) return Augmented_Token_Array
      is
         use Token.List;

         Result : Augmented_Token_Array;
         I      : List_Iterator := Item.First;
      begin
         loop
            exit when Is_Null (I);
            Result.Append (Augmented_Token'(ID => ID (I), Enum_ID => -ID (I)));
            Next (I);
         end loop;
         return Result;
      end To_Augmented;

      Enum_Nonterm     : constant Token_Enum_ID         := -Nonterm;
      Augmented_Tokens : constant Augmented_Token_Array := To_Augmented (Tokens);
   begin
      if Trace_Parse > 0 then
         Put (State.Trace.all, Nonterm, Index, Tokens, Include_Name => Action /= null);
      end if;
      if Action /= null then
         Action (Augmented_Token'(ID => Nonterm, Enum_ID => Enum_Nonterm), Index, Augmented_Tokens);
      end if;
   end Reduce_Stack;

   function To_Nonterminal_Array_Token_Set
     (Item : in Nonterminal_Array_Token_Set)
     return WisiToken.Token_Array_Token_Set
   is
      Result : Token_Array_Token_Set :=
        (LR1_Descriptor.First_Nonterminal .. LR1_Descriptor.Last_Nonterminal =>
           (LR1_Descriptor.First_Terminal .. LR1_Descriptor.Last_Nonterminal => False));
   begin
      for I in Item'Range (1) loop
         for J in Item'Range (2) loop
            Result (+I, +J) := Item (I, J);
         end loop;
      end loop;
      return Result;
   end To_Nonterminal_Array_Token_Set;

   function To_Nonterminal_Array_Terminal_Set
     (Item : in Nonterminal_Array_Terminal_Set)
     return WisiToken.Token_Array_Token_Set
   is
      Result : Token_Array_Token_Set :=
        (LR1_Descriptor.First_Nonterminal .. LR1_Descriptor.Last_Nonterminal =>
           (LR1_Descriptor.First_Terminal .. LR1_Descriptor.Last_Terminal => False));
   begin
      for I in Item'Range (1) loop
         for J in Item'Range (2) loop
            Result (+I, +J) := Item (I, J);
         end loop;
      end loop;
      return Result;
   end To_Nonterminal_Array_Terminal_Set;

   function To_Token_ID_Set (Item : in Nonterminal_ID_Set) return WisiToken.Token_ID_Set
   is
      Result : WisiToken.Token_ID_Set :=
        (LR1_Descriptor.First_Nonterminal .. LR1_Descriptor.Last_Nonterminal => False);
   begin
      for I in Item'Range loop
         Result (+I) := Item (I);
      end loop;
      return Result;
   end To_Token_ID_Set;

   function "+" (Item : in Token_Array) return WisiToken.Token_ID_Set
   is
      Result : Token_ID_Set := (LR1_Descriptor.First_Terminal .. LR1_Descriptor.Last_Terminal => False);
   begin
      for I in Item'Range loop
         Result (+Item (I)) := True;
      end loop;
      return Result;
   end "+";

   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID_Set
   is begin
      return +Token_Array'(1 => Item);
   end "+";

end WisiToken.Gen_Token_Enum;
