--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014, 2015, 2017  All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);
with Ada.Exceptions;
with Wisi.Utils;
package body Wisi.Gen_Generate_Utils is

   function Non_Reporting (Kind : in String) return Boolean
   is begin
      return
        Kind = """line_comment""" or
        Kind = """line_end""" or
        Kind = """whitespace""";
   end Non_Reporting;

   function Count_Non_Reporting return Integer
   is
      Result : Integer := 0;
   begin
      for Kind of Tokens loop
         if Non_Reporting (-Kind.Kind) then
            Result := Result + Integer (Kind.Tokens.Length);
         end if;
      end loop;
      return Result;
   end Count_Non_Reporting;

   function Find_Token_ID (Token : in String) return Token_ID
   is
      use type Standard.Ada.Strings.Unbounded.Unbounded_String;
      Result : Token_ID := Token_ID'First;
   begin
      --  Same order as set_token_images below.
      for Kind of Tokens loop
         if Non_Reporting (-Kind.Kind) then
            for Pair of Kind.Tokens loop
               if Pair.Name = Token then
                  return Result;
               end if;
               Result := Result + 1;
            end loop;
         end if;
      end loop;

      for Pair of Keywords loop
         if Pair.Name = Token then
            return Result;
         end if;
         Result := Result + 1;
      end loop;

      for Kind of Tokens loop
         if not Non_Reporting (-Kind.Kind) then
            for Pair of Kind.Tokens loop
               if Pair.Name = Token then
                  return Result;
               end if;
               Result := Result + 1;
            end loop;
         end if;
      end loop;

      if Token = EOI_Name or
        Token = "EOI" -- used in conflicts
      then
         return Result;
      end if;
      Result := Result + 1;

      for Rule of Rules loop
         if Rule.Left_Hand_Side = Token then
            return Result;
         end if;
         Result := Result + 1;
      end loop;

      if Token = FastToken_Accept_Name or
        Token = "fasttoken_accept"
      then
         return Result;
      end if;

      raise Not_Found with "token '" & Token & "' not found";
   end Find_Token_ID;

   function Set_Token_Images return ID_Array_Access_String_Pair_Type
   is
      ID           : Token_ID := Token_ID'First;
      Token_Images : ID_Array_Access_String_Pair_Type;
   begin
      --  same order as output_ada

      --  non-reporting
      for Kind of Tokens loop
         if Non_Reporting (-Kind.Kind) then
            for Pair of Kind.Tokens loop
               Token_Images (ID, WY)     := new String'(-Pair.Name);
               Token_Images (ID, Output) := new String'(To_Token_Out_Image (Pair.Name));

               ID := ID + 1;
            end loop;
         end if;
      end loop;

      for Pair of Keywords loop
         Token_Images (ID, WY)     := new String'(-Pair.Name);
         Token_Images (ID, Output) := new String'(To_Token_Out_Image (Pair.Name));

         ID := ID + 1;
      end loop;

      for Kind of Tokens loop
         if not Non_Reporting (-Kind.Kind) then
            for Pair of Kind.Tokens loop
               Token_Images (ID, WY)     := new String'(-Pair.Name);
               Token_Images (ID, Output) := new String'(To_Token_Out_Image (Pair.Name));

               ID := ID + 1;
            end loop;
         end if;
      end loop;

      if ID /= EOI_ID then raise Programmer_Error; end if;

      Token_Images (ID, WY)     := new String'(-EOI_Name);
      Token_Images (ID, Output) := new String'(To_Token_Out_Image (EOI_Name));

      ID := ID + 1;

      for Rule of Rules loop
         Token_Images (ID, WY)     := new String'(-Rule.Left_Hand_Side);
         Token_Images (ID, Output) := new String'(To_Token_Out_Image (Rule.Left_Hand_Side));

         ID := ID + 1;
      end loop;

      if ID /= Accept_ID then raise Programmer_Error; end if;

      Token_Images (ID, WY)     := new String'(-FastToken_Accept_Name);
      Token_Images (ID, Output) := new String'(To_Token_Out_Image (FastToken_Accept_Name));

      for ID in Token_Images'Range loop
         if Token_Images (ID, WY).all'Length > Token_WY_Image_Width then
            Token_WY_Image_Width := Token_Images (ID, WY).all'Length;
         end if;
      end loop;

      return Token_Images;
   end Set_Token_Images;

   function Non_Reporting (Cursor : in Token_Cursor) return Boolean
   is
      use Standard.Ada.Strings.Unbounded;
      --  WORKAROUND: in GNAT GPL_2014, using single statement here gives constraint error
      Token_Ref : constant Wisi.Token_Lists.Constant_Reference_Type := Wisi.Token_Lists.Constant_Reference
        (Tokens, Cursor.Token_Kind);

      Kind : constant String := To_String (Token_Ref.Element.Kind);
   begin
      return Non_Reporting (Kind);
   end Non_Reporting;

   function First_Token_Item (Cursor : in Token_Cursor) return String_Pair_Lists.Cursor
   is
      --  WORKAROUND: in GNAT GPL_2014, using single statement here gives discriminant error
      Token_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
        Wisi.Token_Lists.Constant_Reference (Tokens, Cursor.Token_Kind);
   begin
      --  WORKAROUND: in GNAT GPL_2014, using implicit dereference gives "token_ref unused" warning
      return Token_Ref.Element.Tokens.First;
   end First_Token_Item;

   function First return Token_Cursor
   is
      Cursor : Token_Cursor :=
        (State       => Non_Reporting,
         Token_Kind  => Tokens.First,
         Token_Item  => String_Pair_Lists.No_Element,
         Keyword     => String_Pair_Lists.No_Element,
         Nonterminal => Rule_Lists.No_Element);
   begin
      loop
         exit when not Wisi.Token_Lists.Has_Element (Cursor.Token_Kind);
         if Non_Reporting (Cursor) then
            Cursor.Token_Item := First_Token_Item (Cursor);
            if Wisi.String_Pair_Lists.Has_Element (Cursor.Token_Item) then
               exit;
            end if;
         end if;
         Wisi.Token_Lists.Next (Cursor.Token_Kind);
      end loop;

      if not Wisi.Token_Lists.Has_Element (Cursor.Token_Kind) then
         --  no non_reporting tokens
         Cursor :=
           (State       => Terminals_Keywords,
            Token_Kind  => Wisi.Token_Lists.No_Element,
            Token_Item  => String_Pair_Lists.No_Element,
            Keyword     => Keywords.First,
            Nonterminal => Rule_Lists.No_Element);

         if not String_Pair_Lists.Has_Element (Cursor.Keyword) then
            raise Programmer_Error with "no keyword tokens";
         end if;
      end if;

      return Cursor;
   end First;

   procedure Next (Cursor : in out Token_Cursor)
   is
   begin
      case Cursor.State is
      when Non_Reporting =>
         String_Pair_Lists.Next (Cursor.Token_Item);
         if String_Pair_Lists.Has_Element (Cursor.Token_Item) then
            return;
         else
            loop
               Wisi.Token_Lists.Next (Cursor.Token_Kind);
               exit when not Wisi.Token_Lists.Has_Element (Cursor.Token_Kind);

               if Non_Reporting (Cursor) then
                  Cursor.Token_Item := First_Token_Item (Cursor);
                  if String_Pair_Lists.Has_Element (Cursor.Token_Item) then
                     return;
                  end if;
               end if;
            end loop;
         end if;

         Cursor :=
           (State       => Terminals_Keywords,
            Token_Kind  => Wisi.Token_Lists.No_Element,
            Token_Item  => String_Pair_Lists.No_Element,
            Keyword     => Keywords.First,
            Nonterminal => Rule_Lists.No_Element);

         if not String_Pair_Lists.Has_Element (Cursor.Keyword) then
            raise Programmer_Error with "no keyword tokens";
         end if;

      when Terminals_Keywords =>
         --  Keywords before other terminals, so they have precedence over Identifiers

         String_Pair_Lists.Next (Cursor.Keyword);
         if String_Pair_Lists.Has_Element (Cursor.Keyword) then
            return;
         end if;

         --  Done with keywords; on to Terminals_Others
         Cursor :=
           (State       => Terminals_Others,
            Token_Kind  => Tokens.First,
            Token_Item  => String_Pair_Lists.No_Element,
            Keyword     => String_Pair_Lists.No_Element,
            Nonterminal => Rule_Lists.No_Element);

         loop
            exit when not Wisi.Token_Lists.Has_Element (Cursor.Token_Kind);

            if not Non_Reporting (Cursor) then
               Cursor.Token_Item := First_Token_Item (Cursor);
               return;
            end if;

            Wisi.Token_Lists.Next (Cursor.Token_Kind);
         end loop;

         --  no Terminals_Others; on to EOI
         Cursor :=
           (State       => EOI,
            Token_Kind  => Wisi.Token_Lists.No_Element,
            Token_Item  => String_Pair_Lists.No_Element,
            Keyword     => String_Pair_Lists.No_Element,
            Nonterminal => Rule_Lists.No_Element);

      when Terminals_Others =>
         Wisi.String_Pair_Lists.Next (Cursor.Token_Item);
         if Wisi.String_Pair_Lists.Has_Element (Cursor.Token_Item) then
            return;
         else
            loop
               Wisi.Token_Lists.Next (Cursor.Token_Kind);
               exit when not Wisi.Token_Lists.Has_Element (Cursor.Token_Kind);
               if not Non_Reporting (Cursor) then
                  Cursor.Token_Item := First_Token_Item (Cursor);
                  if Wisi.String_Pair_Lists.Has_Element (Cursor.Token_Item) then
                     return;
                  end if;
               end if;
            end loop;
         end if;

         Cursor :=
           (State       => EOI,
            Token_Kind  => Wisi.Token_Lists.No_Element,
            Token_Item  => String_Pair_Lists.No_Element,
            Keyword     => String_Pair_Lists.No_Element,
            Nonterminal => Rule_Lists.No_Element);

      when EOI =>
         if not Rule_Lists.Has_Element (Rules.First) then
            Cursor.State := Done;
         else
            Cursor :=
              (State       => FastToken_Accept,
               Token_Kind  => Wisi.Token_Lists.No_Element,
               Token_Item  => String_Pair_Lists.No_Element,
               Keyword     => String_Pair_Lists.No_Element,
               Nonterminal => Rule_Lists.No_Element);
         end if;

      when FastToken_Accept =>
         Cursor :=
           (State       => Nonterminal,
            Token_Kind  => Wisi.Token_Lists.No_Element,
            Token_Item  => String_Pair_Lists.No_Element,
            Keyword     => String_Pair_Lists.No_Element,
            Nonterminal => Rules.First);

         if not Rule_Lists.Has_Element (Cursor.Nonterminal) then
            Cursor.State := Done;
         end if;

      when Nonterminal =>
         Rule_Lists.Next (Cursor.Nonterminal);
         if not Rule_Lists.Has_Element (Cursor.Nonterminal) then
            Cursor.State := Done;
         end if;

      when Done =>
         null;
      end case;
   end Next;

   function Is_Done (Cursor : in out Token_Cursor) return Boolean
   is begin
      return Cursor.State = Done;
   end Is_Done;

   function Token_Name (Cursor : in out Token_Cursor) return Standard.Ada.Strings.Unbounded.Unbounded_String
   is begin
      case Cursor.State is
      when Non_Reporting | Terminals_Others =>
         declare
            Token_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
              Wisi.Token_Lists.Constant_Reference (Tokens, Cursor.Token_Kind);

            Item_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Token_Ref.Element.Tokens, Cursor.Token_Item);
         begin
            return Item_Ref.Element.Name;
         end;

      when Terminals_Keywords =>
         declare
            Keyword_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Keywords, Cursor.Keyword);
         begin
            return Keyword_Ref.Element.Name;
         end;

      when EOI =>
         return EOI_Name;

      when FastToken_Accept =>
         return FastToken_Accept_Name;

      when Nonterminal =>
         declare
            Rule_Ref : constant Rule_Lists.Constant_Reference_Type := Rule_Lists.Constant_Reference
              (Rules, Cursor.Nonterminal);
         begin
            return Rule_Ref.Element.Left_Hand_Side;
         end;

      when Done =>
         raise Programmer_Error with "token cursor is done";
      end case;
   end Token_Name;

   procedure Put_Tokens
   is
      use Standard.Ada.Text_IO;
   begin
      Put_Line ("Tokens:");
      for I in Token_ID'Range loop
         Put_Line (Token_ID'Image (I) & " => " & Token_WY_Image (I));
      end loop;
      New_Line;
   end Put_Tokens;

   function To_Conflicts
     (Accept_Reduce_Conflict_Count : out Integer;
      Shift_Reduce_Conflict_Count  : out Integer;
      Reduce_Reduce_Conflict_Count : out Integer)
     return Generator_Utils.Conflict_Lists.List
   is
      use Generator_Utils;
      use type LR.Unknown_State_Index;
      use type LR.Parse_Action_Verbs;
      Result   : Generator_Utils.Conflict_Lists.List;
      Conflict : Generator_Utils.Conflict;
   begin
      Accept_Reduce_Conflict_Count := 0;
      Shift_Reduce_Conflict_Count  := 0;
      Reduce_Reduce_Conflict_Count := 0;

      for Item of Conflicts loop
         Conflict :=
           (Conflict_Parse_Actions'Value (-Item.Action_A),
            Find_Token_ID (-Item.LHS_A),
            Conflict_Parse_Actions'Value (-Item.Action_B),
            Find_Token_ID (-Item.LHS_B),
            -1,
            Find_Token_ID (-Item.On));

         case Conflict.Action_A is
         when LR.Shift =>
            Shift_Reduce_Conflict_Count := Shift_Reduce_Conflict_Count + 1;
         when LR.Reduce =>
            Reduce_Reduce_Conflict_Count := Reduce_Reduce_Conflict_Count + 1;
         when LR.Accept_It =>
            Accept_Reduce_Conflict_Count := Reduce_Reduce_Conflict_Count + 1;
         end case;

         Result.Append (Conflict);
      end loop;
      return Result;
   exception
   when E : Not_Found =>
      raise FastToken.Grammar_Error with "known conflicts: " & Standard.Ada.Exceptions.Exception_Message (E);
   end To_Conflicts;

   function "&" (Tokens : in Token_Pkg.List.Instance; Token : in String) return Token_Pkg.List.Instance
   is
      use Token_Pkg.List;
   begin
      return Tokens & Token_Pkg.Get (Find_Token_ID (Token));
   end "&";

   function To_Grammar (Source_File_Name : in String; Start_Token : in String) return Production.List.Instance
   is
      use Production;
      use Token_Pkg.List;

      Self : Nonterminal_Pkg.Synthesize renames Nonterminal_Pkg.Synthesize_Self;
      Grammar : Production.List.Instance;
   begin
      begin
         Grammar := Production.List.Only
           (Nonterminal_Pkg.Get (Accept_ID) <= Nonterminal_Pkg.Get (Find_Token_ID (Start_Token)) &
              Token_Pkg.Get (EOI_ID) + Self);
      exception
      when Not_Found =>
         Wisi.Utils.Put_Error
           (Source_File_Name, First_Rule_Line, "start token '" & (Start_Token) & "' not found; need %start?");
         raise Syntax_Error;
      end;

      for Rule of Rules loop
         declare
            Index : Integer := 0;
         begin
            for Right_Hand_Side of Rule.Right_Hand_Sides loop
               declare
                  use Production.List;

                  Tokens : Token_Pkg.List.Instance;
               begin
                  for Token of Right_Hand_Side.Production loop
                     Tokens := Tokens & Token;
                  end loop;
                  Grammar := Grammar and Nonterminal_Pkg.Get (Find_Token_ID (-Rule.Left_Hand_Side)) <= Tokens + Index;
               exception
               when E : Not_Found =>
                  Wisi.Utils.Put_Error
                    (Source_File_Name, Rule.Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
                  raise Syntax_Error;
               end;
               Index := Index + 1;
            end loop;
         end;
      end loop;

      return Grammar;
   end To_Grammar;

   function To_Nonterminal_ID_Set (Item : in String_Lists.List) return Token_Pkg.Nonterminal_ID_Set
   is
      Result : Token_Pkg.Nonterminal_ID_Set := (others => False);
   begin
      for Token of Item loop
         Result (Find_Token_ID (Token)) := True;
      end loop;
      return Result;
   end To_Nonterminal_ID_Set;

   function To_State_Count (State_Last : in LR.State_Index) return LR.State_Index
   is
      use all type LR.Unknown_State_Index;
   begin
      return State_Last - LR.State_Index'First + 1;
   end To_State_Count;

begin
   if Verbosity > 0 then
      Put_Tokens;
   end if;

end Wisi.Gen_Generate_Utils;
