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
with WisiToken.Token;
package body Wisi.Gen_Generate_Utils is

   --  For Constant_Reference
   Aliased_EOI_Name              : aliased constant Standard.Ada.Strings.Unbounded.Unbounded_String := EOI_Name;
   Aliased_WisiToken_Accept_Name : aliased constant Standard.Ada.Strings.Unbounded.Unbounded_String :=
     WisiToken_Accept_Name;

   function Non_Reporting (Kind : in String) return Boolean
   is begin
      return
        Kind = "line_comment" or
        Kind = "line_end" or
        Kind = "whitespace";
   end Non_Reporting;

   function Count_Non_Reporting return Integer
   is
      --  It is tempting to rename this to Last_Non_Reporting, and
      --  return Token_ID, but many test grammars have no
      --  non-reporting tokens.
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
   is begin
      for Cursor in All_Tokens.Iterate loop
         if Name (Cursor) = Token then
            return ID (Cursor);
         end if;
      end loop;
      raise Not_Found with "token '" & Token & "' not found";
   end Find_Token_ID;

   function Name_1 (Cursor : in Token_Cursor) return String;

   procedure Set_Token_Images
   is begin
      LR1_Descriptor.Terminal_Image_Width := 0;
      LR1_Descriptor.Image_Width := 0;

      for Cursor in All_Tokens.Iterate loop
         LR1_Descriptor.Image (ID (Cursor)) := new String'(Name_1 (Cursor));
      end loop;

      for ID in LR1_Descriptor.Image'Range loop
         if ID in LR1_Descriptor.First_Terminal .. LR1_Descriptor.Last_Terminal then
            if LR1_Descriptor.Image (ID).all'Length > LR1_Descriptor.Terminal_Image_Width then
               LR1_Descriptor.Terminal_Image_Width := LR1_Descriptor.Image (ID).all'Length;
            end if;
         end if;

         if LR1_Descriptor.Image (ID).all'Length > LR1_Descriptor.Image_Width then
            LR1_Descriptor.Image_Width := LR1_Descriptor.Image (ID).all'Length;
         end if;
      end loop;

      LALR_Descriptor.Image                := LR1_Descriptor.Image;
      LALR_Descriptor.Terminal_Image_Width := LR1_Descriptor.Terminal_Image_Width;
      LALR_Descriptor.Image_Width          := LR1_Descriptor.Image_Width;
   end Set_Token_Images;

   function First_Token_Item (Cursor : in Token_Cursor) return String_Pair_Lists.Cursor
   is
      --  WORKAROUND: in GNAT GPL_2014, using single statement here gives discriminant error
      Token_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
        Wisi.Token_Lists.Constant_Reference (Tokens, Cursor.Token_Kind);
   begin
      --  WORKAROUND: in GNAT GPL_2014, using implicit dereference gives "token_ref unused" warning
      return Token_Ref.Element.Tokens.First;
   end First_Token_Item;

   function Constant_Reference
     (Container : aliased in Token_Container'Class;
      Cursor    :         in Token_Cursor)
     return Token_Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      case Cursor.State is
      when Non_Reporting | Terminals_Others =>
         declare
            Token_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
              Wisi.Token_Lists.Constant_Reference (Tokens, Cursor.Token_Kind);

            Item_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Token_Ref.Element.Tokens, Cursor.Token_Item);
         begin
            return (Element => Item_Ref.Element.all.Name'Access);
         end;

      when Terminals_Keywords =>
         declare
            Keyword_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Keywords, Cursor.Keyword);
         begin
            return (Element => Keyword_Ref.Element.all.Name'Access);
         end;

      when EOI =>
         return (Element => Aliased_EOI_Name'Access);

      when WisiToken_Accept =>
         return (Element => Aliased_WisiToken_Accept_Name'Access);

      when Nonterminal =>
         declare
            Rule_Ref : constant Rule_Lists.Constant_Reference_Type := Rule_Lists.Constant_Reference
              (Rules, Cursor.Nonterminal);
         begin
            return (Element => Rule_Ref.Element.all.Left_Hand_Side'Access);
         end;

      when Done =>
         raise Programmer_Error with "token cursor is done";
      end case;
   end Constant_Reference;

   type Token_Access_Constant is access constant Token_Container;
   type Iterator is new Iterator_Interfaces.Forward_Iterator with record
      Container     : Token_Access_Constant;
      Non_Reporting : Boolean;
      Other_Tokens  : Boolean; -- Stop after user-defined terminals.
   end record;

   overriding function First (Object : Iterator) return Token_Cursor;
   overriding function Next (Object : Iterator; Position : Token_Cursor) return Token_Cursor;

   overriding function First (Object : Iterator) return Token_Cursor
   is begin
      return First (Object.Non_Reporting);
   end First;

   overriding function Next (Object  : Iterator; Position : Token_Cursor) return Token_Cursor
   is
      Next_Position : Token_Cursor := Position;
   begin
      Next (Next_Position, Object.Other_Tokens);
      return Next_Position;
   end Next;

   function Iterate
     (Container     : aliased    Token_Container;
      Non_Reporting :         in Boolean := True;
      Other_Tokens  :         in Boolean := True)
     return Iterator_Interfaces.Forward_Iterator'Class
   is begin
      return Iterator'(Container'Access, Non_Reporting, Other_Tokens);
   end Iterate;

   function First (Non_Reporting : in Boolean) return Token_Cursor
   is
      Cursor : Token_Cursor :=
        (State       => Gen_Generate_Utils.Non_Reporting,
         ID          => Token_ID'First,
         Token_Kind  => Tokens.First,
         Token_Item  => String_Pair_Lists.No_Element,
         Keyword     => String_Pair_Lists.No_Element,
         Nonterminal => Rule_Lists.No_Element);
   begin
      if Non_Reporting then
         loop
            exit when not Wisi.Token_Lists.Has_Element (Cursor.Token_Kind);
            if Gen_Generate_Utils.Non_Reporting (-Wisi.Token_Lists.Element (Cursor.Token_Kind).Kind) then
               Cursor.Token_Item := First_Token_Item (Cursor);
               if Wisi.String_Pair_Lists.Has_Element (Cursor.Token_Item) then
                  exit;
               end if;
            end if;
            Wisi.Token_Lists.Next (Cursor.Token_Kind);
         end loop;
      else
         Cursor.Token_Kind := Wisi.Token_Lists.No_Element;
      end if;

      if not Wisi.Token_Lists.Has_Element (Cursor.Token_Kind) then
         --  There are no non_reporting tokens, or Non_Reporting false
         Cursor :=
           (State       => Terminals_Keywords,
            ID          => LR1_Descriptor.First_Terminal,
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

   procedure Next (Cursor : in out Token_Cursor; Other_Tokens : in Boolean)
   is begin
      Cursor.ID := Cursor.ID + 1;

      case Cursor.State is
      when Non_Reporting =>
         String_Pair_Lists.Next (Cursor.Token_Item);
         if String_Pair_Lists.Has_Element (Cursor.Token_Item) then
            return;
         else
            loop
               Wisi.Token_Lists.Next (Cursor.Token_Kind);
               exit when not Wisi.Token_Lists.Has_Element (Cursor.Token_Kind);

               if Non_Reporting (-Wisi.Token_Lists.Element (Cursor.Token_Kind).Kind) then
                  Cursor.Token_Item := First_Token_Item (Cursor);
                  if String_Pair_Lists.Has_Element (Cursor.Token_Item) then
                     return;
                  end if;
               end if;
            end loop;
         end if;

         Cursor :=
           (State       => Terminals_Keywords,
            ID          => Cursor.ID,
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
            ID          => Cursor.ID,
            Token_Kind  => Tokens.First,
            Token_Item  => String_Pair_Lists.No_Element,
            Keyword     => String_Pair_Lists.No_Element,
            Nonterminal => Rule_Lists.No_Element);

         loop
            exit when not Wisi.Token_Lists.Has_Element (Cursor.Token_Kind);

            if not Non_Reporting (-Wisi.Token_Lists.Element (Cursor.Token_Kind).Kind) then
               Cursor.Token_Item := First_Token_Item (Cursor);
               return;
            end if;

            Wisi.Token_Lists.Next (Cursor.Token_Kind);
         end loop;

         --  no Terminals_Others; on to EOI
         Cursor :=
           (State       => EOI,
            ID          => Cursor.ID,
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
               if not Non_Reporting (-Wisi.Token_Lists.Element (Cursor.Token_Kind).Kind) then
                  Cursor.Token_Item := First_Token_Item (Cursor);
                  if Wisi.String_Pair_Lists.Has_Element (Cursor.Token_Item) then
                     return;
                  end if;
               end if;
            end loop;
         end if;

         if not Other_Tokens then
            Cursor.State := Done;
         else
            Cursor :=
              (State       => EOI,
               ID          => Cursor.ID,
               Token_Kind  => Wisi.Token_Lists.No_Element,
               Token_Item  => String_Pair_Lists.No_Element,
               Keyword     => String_Pair_Lists.No_Element,
               Nonterminal => Rule_Lists.No_Element);
         end if;

      when EOI =>
         if not Rule_Lists.Has_Element (Rules.First) then
            Cursor.State := Done;
         else
            Cursor :=
              (State       => WisiToken_Accept,
               ID          => Cursor.ID,
               Token_Kind  => Wisi.Token_Lists.No_Element,
               Token_Item  => String_Pair_Lists.No_Element,
               Keyword     => String_Pair_Lists.No_Element,
               Nonterminal => Rule_Lists.No_Element);
         end if;

      when WisiToken_Accept =>
         Cursor :=
           (State       => Nonterminal,
            ID          => Cursor.ID,
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

   function Is_Done (Cursor : in Token_Cursor) return Boolean
   is begin
      return Cursor.State = Done;
   end Is_Done;

   function ID (Cursor : in Token_Cursor) return Token_ID
   is begin
      return Cursor.ID;
   end ID;

   function Name_1 (Cursor : in Token_Cursor) return String
   is begin
      --   This function is used to compute LR1_descriptor.Image
      case Cursor.State is
      when Non_Reporting | Terminals_Others =>
         declare
            Token_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
              Wisi.Token_Lists.Constant_Reference (Tokens, Cursor.Token_Kind);

            Item_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Token_Ref.Element.Tokens, Cursor.Token_Item);
         begin
            return -Item_Ref.Element.Name;
         end;

      when Terminals_Keywords =>
         declare
            Keyword_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Keywords, Cursor.Keyword);
         begin
            return -Keyword_Ref.Element.Name;
         end;

      when EOI =>
         return -EOI_Name;

      when WisiToken_Accept =>
         return -WisiToken_Accept_Name;

      when Nonterminal =>
         declare
            Rule_Ref : constant Rule_Lists.Constant_Reference_Type := Rule_Lists.Constant_Reference
              (Rules, Cursor.Nonterminal);
         begin
            return -Rule_Ref.Element.Left_Hand_Side;
         end;

      when Done =>
         raise Programmer_Error with "token cursor is done";
      end case;
   end Name_1;

   function Name (Cursor : in Token_Cursor) return String
   is begin
      return LR1_Descriptor.Image (Cursor.ID).all;
   end Name;

   function Kind (Cursor : in Token_Cursor) return String
   is begin
      case Cursor.State is
      when Non_Reporting | Terminals_Others =>
         return -Token_Lists.Element (Cursor.Token_Kind).Kind;

      when Terminals_Keywords =>
         return "keyword";

      when EOI =>
         return "EOI";

      when WisiToken_Accept =>
         return "accept";

      when Nonterminal =>
            return "nonterminal";

      when Done =>
         raise Programmer_Error with "token cursor is done";
      end case;
   end Kind;

   function Value (Cursor : in Token_Cursor) return String
   is begin
      case Cursor.State is
      when Non_Reporting | Terminals_Others =>
         declare
            Token_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
              Wisi.Token_Lists.Constant_Reference (Tokens, Cursor.Token_Kind);

            Item_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Token_Ref.Element.Tokens, Cursor.Token_Item);
         begin
            return -Item_Ref.Element.Value;
         end;

      when Terminals_Keywords =>
         declare
            Keyword_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Keywords, Cursor.Keyword);
         begin
            return -Keyword_Ref.Element.Value;
         end;

      when EOI | WisiToken_Accept | Nonterminal =>
            return "";

      when Done =>
         raise Programmer_Error with "token cursor is done";
      end case;
   end Value;

   procedure Put_Tokens
   is
      use Standard.Ada.Text_IO;
   begin
      Put_Line ("Tokens:");
      for I in Token_ID'First .. LR1_Descriptor.Last_Nonterminal loop
         Put_Line (Token_ID'Image (I) & " => " & Token_WY_Image (I));
      end loop;
      New_Line;
   end Put_Tokens;

   function To_Conflicts
     (Source_File_Name             : in     String;
      Accept_Reduce_Conflict_Count :    out Integer;
      Shift_Reduce_Conflict_Count  :    out Integer;
      Reduce_Reduce_Conflict_Count :    out Integer)
     return WisiToken.Parser.LR.Generator_Utils.Conflict_Lists.List
   is
      use WisiToken.Parser.LR.Generator_Utils;
      use all type WisiToken.Parser.LR.Unknown_State_Index;
      use all type WisiToken.Parser.LR.Parse_Action_Verbs;
      Result   : WisiToken.Parser.LR.Generator_Utils.Conflict_Lists.List;
      Conflict : WisiToken.Parser.LR.Generator_Utils.Conflict;
      Error    : Boolean := False;
   begin
      Accept_Reduce_Conflict_Count := 0;
      Shift_Reduce_Conflict_Count  := 0;
      Reduce_Reduce_Conflict_Count := 0;

      for Item of Conflicts loop
         begin
            Conflict :=
              (Conflict_Parse_Actions'Value (-Item.Action_A),
               Find_Token_ID (-Item.LHS_A),
               Conflict_Parse_Actions'Value (-Item.Action_B),
               Find_Token_ID (-Item.LHS_B),
               -1,
               Find_Token_ID (-Item.On));

            case Conflict.Action_A is
            when Shift =>
               Shift_Reduce_Conflict_Count := Shift_Reduce_Conflict_Count + 1;
            when Reduce =>
               Reduce_Reduce_Conflict_Count := Reduce_Reduce_Conflict_Count + 1;
            when Accept_It =>
               Accept_Reduce_Conflict_Count := Reduce_Reduce_Conflict_Count + 1;
            end case;

            Result.Append (Conflict);
         exception
         when E : Not_Found =>
            Wisi.Utils.Put_Error (Source_File_Name, Item.Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
            Error := True;
         end;
      end loop;
      if Error then
         raise WisiToken.Grammar_Error with "known conflicts: tokens not found, aborting";
      end if;
      return Result;
   end To_Conflicts;

   function "&" (Tokens : in Token.List.Instance; Token : in String) return WisiToken.Token.List.Instance
   is
      use WisiToken.Token.List;
   begin
      return Tokens & Find_Token_ID (Token);
   end "&";

   function To_Grammar
     (Descriptor       : in WisiToken.Descriptor'Class;
      Source_File_Name : in String;
      Start_Token      : in String)
     return Production.List.Instance
   is
      use Production;
      use Token.List;

      Grammar : Production.List.Instance;
      Error   : Boolean := False;
   begin
      begin
         Grammar := Production.List.Only
           (Descriptor.Accept_ID <= Find_Token_ID (Start_Token) & EOF_ID + Null_Action);
      exception
      when Not_Found =>
         Wisi.Utils.Put_Error
           (Source_File_Name, First_Rule_Line, "start token '" & (Start_Token) & "' not found; need %start?");
         Error := True;
      end;

      for Rule of Rules loop
         declare
            Index : Natural := 0; -- Semantic_Action defines Index as zero-origin
         begin
            for Right_Hand_Side of Rule.Right_Hand_Sides loop
               declare
                  use Production.List;

                  Tokens : WisiToken.Token.List.Instance;
               begin
                  for Token of Right_Hand_Side.Production loop
                     Tokens := Tokens & Token;
                  end loop;
                  Grammar := Grammar and Find_Token_ID (-Rule.Left_Hand_Side) <= Tokens + Index;
               exception
               when E : Not_Found =>
                  Wisi.Utils.Put_Error
                    (Source_File_Name, Rule.Source_Line, Standard.Ada.Exceptions.Exception_Message (E));
                  Error := True;
               end;
               Index := Index + 1;
            end loop;
         end;
      end loop;

      if Error then
         raise Syntax_Error;
      else
         return Grammar;
      end if;
   end To_Grammar;

   function To_Nonterminal_ID_Set (Item : in String_Lists.List) return Token_ID_Set
   is
      Result : Token_ID_Set := (LR1_Descriptor.First_Nonterminal .. LR1_Descriptor.Last_Nonterminal => False);
   begin
      for Token of Item loop
         Result (Find_Token_ID (Token)) := True;
      end loop;
      return Result;
   end To_Nonterminal_ID_Set;

   function To_McKenzie_Param (Item : in McKenzie_Recover_Param_Type) return WisiToken.Parser.LR.McKenzie_Param_Type
   is
      use Standard.Ada.Strings.Unbounded;

      Result : WisiToken.Parser.LR.McKenzie_Param_Type :=
        --  We use an aggregate, and overwrite some below, so the compiler
        --  reminds us to change this when we modify McKenzie_Param_Type.
        (LR1_Descriptor.First_Terminal,
         LR1_Descriptor.Last_Terminal,
         LR1_Descriptor.First_Nonterminal,
         LR1_Descriptor.Last_Nonterminal,
         Insert      => (others => Item.Default_Insert),
         Delete      => (others => Item.Default_Delete_Terminal),
         Cost_Limit  => Item.Cost_Limit,
         Check_Limit => Item.Check_Limit,
         Patterns    => WisiToken.Parser.LR.Patterns.Empty_List);
   begin
      Result.Delete (Result.First_Nonterminal .. Result.Last_Nonterminal) :=
        (others => Item.Default_Delete_Nonterminal);

      for Pair of Item.Delete loop
         Result.Delete (Find_Token_ID (-Pair.Name)) := Natural'Value (-Pair.Value);
      end loop;
      for Pair of Item.Insert loop
         Result.Insert (Find_Token_ID (-Pair.Name)) := Natural'Value (-Pair.Value);
      end loop;

      for Pattern of Item.Patterns loop
         if Pattern in Wisi.Recover_Pattern_1'Class then
            declare
               Pat : Wisi.Recover_Pattern_1 renames Wisi.Recover_Pattern_1 (Pattern);
            begin
               Result.Patterns.Append
                 (WisiToken.Parser.LR.Recover_Pattern_1'
                    (Stack     => Find_Token_ID (-Pat.Stack),
                     Error     => Find_Token_ID (-Pat.Error),
                     Expecting => Find_Token_ID (-Pat.Expecting)));
            end;
         else
            raise Programmer_Error;
         end if;
      end loop;
      return Result;
   end To_McKenzie_Param;

begin
   Set_Token_Images;

   if Verbosity > 0 then
      Put_Tokens;
   end if;

end Wisi.Gen_Generate_Utils;
