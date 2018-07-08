--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014, 2015, 2017, 2018  All Rights Reserved.
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
with Ada.Text_IO;
with WisiToken.Generate; use WisiToken.Generate;
with WisiToken.Syntax_Trees;
with WisiToken.Wisi_Ada;
package body Wisi.Generate_Utils is

   --  For Constant_Reference
   Aliased_EOI_Name              : aliased constant Standard.Ada.Strings.Unbounded.Unbounded_String := +EOI_Name;
   Aliased_WisiToken_Accept_Name : aliased constant Standard.Ada.Strings.Unbounded.Unbounded_String :=
     +WisiToken_Accept_Name;

   --  body specs, as needed.

   ----------
   --  Body subprograms

   function Find_Kind (Data : aliased Generate_Data; Target_Kind : in String) return Token_ID
   is begin
      for Cursor in All_Tokens (Data).Iterate loop
         if Kind (Cursor) = Target_Kind then
            return ID (Cursor);
         end if;
      end loop;
      return Invalid_Token_ID;
   end Find_Kind;

   function Name_1 (Cursor : in Token_Cursor) return String
   is begin
      --   This function is used to compute LR1_descriptor.Image
      case Cursor.Kind is
      when Non_Grammar_Kind =>
         declare
            Kind_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
              Wisi.Token_Lists.Constant_Reference (Cursor.Data.Tokens.Non_Grammar, Cursor.Token_Kind);

            Item_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Kind_Ref.Element.Tokens, Cursor.Token_Item);
         begin
            return -Item_Ref.Element.Name;
         end;

      when Terminals_Keywords =>
         declare
            Keyword_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Cursor.Data.Tokens.Keywords, Cursor.Keyword);
         begin
            return -Keyword_Ref.Element.Name;
         end;

      when Terminals_Others =>
         declare
            Kind_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
              Wisi.Token_Lists.Constant_Reference (Cursor.Data.Tokens.Tokens, Cursor.Token_Kind);

            Item_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Kind_Ref.Element.Tokens, Cursor.Token_Item);
         begin
            return -Item_Ref.Element.Name;
         end;

      when EOI =>
         return EOI_Name;

      when WisiToken_Accept =>
         return WisiToken_Accept_Name;

      when Nonterminal =>
         declare
            Rule_Ref : constant Rule_Lists.Constant_Reference_Type := Rule_Lists.Constant_Reference
              (Cursor.Data.Tokens.Rules, Cursor.Nonterminal);
         begin
            return -Rule_Ref.Element.Left_Hand_Side;
         end;

      when Done =>
         raise Programmer_Error with "token cursor is done";
      end case;
   end Name_1;

   procedure To_Grammar
     (Data             : aliased in out Generate_Data;
      Source_File_Name :         in     String;
      Start_Token      :         in     String)
   is
      use WisiToken.Wisi_Ada;
      Descriptor : WisiToken.Descriptor renames Data.LR1_Descriptor.all;
   begin
      Data.Grammar.Set_First (Descriptor.First_Nonterminal);
      Data.Grammar.Set_Last (Descriptor.Last_Nonterminal);
      Data.Source_Line_Map.Set_First (Descriptor.First_Nonterminal);
      Data.Source_Line_Map.Set_Last (Descriptor.Last_Nonterminal);

      pragma Assert (Descriptor.Accept_ID = Descriptor.First_Nonterminal);
      begin
         Data.Grammar (Descriptor.Accept_ID) :=
           Descriptor.Accept_ID <= Only
             (Find_Token_ID (Data, Start_Token) & Descriptor.EOF_ID + WisiToken.Syntax_Trees.Null_Action);

         Data.Source_Line_Map (Descriptor.Accept_ID).Line := Line_Number_Type'First;
         Data.Source_Line_Map (Descriptor.Accept_ID).RHS_Map.Set_First (0);
         Data.Source_Line_Map (Descriptor.Accept_ID).RHS_Map.Set_Last (0);
         Data.Source_Line_Map (Descriptor.Accept_ID).RHS_Map (0) := Line_Number_Type'First;
      exception
      when Not_Found =>
         Put_Error
           (Error_Message
              (Source_File_Name, 1, "start token '" & (Start_Token) & "' not found; need %start?"));
      end;

      for Rule of Data.Tokens.Rules loop
         declare
            RHS_Index : Natural := 0;
            RHSs      : WisiToken.Productions.RHS_Arrays.Vector;
            LHS       : Token_ID; -- not initialized for exception handler
         begin
            LHS := Find_Token_ID (Data, -Rule.Left_Hand_Side);

            RHSs.Set_First (RHS_Index);
            RHSs.Set_Last (Natural (Rule.Right_Hand_Sides.Length) - 1);

            Data.Source_Line_Map (LHS).Line := Rule.Source_Line;
            Data.Source_Line_Map (LHS).RHS_Map.Set_First (RHSs.First_Index);
            Data.Source_Line_Map (LHS).RHS_Map.Set_Last (RHSs.Last_Index);

            for Right_Hand_Side of Rule.Right_Hand_Sides loop
               declare
                  use all type Standard.Ada.Containers.Count_Type;
                  Tokens : WisiToken.Token_ID_Arrays.Vector;
                  I      : Integer := 1;
               begin
                  if Right_Hand_Side.Tokens.Length > 0 then
                     Tokens.Set_First (I);
                     Tokens.Set_Last (Integer (Right_Hand_Side.Tokens.Length));
                     for Token of Right_Hand_Side.Tokens loop
                        Tokens (I) := Find_Token_ID (Data, Token);
                        I := I + 1;
                     end loop;
                  end if;
                  RHSs (RHS_Index) := (Tokens => Tokens, Action => null, Check => null);

                  Data.Source_Line_Map (LHS).RHS_Map (RHS_Index) := Right_Hand_Side.Source_Line;
               exception
               when E : Not_Found =>
                  --  From "&"
                  Put_Error
                    (Error_Message
                       (Source_File_Name, Right_Hand_Side.Source_Line, Standard.Ada.Exceptions.Exception_Message (E)));
               end;
               RHS_Index := RHS_Index + 1;
            end loop;

            Data.Grammar (LHS) := LHS <= RHSs;
         exception
         when E : Not_Found =>
            --  From Find_Token_ID (left_hand_side)
            Put_Error
              (Error_Message
                 (Source_File_Name, Rule.Source_Line, Standard.Ada.Exceptions.Exception_Message (E)));
         end;
      end loop;

      WisiToken.Generate.Check_Consistent (Data.Grammar, Descriptor, Source_File_Name);
   end To_Grammar;

   ----------
   --  Public subprograms, declaration order

   function Initialize
     (Source_File_Name :         in String;
      Tokens           : aliased in Wisi.Tokens;
      Start_Token      :         in String)
     return Generate_Data
   is
      EOF_ID : constant Token_ID := Token_ID
        (Count (Tokens.Non_Grammar) + Count (Tokens.Tokens)) + Token_ID (Tokens.Keywords.Length) + Token_ID'First;
   begin
      return Result : aliased Generate_Data :=
        (Tokens => Tokens'Access,

         LR1_Descriptor => new WisiToken.Descriptor
           (First_Terminal    =>
              (if Count (Tokens.Non_Grammar) > 0
               then Token_ID (Count (Tokens.Non_Grammar)) + Token_ID'First
               else Token_ID'First),
            Last_Terminal     => EOF_ID,
            EOF_ID            => EOF_ID,
            Accept_ID         => EOF_ID + 1,
            First_Nonterminal => EOF_ID + 1,
            Last_Nonterminal  => EOF_ID + 1 + Token_ID (Tokens.Rules.Length)),

         others => <>)
      do

         --  FIXME: We don't set Descriptor.Case_Insensitive,
         --  Embedded_Quote_Escape_Doubled here, because we don't have access
         --  to Generate_Params. They are set in the generated code in
         --  wisi-gen_output_ada_common.adb.
         Result.LR1_Descriptor.New_Line_ID    := Find_Kind (Result, "new-line");
         Result.LR1_Descriptor.Comment_ID     := Find_Kind (Result, "comment");
         Result.LR1_Descriptor.Left_Paren_ID  := Find_Kind (Result, "left-paren");
         Result.LR1_Descriptor.Right_Paren_ID := Find_Kind (Result, "right-paren");
         Result.LR1_Descriptor.String_1_ID    := Find_Kind (Result, "string-single");
         Result.LR1_Descriptor.String_2_ID    := Find_Kind (Result, "string-double");

         Result.LR1_Descriptor.Terminal_Image_Width := 0;
         Result.LR1_Descriptor.Image_Width          := 0;

         for Cursor in All_Tokens (Result).Iterate loop
            Result.LR1_Descriptor.Image (ID (Cursor)) := new String'(Name_1 (Cursor));
         end loop;

         for ID in Result.LR1_Descriptor.Image'Range loop
            if ID in Result.LR1_Descriptor.First_Terminal .. Result.LR1_Descriptor.Last_Terminal then
               if Result.LR1_Descriptor.Image (ID).all'Length > Result.LR1_Descriptor.Terminal_Image_Width then
                  Result.LR1_Descriptor.Terminal_Image_Width := Result.LR1_Descriptor.Image (ID).all'Length;
               end if;
            end if;

            if Result.LR1_Descriptor.Image (ID).all'Length > Result.LR1_Descriptor.Image_Width then
               Result.LR1_Descriptor.Image_Width := Result.LR1_Descriptor.Image (ID).all'Length;
            end if;
         end loop;

         Result.LALR_Descriptor := new WisiToken.LALR_Descriptor'
           (First_Terminal                => Result.LR1_Descriptor.First_Terminal,
            Last_Terminal                 => Result.LR1_Descriptor.Last_Terminal,
            First_Nonterminal             => EOF_ID + 1,
            Last_Nonterminal              => Result.LR1_Descriptor.Last_Nonterminal,
            EOF_ID                        => Result.LR1_Descriptor.EOF_ID,
            Accept_ID                     => EOF_ID + 1,
            Propagate_ID                  => EOF_ID + 1,
            Case_Insensitive              => False, --  FIXME:
            New_Line_ID                   => Result.LR1_Descriptor.New_Line_ID,
            Comment_ID                    => Result.LR1_Descriptor.Comment_ID,
            Left_Paren_ID                 => Result.LR1_Descriptor.Left_Paren_ID,
            Right_Paren_ID                => Result.LR1_Descriptor.Right_Paren_ID,
            String_1_ID                   => Result.LR1_Descriptor.String_1_ID,
            String_2_ID                   => Result.LR1_Descriptor.String_2_ID,
            Embedded_Quote_Escape_Doubled => False, --  FIXME:
            Image                         => Result.LR1_Descriptor.Image,
            Terminal_Image_Width          => Result.LR1_Descriptor.Terminal_Image_Width,
            Image_Width                   => Result.LR1_Descriptor.Image_Width);

         To_Grammar (Result, Source_File_Name, Start_Token);
      end return;
   end Initialize;

   function Find_Token_ID (Data : aliased Generate_Data; Token : in String) return Token_ID
   is begin
      for Cursor in All_Tokens (Data).Iterate loop
         if Name (Cursor) = Token then
            return ID (Cursor);
         end if;
      end loop;
      raise Not_Found with "token '" & Token & "' not found";
   end Find_Token_ID;

   function All_Tokens (Data : aliased in Generate_Data) return Token_Container
   is begin
      return (Data => Data'Access);
   end All_Tokens;

   function Constant_Reference
     (Container : aliased in Token_Container'Class;
      Cursor    :         in Token_Cursor)
     return Token_Constant_Reference_Type
   is begin
      case Cursor.Kind is
      when Non_Grammar_Kind =>
         declare
            Token_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
              Wisi.Token_Lists.Constant_Reference (Container.Data.Tokens.Non_Grammar, Cursor.Token_Kind);

            Item_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Token_Ref.Element.Tokens, Cursor.Token_Item);
         begin
            return (Element => Item_Ref.Element.all.Name'Access);
         end;

      when Terminals_Keywords =>
         declare
            Keyword_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Container.Data.Tokens.Keywords, Cursor.Keyword);
         begin
            return (Element => Keyword_Ref.Element.all.Name'Access);
         end;

      when Terminals_Others =>
         declare
            Token_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
              Wisi.Token_Lists.Constant_Reference (Container.Data.Tokens.Tokens, Cursor.Token_Kind);

            Item_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Token_Ref.Element.Tokens, Cursor.Token_Item);
         begin
            return (Element => Item_Ref.Element.all.Name'Access);
         end;

      when EOI =>
         return (Element => Aliased_EOI_Name'Access);

      when WisiToken_Accept =>
         return (Element => Aliased_WisiToken_Accept_Name'Access);

      when Nonterminal =>
         declare
            Rule_Ref : constant Rule_Lists.Constant_Reference_Type := Rule_Lists.Constant_Reference
              (Container.Data.Tokens.Rules, Cursor.Nonterminal);
         begin
            return (Element => Rule_Ref.Element.all.Left_Hand_Side'Access);
         end;

      when Done =>
         raise Programmer_Error with "token cursor is done";
      end case;
   end Constant_Reference;

   type Token_Access_Constant is access constant Token_Container;
   type Iterator is new Iterator_Interfaces.Forward_Iterator with record
      Container    : Token_Access_Constant;
      Non_Grammar  : Boolean;
      Nonterminals : Boolean;
   end record;

   overriding function First (Object : Iterator) return Token_Cursor;
   overriding function Next (Object : Iterator; Position : Token_Cursor) return Token_Cursor;

   overriding function First (Object : Iterator) return Token_Cursor
   is begin
      return First (Object.Container.Data.all, Object.Non_Grammar, Object.Nonterminals);
   end First;

   overriding function Next (Object  : Iterator; Position : Token_Cursor) return Token_Cursor
   is
      Next_Position : Token_Cursor := Position;
   begin
      Next (Next_Position, Object.Nonterminals);
      return Next_Position;
   end Next;

   function Iterate
     (Container    : aliased    Token_Container;
      Non_Grammar  :         in Boolean := True;
      Nonterminals :         in Boolean := True)
     return Iterator_Interfaces.Forward_Iterator'Class
   is begin
      return Iterator'(Container'Access, Non_Grammar, Nonterminals);
   end Iterate;

   function Next_Kind_Internal
     (Cursor       : in out Token_Cursor;
      Nonterminals : in     Boolean)
     return Boolean
   is begin
      --  Advance Cursor to the next kind; return True if any of that
      --  kind exist, or kind is Done; False otherwise.
      case Cursor.Kind is
      when Non_Grammar_Kind =>

         Cursor :=
           (Data        => Cursor.Data,
            Kind        => Terminals_Keywords,
            ID          => Cursor.Data.LR1_Descriptor.First_Terminal,
            Token_Kind  => Wisi.Token_Lists.No_Element,
            Token_Item  => String_Pair_Lists.No_Element,
            Keyword     => Cursor.Data.Tokens.Keywords.First,
            Nonterminal => Rule_Lists.No_Element);

         return String_Pair_Lists.Has_Element (Cursor.Keyword);

      when Terminals_Keywords =>

         Cursor :=
           (Data        => Cursor.Data,
            Kind        => Terminals_Others,
            ID          => Cursor.ID,
            Token_Kind  => Cursor.Data.Tokens.Tokens.First,
            Token_Item  => String_Pair_Lists.No_Element,
            Keyword     => String_Pair_Lists.No_Element,
            Nonterminal => Rule_Lists.No_Element);

         if Wisi.Token_Lists.Has_Element (Cursor.Token_Kind) then
            Cursor.Token_Item := Cursor.Data.Tokens.Tokens (Cursor.Token_Kind).Tokens.First;
            return Wisi.String_Pair_Lists.Has_Element (Cursor.Token_Item);
         else
            return False;
         end if;

      when Terminals_Others =>
         Cursor :=
           (Data        => Cursor.Data,
            Kind        => EOI,
            ID          => Cursor.ID,
            Token_Kind  => Wisi.Token_Lists.No_Element,
            Token_Item  => String_Pair_Lists.No_Element,
            Keyword     => String_Pair_Lists.No_Element,
            Nonterminal => Rule_Lists.No_Element);

         return True;

      when EOI =>
         if Nonterminals then
            if Rule_Lists.Has_Element (Cursor.Data.Tokens.Rules.First) then
               Cursor :=
                 (Data        => Cursor.Data,
                  Kind        => WisiToken_Accept,
                  ID          => Cursor.ID,
                  Token_Kind  => Wisi.Token_Lists.No_Element,
                  Token_Item  => String_Pair_Lists.No_Element,
                  Keyword     => String_Pair_Lists.No_Element,
                  Nonterminal => Rule_Lists.No_Element);
            else
               Cursor.Kind := Done;
            end if;
            return True;
         else
            Cursor.Kind := Done;
            return True;
         end if;

      when WisiToken_Accept =>
         Cursor :=
           (Data        => Cursor.Data,
            Kind        => Nonterminal,
            ID          => Cursor.ID,
            Token_Kind  => Wisi.Token_Lists.No_Element,
            Token_Item  => String_Pair_Lists.No_Element,
            Keyword     => String_Pair_Lists.No_Element,
            Nonterminal => Cursor.Data.Tokens.Rules.First);

         --  Can't get here with no rules
         return True;

      when Nonterminal =>
         Cursor.Kind := Done;

         return True;

      when Done =>
         return True;
      end case;
   end Next_Kind_Internal;

   function First
     (Data         : aliased in Generate_Data;
      Non_Grammar  :         in Boolean;
      Nonterminals :         in Boolean)
     return Token_Cursor
   is
      Cursor : Token_Cursor :=
        (Data        => Data'Access,
         Kind        => Non_Grammar_Kind,
         ID          => Token_ID'First,
         Token_Kind  => Data.Tokens.Non_Grammar.First,
         Token_Item  => String_Pair_Lists.No_Element,
         Keyword     => String_Pair_Lists.No_Element,
         Nonterminal => Rule_Lists.No_Element);
   begin
      if Non_Grammar then
         if Wisi.Token_Lists.Has_Element (Cursor.Token_Kind) then
            Cursor.Token_Item := Cursor.Data.Tokens.Non_Grammar (Cursor.Token_Kind).Tokens.First;
            if Wisi.String_Pair_Lists.Has_Element (Cursor.Token_Item) then
               return Cursor;
            end if;
         end if;
      end if;

      --  There are no non_grammar tokens, or Non_Grammar false
      loop
         exit when Next_Kind_Internal (Cursor, Nonterminals);
      end loop;
      return Cursor;
   end First;

   procedure Next (Cursor : in out Token_Cursor; Nonterminals : in Boolean)
   is begin
      Cursor.ID := Cursor.ID + 1;

      case Cursor.Kind is
      when Non_Grammar_Kind =>
         String_Pair_Lists.Next (Cursor.Token_Item);
         if String_Pair_Lists.Has_Element (Cursor.Token_Item) then
            return;
         else
            Wisi.Token_Lists.Next (Cursor.Token_Kind);

            if Wisi.Token_Lists.Has_Element (Cursor.Token_Kind) then
               Cursor.Token_Item := Cursor.Data.Tokens.Non_Grammar (Cursor.Token_Kind).Tokens.First;
               if String_Pair_Lists.Has_Element (Cursor.Token_Item) then
                  return;
               end if;
            end if;
         end if;

         loop
            exit when Next_Kind_Internal (Cursor, Nonterminals);
         end loop;
         return;

      when Terminals_Keywords =>
         --  Keywords before other terminals, so they have precedence over Identifiers

         String_Pair_Lists.Next (Cursor.Keyword);
         if String_Pair_Lists.Has_Element (Cursor.Keyword) then
            return;
         end if;

         loop
            exit when Next_Kind_Internal (Cursor, Nonterminals);
         end loop;
         return;

      when Terminals_Others =>
         Wisi.String_Pair_Lists.Next (Cursor.Token_Item);
         if Wisi.String_Pair_Lists.Has_Element (Cursor.Token_Item) then
            return;
         else
            Wisi.Token_Lists.Next (Cursor.Token_Kind);
            if Wisi.Token_Lists.Has_Element (Cursor.Token_Kind) then
               Cursor.Token_Item := Cursor.Data.Tokens.Tokens (Cursor.Token_Kind).Tokens.First;
               if Wisi.String_Pair_Lists.Has_Element (Cursor.Token_Item) then
                  return;
               end if;
            end if;
         end if;

         loop
            exit when Next_Kind_Internal (Cursor, Nonterminals);
         end loop;
         return;

      when EOI =>
         if Next_Kind_Internal (Cursor, Nonterminals) then
            return;
         else
            raise Programmer_Error;
         end if;

      when WisiToken_Accept =>
         if Next_Kind_Internal (Cursor, Nonterminals) then
            return;
         else
            raise Programmer_Error;
         end if;

      when Nonterminal =>
         Rule_Lists.Next (Cursor.Nonterminal);
         if not Rule_Lists.Has_Element (Cursor.Nonterminal) then
            Cursor.Kind := Done;
         end if;

      when Done =>
         null;
      end case;
   end Next;

   function Is_Done (Cursor : in Token_Cursor) return Boolean
   is begin
      return Cursor.Kind = Done;
   end Is_Done;

   function ID (Cursor : in Token_Cursor) return Token_ID
   is begin
      return Cursor.ID;
   end ID;

   function Name (Cursor : in Token_Cursor) return String
   is begin
      return Cursor.Data.LR1_Descriptor.Image (Cursor.ID).all;
   end Name;

   function Kind (Cursor : in Token_Cursor) return String
   is begin
      case Cursor.Kind is
      when Non_Grammar_Kind =>
         return -Token_Lists.Constant_Reference (Cursor.Data.Tokens.Non_Grammar, Cursor.Token_Kind).Kind;

      when Terminals_Keywords =>
         return "keyword";

      when Terminals_Others =>
         return -Token_Lists.Constant_Reference (Cursor.Data.Tokens.Tokens, Cursor.Token_Kind).Kind;

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
      case Cursor.Kind is
      when Non_Grammar_Kind =>
         declare
            Token_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
              Wisi.Token_Lists.Constant_Reference (Cursor.Data.Tokens.Non_Grammar, Cursor.Token_Kind);

            Item_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Token_Ref.Element.Tokens, Cursor.Token_Item);
         begin
            return -Item_Ref.Element.Value;
         end;

      when Terminals_Keywords =>
         declare
            Keyword_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Cursor.Data.Tokens.Keywords, Cursor.Keyword);
         begin
            return -Keyword_Ref.Element.Value;
         end;

      when Terminals_Others =>
         declare
            Token_Ref : constant Wisi.Token_Lists.Constant_Reference_Type :=
              Wisi.Token_Lists.Constant_Reference (Cursor.Data.Tokens.Tokens, Cursor.Token_Kind);

            Item_Ref : constant String_Pair_Lists.Constant_Reference_Type :=
              String_Pair_Lists.Constant_Reference (Token_Ref.Element.Tokens, Cursor.Token_Item);
         begin
            return -Item_Ref.Element.Value;
         end;

      when EOI | WisiToken_Accept | Nonterminal =>
            return "";

      when Done =>
         raise Programmer_Error with "token cursor is done";
      end case;
   end Value;

   function To_Conflicts
     (Data             : aliased in out Generate_Data;
      Conflicts        :         in     Wisi.Conflict_Lists.List;
      Source_File_Name :         in     String)
     return WisiToken.LR.Generator_Utils.Conflict_Lists.List
   is
      use WisiToken.LR.Generator_Utils;
      use all type WisiToken.LR.Parse_Action_Verbs;
      Result   : WisiToken.LR.Generator_Utils.Conflict_Lists.List;
      Conflict : WisiToken.LR.Generator_Utils.Conflict;
   begin
      Data.Accept_Reduce_Conflict_Count := 0;
      Data.Shift_Reduce_Conflict_Count  := 0;
      Data.Reduce_Reduce_Conflict_Count := 0;

      for Item of Conflicts loop
         begin
            Conflict :=
              (Conflict_Parse_Actions'Value (-Item.Action_A),
               Find_Token_ID (Data, -Item.LHS_A),
               Conflict_Parse_Actions'Value (-Item.Action_B),
               Find_Token_ID (Data, -Item.LHS_B),
               -1,
               Find_Token_ID (Data, -Item.On));

            case Conflict.Action_A is
            when Shift =>
               Data.Shift_Reduce_Conflict_Count := Data.Shift_Reduce_Conflict_Count + 1;
            when Reduce =>
               Data.Reduce_Reduce_Conflict_Count := Data.Reduce_Reduce_Conflict_Count + 1;
            when Accept_It =>
               Data.Accept_Reduce_Conflict_Count := Data.Reduce_Reduce_Conflict_Count + 1;
            end case;

            Result.Append (Conflict);
         exception
         when E : Not_Found =>
            Put_Error
              (Error_Message
                 (Source_File_Name, Item.Source_Line, Standard.Ada.Exceptions.Exception_Message (E)));
         end;
      end loop;
      return Result;
   end To_Conflicts;

   function To_Nonterminal_ID_Set
     (Data : aliased in Generate_Data;
      Item :         in String_Lists.List)
     return Token_ID_Set
   is
      Result : Token_ID_Set := (Data.LR1_Descriptor.First_Nonterminal .. Data.LR1_Descriptor.Last_Nonterminal => False);
   begin
      for Token of Item loop
         Result (Find_Token_ID (Data, Token)) := True;
      end loop;
      return Result;
   end To_Nonterminal_ID_Set;

   function To_McKenzie_Param
     (Data : aliased in Generate_Data;
      Item :         in McKenzie_Recover_Param_Type)
     return WisiToken.LR.McKenzie_Param_Type
   is
      use Standard.Ada.Strings.Unbounded;

      Result : WisiToken.LR.McKenzie_Param_Type :=
        --  We use an aggregate, and overwrite some below, so the compiler
        --  reminds us to change this when we modify McKenzie_Param_Type.
        (Data.LR1_Descriptor.First_Terminal,
         Data.LR1_Descriptor.Last_Terminal,
         Data.LR1_Descriptor.First_Nonterminal,
         Data.LR1_Descriptor.Last_Nonterminal,
         Insert            => (others => Item.Default_Insert),
         Delete            => (others => Item.Default_Delete_Terminal),
         Push_Back         => (others => Item.Default_Push_Back),
         Task_Count        => 0,
         Cost_Limit        => Item.Cost_Limit,
         Check_Limit       => Item.Check_Limit,
         Check_Delta_Limit => Item.Check_Delta_Limit,
         Enqueue_Limit     => Item.Enqueue_Limit);
   begin
      Result.Delete (Result.First_Nonterminal .. Result.Last_Nonterminal) :=
        (others => Item.Default_Delete_Nonterminal);

      for Pair of Item.Delete loop
         Result.Delete (Find_Token_ID (Data, -Pair.Name)) := Natural'Value (-Pair.Value);
      end loop;
      for Pair of Item.Insert loop
         Result.Insert (Find_Token_ID (Data, -Pair.Name)) := Natural'Value (-Pair.Value);
      end loop;
      for Pair of Item.Push_Back loop
         Result.Push_Back (Find_Token_ID (Data, -Pair.Name)) := Natural'Value (-Pair.Value);
      end loop;

      return Result;
   end To_McKenzie_Param;

   procedure Count_Actions
     (Data : in out Generate_Utils.Generate_Data;
      Alg  : in     LR_Generate_Algorithm)
   is begin
      Data.Table_Actions_Count := 0;
      for State_Index in Data.LR_Parsers (Alg).States'Range loop
         Data.Table_Actions_Count := Data.Table_Actions_Count +
           WisiToken.LR.Actions_Length (Data.LR_Parsers (Alg).States (State_Index)) + 1;
      end loop;
   end Count_Actions;

   procedure Put_Stats
     (Input_Data    : in WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
      Generate_Data : in Generate_Utils.Generate_Data)
   is
      use Standard.Ada.Text_IO;
   begin
      New_Line;
      Put_Line
        (Integer'Image (Input_Data.Rule_Count) & " rules," &
           Integer'Image (Input_Data.Action_Count) & " user actions," &
           Integer'Image (Input_Data.Check_Count) & " checks," &
           WisiToken.State_Index'Image (Generate_Data.Parser_State_Count) & " states," &
           Integer'Image (Generate_Data.Table_Actions_Count) & " parse actions");
      Put_Line
        (Integer'Image (Generate_Data.Accept_Reduce_Conflict_Count) & " accept/reduce conflicts," &
           Integer'Image (Generate_Data.Shift_Reduce_Conflict_Count) & " shift/reduce conflicts," &
           Integer'Image (Generate_Data.Reduce_Reduce_Conflict_Count) & " reduce/reduce conflicts");
   end Put_Stats;

end Wisi.Generate_Utils;
