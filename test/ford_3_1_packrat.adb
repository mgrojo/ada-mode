--  Abstract :
--
--  Implement [1] section 3.1.4 example using packrat
--
--     Additive  <= Multitive '+' Additive / Multitive
--
--     Multitive <= Primary '*' Multitive / Primary
--
--     Primary   <= '(' Additive ')' / Decimal
--
--     Decimal   <= '0' / ... / '9'
--
--  [1] Bryan Ford MIT thesis
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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
with GNAT.Traceback.Symbolic;
procedure Ford_3_1_Packrat
is
   Programmer_Error : exception;

   type Result_Labels is (Lazy, Parsed, NoParse);
   type Value_Labels is (Int, Char);

   type Derivs;
   type Derivs_Access is access Derivs;

   --  Derivs and Result should be controlled for deep free, but we're ignoring that.

   type Result
     (Result_Label : Result_Labels := Result_Labels'First;
      Value_Label  : Value_Labels  := Value_Labels'First)
   is record
      case Result_Label is
      when Lazy =>
         Arg : Derivs_Access;

      when Parsed =>
         Remaining : Derivs_Access;

         case Value_Label is
         when Int =>
            Int_Value : Integer;
         when Char =>
            Char_Value : Character;
         end case;

      when NoParse =>
         null;

      end case;
   end record;

   subtype Result_Int is Result with Dynamic_Predicate  => Result_Int.Value_Label = Int;
   subtype Result_Char is Result with Dynamic_Predicate => Result_Char.Value_Label = Char;

   type Derivs is record
      Additive  : Result_Int;
      Multitive : Result_Int;
      Primary   : Result_Int;
      Decimal   : Result_Int;
      Char      : Result_Char;
   end record;

   function Additive  (Item : in out Derivs) return Result_Int;
   function Multitive (Item : in out Derivs) return Result_Int;
   function Primary   (Item : in out Derivs) return Result_Int;
   function Decimal   (Item : in out Derivs) return Result_Int;

   function Additive (Item : in out Derivs) return Result_Int
   is begin
      if Item.Additive.Result_Label = Lazy then
         declare
            Left : constant Result_Int := Multitive (Item.Additive.Arg.all);
         begin
            case Left.Result_Label is
            when Lazy =>
               raise Programmer_Error;

            when Parsed =>
               --  Additive  <= Multitive '+' Additive
               case Left.Result_Label is
               when Lazy =>
                  raise Programmer_Error;

               when Parsed =>
                  declare
                     Op : constant Result_Char := Left.Remaining.Char;
                  begin
                     case Op.Result_Label is
                     when Lazy =>
                        raise Programmer_Error;

                     when Parsed =>
                        if Op.Char_Value = '+' then
                           declare
                              Right : constant Result_Int := Additive (Op.Remaining.all);
                           begin
                              case Right.Result_Label is
                              when Lazy =>
                                 raise Programmer_Error;

                              when Parsed =>
                                 Item.Additive := (Parsed, Int, Right.Remaining, Left.Int_Value + Right.Int_Value);
                              when NoParse =>
                                 null;
                              end case;
                           end;
                        end if;

                     when NoParse =>
                        null;
                     end case;
                  end;
               when NoParse =>
                  null;
               end case;
            when NoParse =>
               null;
            end case;

            if Item.Additive.Result_Label = Lazy then
               --  Additive <= Multitive
               Item.Additive := Left;
            end if;
         end;
      end if;

      return Item.Additive;
   end Additive;

   function Multitive (Item : in out Derivs) return Result_Int
   is begin
      if Item.Multitive.Result_Label = Lazy then
         declare
            Left : constant Result_Int := Primary (Item.Multitive.Arg.all);
         begin
            case Left.Result_Label is
            when Lazy =>
               raise Programmer_Error;

            when Parsed =>
               --  Multitive  <= Primary '*' Multitive
               declare
                  Op : constant Result_Char := Left.Remaining.Char;
               begin
                  case Op.Result_Label is
                  when Lazy =>
                     raise Programmer_Error;

                  when Parsed =>
                     if Op.Char_Value = '*' then
                        declare
                           Right : constant Result_Int := Multitive (Op.Remaining.all);
                        begin
                           case Right.Result_Label is
                           when Lazy =>
                              raise Programmer_Error;

                           when Parsed =>
                              Item.Multitive := (Parsed, Int, Right.Remaining, Left.Int_Value * Right.Int_Value);
                           when NoParse =>
                              null;
                           end case;
                        end;
                     end if;

                  when NoParse =>
                     null;
                  end case;
               end;
            when NoParse =>
               null;
            end case;

            if Item.Multitive.Result_Label = Lazy then
               --  Multitive <= Primary
               Item.Multitive := Left;
            end if;
         end;
      end if;

      return Item.Multitive;
   end Multitive;

   function Primary (Item : in out Derivs) return Result_Int
   is begin
      if Item.Primary.Result_Label = Lazy then
         --  Primary  <= '(' Additive ')'
         declare
            Item_1 : constant Result_Char := Item.Primary.Arg.Char;
         begin
            case Item_1.Result_Label is
            when Lazy =>
               raise Programmer_Error;

            when Parsed =>
               if Item_1.Char_Value = '(' then
                  declare
                     Item_2 : constant Result_Int := Additive (Item_1.Remaining.all);
                  begin
                     case Item_2.Result_Label is
                     when Lazy =>
                        raise Programmer_Error;

                     when Parsed =>
                        declare
                           Item_3 : constant Result_Char := Item_2.Remaining.Char;
                        begin
                           case Item_3.Result_Label is
                           when Lazy =>
                              raise Programmer_Error;

                           when Parsed =>
                              if Item_3.Char_Value = ')' then
                                 Item.Primary :=
                                   (Result_Label => Parsed,
                                    Value_Label  => Int,
                                    Remaining    => Item_3.Remaining,
                                    Int_Value    => Item_2.Int_Value);
                              end if;
                           when NoParse =>
                              null;
                           end case;
                        end;
                     when NoParse =>
                        null;
                     end case;
                  end;
               end if;

               if Item.Primary.Result_Label = Lazy then
                  --  Primary <= Decimal
                  Item.Primary := Decimal (Item);
               end if;

            when NoParse =>
               --  EOF.
               Item.Primary := (NoParse, Int);
            end case;
         end;
      end if;

      return Item.Primary;
   end Primary;

   function Decimal (Item : in out Derivs) return Result_Int
   is
      subtype Digit is Character range '0' .. '9';
   begin
      if Item.Decimal.Result_Label = Lazy then
         declare
            Item_1 : constant Result_Char := Item.Decimal.Arg.Char;
         begin
            Item.Decimal :=
              (case Item_1.Result_Label is
               when Lazy =>
                  --  Parse always sets Char
                  raise Programmer_Error,

               when Parsed =>
                  --  Decimal <= '0' / ... / '9'
                 (case Item_1.Char_Value is
                  when Digit =>
                    (Parsed, Int, Item_1.Remaining, Integer'Value ("" & Item_1.Char_Value)),
                  when others =>
                    (NoParse, Int)),
               when NoParse =>
                 (NoParse, Int));
         end;
      end if;

      return Item.Decimal;
   end Decimal;

   function Parse (Input : in String) return Derivs_Access
   is begin
      return D : constant Derivs_Access := new Derivs
      do
         D.Additive  := (Lazy, Int, D);
         D.Multitive := (Lazy, Int, D);
         D.Primary   := (Lazy, Int, D);
         D.Decimal   := (Lazy, Int, D);

         D.Char      :=
           (if Input'Length = 0 then (NoParse, Char)
            else
              (Result_Label => Parsed,
               Value_Label  => Char,
               Remaining    => Parse (Input (Input'First + 1 .. Input'Last)),
               Char_Value   => Input (Input'First)));
      end return;
   end Parse;

   procedure Force (Item : in out Derivs)
   is begin
      if Item.Additive.Result_Label = Lazy then
         Item.Additive := Additive (Item.Additive.Arg.all);
      end if;

      if Item.Multitive.Result_Label = Lazy then
         Item.Multitive := Multitive (Item.Multitive.Arg.all);
      end if;

      if Item.Primary.Result_Label = Lazy then
         Item.Primary := Primary (Item.Primary.Arg.all);
      end if;

      if Item.Decimal.Result_Label = Lazy then
         Item.Decimal := Decimal (Item.Decimal.Arg.all);
      end if;

   end Force;

   function Image (Item : in Result) return String
   is begin
      case Item.Result_Label is
      when Lazy | NoParse =>
         return Result_Labels'Image (Item.Result_Label);

      when Parsed =>
         case Item.Value_Label is
         when Int =>
            return Integer'Image (Item.Int_Value);
         when Char =>
            return Item.Char_Value & "";
         end case;
      end case;
   end Image;

   procedure Parse_Put (Input : in String)
   is
      use Ada.Text_IO;

      D : constant Derivs_Access := Parse (Input);
   begin
      Put (Input & " => ");

      Force (D.all);

      if D.Additive.Result_Label = Parsed then
         Put_Line (Image (D.Additive));

      elsif D.Multitive.Result_Label = Parsed then
         Put_Line (Image (D.Multitive));

      elsif D.Primary.Result_Label = Parsed then
         Put_Line (Image (D.Primary));

      elsif D.Decimal.Result_Label = Parsed then
         Put_Line (Image (D.Decimal));

      elsif D.Char.Result_Label = Parsed then
         Put_Line (Image (D.Char));

      else
         Put_Line ("that does not compute");
      end if;
   end Parse_Put;

begin
   Parse_Put ("2*(3+4)");
   Parse_Put ("");
   Parse_Put ("2");
   Parse_Put ("1+2");
exception
when E : others =>
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Ford_3_1_Packrat;
