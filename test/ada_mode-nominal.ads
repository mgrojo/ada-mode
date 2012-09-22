with Ada.Text_IO;
with Ada.Strings.Unbounded; -- FIXME: test 'use' in context clause
package Ada_Mode.Nominal is
   --  No comment on the first line, to make sure we can handle that :)

   pragma Elaborate_Body (Ada_Mode.Nominal);

   -- FIXME: review ARM list of declaration items, add missing.
   -- Aspects covered in FIXME:
   -- Constraints covered in FIXME:
   -- Indenting inside parens is covered in FIXME:
   -- 'overriding' covered in FIXME:
   -- indenting inside names covered in FIXME:
   -- record types coverd in FIXME:
   -- synchronized coverd in FIXME:

   -- Extensive coverage of type declarations
   --
   -- Most of these indentations are not likely to occur in practice
   -- (so we really don't care if they are 'right'); we are making
   -- sure the indentation engine doesn't hang or crash for odd cases.
   --
   -- FIXME: see (some other file) for alignment of multiple declarations
   -- within parens.
   --
   -- FIXME: add discriminants

   -- access to object
   type Object_Access_Type_0a is access Integer;
   type Object_Access_Type_0b is access all Integer;
   type Object_Access_Type_0c is not null access all Integer;
   type Object_Access_Type_0d is not null access all Integer;
   type Object_Access_Type_0e is access constant Integer;
   type Object_Access_Type_0f is not null access constant Integer;
   type Object_Access_Type_1 is not null access all Integer
      ; -- we don't really care
   type Object_Access_Type_2a is not null access all
      Integer;
   type Object_Access_Type_2b is not null access constant
      Integer;
   type Object_Access_Type_2c is not null access
      Integer;
   type Object_Access_Type_3a is not null access
      all Integer;
   type Object_Access_Type_3b is not null access
      constant Integer;
   type Object_Access_Type_4 is not null
      access all Integer; -- it no longer matters wither this is 'all' or 'constant'
   type Object_Access_Type_5a is not
      null access all Integer;
   type Object_Access_Type_6 is
      not null access all Integer;
   type Object_Access_Type_6b is
      access all Integer;
   type Object_Access_Type_7
      is access all Integer;
   type
      Object_Access_Type_8 is access all Integer;

   -- Not significantly different than previous, so we only do one.
   type Object_Access_Type_9 is access Integer;

   -- access to procedure
   type Procedure_Access_Type_1 is access protected procedure (A_Param : out Integer);
   type Procedure_Access_Type_2 is access protected procedure (A_Param : out Integer
   ); -- FIXME: from smie-indent-close!
   type Procedure_Access_Type_3 is access protected procedure (A_Param : out
                                                                  Integer); -- don't care
   type Procedure_Access_Type_4 is access protected procedure (A_Param :
      out Integer); -- don't care
   type Procedure_Access_Type_5 is access protected procedure (A_Param
                                                                  : out Integer); -- don't care
   type Procedure_Access_Type_6 is access protected procedure (
                                                               A_Param : out Integer);

   type Procedure_Access_Type_7 is access protected procedure
      (A_Param : out Integer);
   type Procedure_Access_Type_8 is access protected
      procedure (A_Param : out Integer);
   type Procedure_Access_Type_9 is access
      protected procedure (A_Param : out Integer);
   type Procedure_Access_Type_10 is
      access protected procedure (A_Param : out Integer);
   type Procedure_Access_Type_11
      is access protected procedure (A_Param : out Integer);
   type
      Procedure_Access_Type_12 is access protected procedure (A_Param : out Integer);

   -- A more typical case, not covered above (usually with multiple params)
   type Procedure_Access_Type_13 is access
      protected procedure
         (A_Param : out Integer);

   ----------
   -- access to function
   --
   -- We covered newline within paren above.
   type Function_Access_Type_1a is access protected function (A_Param : in Float) return Standard.Float;
   type Function_Access_Type_1b is access protected function (A_Param : in Float) return access Standard.Float;
   type Function_Access_Type_1c is access protected function (A_Param : in Float) return access all Standard.Float;
   type Function_Access_Type_1d is access protected function (A_Param : in Float) return access constant Standard.Float;
   type Function_Access_Type_2a is access protected function (A_Param : in Float) return Standard.
      Float;
   type Function_Access_Type_2b is access protected function (A_Param : in Float) return Standard
      .Float;
   type Function_Access_Type_2c is access protected function (A_Param : in Float) return
      Standard.Float;
   type Function_Access_Type_2d is access protected function (A_Param : in Float) return access
      Standard.Float;
   type Function_Access_Type_2e is access protected function (A_Param : in Float) return access all
      Standard.Float;
   type Function_Access_Type_2f is access protected function (A_Param : in Float) return access constant
      Standard.Float;
   type Function_Access_Type_2g is access protected function (A_Param : in Float) return
      access Standard.Float;
   type Function_Access_Type_3 is access protected function (A_Param : in Float)
      return Standard.Float;
   type Function_Access_Type_4 is access protected function
      (A_Param : in Float) return Standard.Float;
   type Function_Access_Type_5 is access protected
      function (A_Param : in Float) return Standard.Float;
   type Function_Access_Type_6 is access
      protected function (A_Param : in Float) return Standard.Float;
   type Function_Access_Type_7 is
      access protected function (A_Param : in Float) return Standard.Float;
   type Function_Access_Type_8
      is access protected function (A_Param : in Float) return Standard.Float;
   type
      Function_Access_Type_9 is access protected function (A_Param : in Float) return Standard.Float;
   -- comment aligned with 'type', which is the start of the previous statement

   type Unconstrained_Array_Type_1 is array (Integer range <>, Standard.Character range <>) of Object_Access_Type_1;
   type Unconstrained_Array_Type_2 is array (Integer range <>, Standard.Character range <>) of
      Object_Access_Type_1;
   type Unconstrained_Array_Type_3 is array (Integer range <>, Standard.Character range <>)
      of Object_Access_Type_1;
   type Unconstrained_Array_Type_4 is array
      (Integer range <>, Standard.Character range <>) of Object_Access_Type_1;
   type Unconstrained_Array_Type_5 is
      array (Integer range <>, Standard.Character range <>) of Object_Access_Type_1;
   type Unconstrained_Array_Type_6
      is array (Integer range <>, Standard.Character range <>) of Object_Access_Type_1;
   type
      Unconstrained_Array_Type_7 is array (Integer range <>, Standard.Character range <>) of Object_Access_Type_1;

   -- not really different than unconstrained array, so we only do one.
   type Constrained_Array_Type is array (Character) of Ada.Text_IO.Count;

   type Private_Type_1 is abstract tagged limited private;
   type Private_Type_2 is abstract tagged limited
      private;
   -- Rest 'null record' to avoid declaring full type
   type Private_Type_3 is abstract tagged
      limited null record;
   type Private_Type_4 is abstract
      tagged limited null record;
   type Private_Type_5 is
      abstract tagged limited null record;
   type Private_Type_6
      is abstract tagged limited null record;
   type
      Private_Type_7 is abstract tagged limited null record;

   type Limited_Derived_Type_1 is abstract limited new Private_Type_1 with private;
   type Limited_Derived_Type_2 is abstract limited new Private_Type_1 with
      private;
   -- rest of Limited_Derived below, due to freezing rules

   -- FIXME: add 'null record'

   type Discrete_Type_1 is (A, B, C);
   type Discrete_Type_2 is
      (A, B, C);
   type Discrete_Type_3
      is (A, B, C);
   type
      Discrete_Type_4 is (A, B, C);

   -- Numeric types
   type Decimal_Fixed_Point_1 is delta 0.10 digits 10;
   type Decimal_Fixed_Point_2 is delta 0.10 digits
      10;
   type Decimal_Fixed_Point_3 is delta 0.10
      digits 10;
   type Decimal_Fixed_Point_4 is delta
      0.10 digits 10;
   type Decimal_Fixed_Point_5 is
      delta 0.10 digits 10;
   type Decimal_Fixed_Point_6
      is delta 0.10 digits 10;
   type
      Decimal_Fixed_Point_7 is delta 0.10 digits 10;

   -- These are not signicantly different, so only one.
   type Floating_Point is digits 10;
   type Modular_Type is mod 10;
   type Ordinary_Fixed_Point is delta 0.10 range 10.0 .. 11.0;
   type Signed_Integer_Type is range 10 .. 11;

   protected type Protected_1 is
      -- only two examples, to get 'protected' and 'is-entry_body' into grammar

      function F1 return Integer;
      function F2 (A : Float; B : Float) return Float;
      entry E1 (X : Integer);
      entry E2 (X : Integer);
      procedure P1;
      procedure P2 (A : Float; B : Float);

      -- This is a comment just before 'private'; default smie
      -- indentation doesn't do what we want here.
   private

      -- More than three objects, to be sure we are handling
      -- indefinite lists of objects properly
      Local_1 : Integer;
      Local_2 : Integer;
      Local_3 : Integer;
      Local_4 : Integer;

      -- A comment just before 'end'
   end Protected_1;

   -- Ici l'exemple du chapitre 9 du RM sur le tasking

   protected Buffer is
      entry Read (C : out Character);
      entry Write (C : in  Character);
   private
      Pool      : String(1 .. 100);
      Count     : Natural := 0;
      In_Index, Out_Index : Positive := 1;
   end Buffer;

   ----------
   -- Objects
   Integer_A, Integer_B, Integer_C : Integer;
   Integer_D, Integer_E, Integer_F :
      Integer;
   Integer_G, Integer_H,
      Integer_I : Integer;
   Integer_J,
     Integer_K, Integer_L : Integer;

   Float_1 : aliased constant Float := 1.0;
   Float_2 : aliased constant Float :=
      1.0;
   Float_3 : aliased constant Float
      := 1.0;
   Float_4 : aliased constant
      Float := 1.0;
   Float_5 : aliased
     constant Float := 1.0;
   Float_6 :
     aliased constant Float := 1.0;
   Float_7
     : aliased constant Float := 1.0;

   -- Non-trivial type name
   P_1 : Ada.Strings.Unbounded.String_Access;
   P_2 : Ada.Strings.Unbounded.
      String_Access;
   P_3 : Ada.Strings.Unbounded
   .String_Access;
   P_4 : Ada.Strings.
      Unbounded.String_Access;
   P_5 : Ada.
      Strings.Unbounded.String_Access;
   P_6 : Ada
      .Strings.Unbounded.String_Access;
   P_7 :
      Ada.Strings.Unbounded.String_Access;
   P_8
      : Ada.Strings.Unbounded.String_Access;

   ----------
   -- Subprograms
   -- most 'is null' so we don't have to declare a body
   procedure Procedure_1a (Item  : in out Ada.Strings.Unbounded.Unbounded_String; New_Item : Character);
   procedure Procedure_1b
      (Item  : in out Ada.Strings.Unbounded.Unbounded_String; New_Item : Character) is null;
   procedure
      Procedure_1c (Item  : in out Ada.Strings.Unbounded.Unbounded_String; New_Item : Character) is null;

   procedure Procedure_2a;
   procedure
      Procedure_2b is null;

   procedure Procedure_3a is null;
   procedure Procedure_3b is
      null;
   procedure Procedure_3c
      is null;
   procedure
      Procedure_3d is null;

   procedure Abstract_Procedure_1 (Item : access Private_Type_1) is abstract;
   procedure Abstract_Procedure_2 (Item : access Private_Type_1) is
      abstract;
   procedure Abstract_Procedure_3 (Item : access Private_Type_1)
      is abstract;
   procedure Abstract_Procedure_4
      (Item : access Private_Type_1) is abstract;
   procedure
      Abstract_Procedure_5 (Item : access Private_Type_1) is abstract;

   function Function_1a return Float;
   function Function_1b return
      Float;
   function Function_1c
      return Float;
   function
      Function_1d return Float;

   function Function_2a (Param : in Integer) return Float;
   function Function_2b (Param : in Integer) return
      Float;
   function Function_2c (Param : in Integer)
      return Float;
   function Function_2d
      (Param : in Integer) return Float;
   function
      Function_2e (Param : in Integer) return Float;

   -- FIXME: nested packages

private
   type Private_Type_1 is abstract tagged limited null record;
   type Private_Type_2 is abstract tagged limited
   record
      Component_1 : Integer;
   end record;

   type Limited_Derived_Type_1 is abstract limited
      new Private_Type_1 with
   record
      Component_1 : Integer;
   end record;

   type Limited_Derived_Type_2 is abstract limited new Private_Type_1 with null record;

   type Limited_Derived_Type_3 is abstract limited new Private_Type_1
      with null record;
   type Limited_Derived_Type_4 is abstract limited new
      Private_Type_1 with null record;
   type Limited_Derived_Type_5 is abstract limited
      new Private_Type_1 with null record;
   type Limited_Derived_Type_6 is abstract
      limited new Private_Type_1 with null record;
   -- rest covered by Private_Type_n

   type Incomplete_Type_1 (<>) is tagged;
   type Incomplete_Type_2 (<>) is
      tagged;
   type Incomplete_Type_3 (<>)
      is tagged;
   type Incomplete_Type_4
      (<>) is tagged;
   type
      Incomplete_Type_5 (<>) is tagged;

end Ada_Mode.Nominal;
