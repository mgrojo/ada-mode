
with
  Ada.Text_IO; -- font-lock doesn't work across newline

-- don't indent this comment with the previous; blank line between
--EMACSCMD:(font-lock-fontify-buffer)
--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" font-lock-constant-face)
with Ada.Strings.Unbounded;
--EMACSCMD:(test-face "limited" font-lock-keyword-face)
--EMACSCMD:(test-face "private" font-lock-keyword-face)
--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada.Strings" font-lock-constant-face)
limited private with Ada.Strings.Bounded,
                  --EMACSCMD:(test-face "Ada.Containers" 'default)
                  Ada.Containers;
--EMACSCMD:(test-face "limited" font-lock-keyword-face)
--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" font-lock-constant-face)
limited with Ada.Strings.Bounded,
          Ada.Containers;
--EMACSCMD:(test-face "private" font-lock-keyword-face)
--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" font-lock-constant-face)
private with Ada.Containers.Vectors,
          Ada.Containers.Bounded_Doubly_Linked_Lists;
package Ada_Mode.Nominal is
   --  No comment on the first line, to make sure we can handle that :)
   --  blank on first line, to test beginning-of-buffer logic for "with-context"

   -- ada-indent-refine-subprogram calls ada-indent-is-generic-p,
   -- which can scan the entire buffer looking for generic. That can
   -- blow the lisp stack if enough occurences of unrefined "function"
   -- or "procedure" are encountered, which was the case is in this
   -- buffer, when indenting "function Function_2f". So we force
   -- indenting that first, to test that problem is fixed. EMACSCMD:
   -- (progn (forward-line) (search-forward "function Function_2f")
   -- (indent-for-tab-command))

   --EMACSCMD:(test-face "pragma" font-lock-keyword-face)
   --EMACSCMD:(test-face "Elaborate_Body" font-lock-function-name-face)
   --EMACSCMD:(test-face "Ada_Mode" 'default)
   pragma Elaborate_Body (Ada_Mode.Nominal);

   -- Comment after one line of code; broken versions of the
   -- indentation engine aligned this with 'package'.

   -- FIXME: review ARM list of declaration items, add missing.
   -- Aspects covered in FIXME:
   -- Constraints covered in FIXME:
   -- Indenting inside parens is covered in ada_mode-parens.ads, .adb
   -- 'overriding' covered in ada_mode-nominal-child.ads
   -- indenting inside names covered in FIXME:
   -- record types covered in FIXME:
   -- synchronized covered in FIXME:

   -- Extensive coverage of type declarations
   --
   -- Most of these indentations are not likely to occur in practice
   -- (so we really don't care if they are 'right'); we are making
   -- sure the indentation engine doesn't hang or crash for odd cases.

   -- access to object
   -- some of the font-lock for this tested by access to function below
   --EMACSCMD:(test-face "Integer" font-lock-type-face)
   type Object_Access_Type_0a is access Integer;
   --EMACSCMD:(test-face "all" font-lock-keyword-face)
   --EMACSCMD:(test-face "Integer" font-lock-type-face)
   type Object_Access_Type_0b is access all Integer;
   --EMACSCMD:(test-face "not" font-lock-keyword-face)
   --EMACSCMD:(test-face "null" font-lock-keyword-face)
   --EMACSCMD:(test-face "Integer" font-lock-type-face)
   type Object_Access_Type_0c is not null access all Integer;
   --EMACSCMD:(test-face "Integer" font-lock-type-face)
   type Object_Access_Type_0d is not null access constant Integer;
   --EMACSCMD:(test-face "Integer" font-lock-type-face)
   type Object_Access_Type_0e is access constant Integer;
   --EMACSCMD:(test-face "Integer" font-lock-type-face)
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
     all Integer; -- font-lock not worth fixing here
   type Object_Access_Type_3b is not null access
     -- Comment between keywords
     constant Integer;
   type Object_Access_Type_4 is not null
     --EMACSCMD:(test-face "Integer" font-lock-type-face)
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
   -- most of the font-lock for this is tested by access to function below
   --EMACSCMD:(test-face "procedure (" font-lock-keyword-face)
   type Procedure_Access_Type_1 is access protected procedure (A_Param : out Integer);
   -- we don't put newline inside the paren here
   type Procedure_Access_Type_2 is access protected procedure
     (A_Param : out Integer);
   type Procedure_Access_Type_3 is access protected
     procedure (A_Param : out Integer);
   type Procedure_Access_Type_4 is access
     protected procedure (A_Param : out Integer);
   type Procedure_Access_Type_5 is
     -- no 'protected' to test that font-lock case
     --EMACSCMD:(test-face "procedure (" font-lock-keyword-face)
     access procedure (A_Param : out Integer);
   type Procedure_Access_Type_6
     is access protected procedure (A_Param : out Integer);
   type
     Procedure_Access_Type_7 is access protected procedure (A_Param : out Integer);
   --  FIXME: Procedure_Access_Type_7 not highlighted here.

   -- A more typical case, not covered above (usually with multiple
   -- params). The parameters should be indented relative to the line
   -- 'procedure' is on.
   type Procedure_Access_Type_8 is access
     protected procedure
       (A_Param : out Integer);

   ----------
   -- access to function
   --
   -- We covered newline within paren above.
   --EMACSCMD:(test-face "type" font-lock-keyword-face)
   --EMACSCMD:(test-face "Function_Access_Type_1a" font-lock-type-face)
   --EMACSCMD:(test-face "is" font-lock-keyword-face)
   --EMACSCMD:(test-face "access protected" font-lock-keyword-face)
   --EMACSCMD:(test-face "protected" font-lock-keyword-face)
   --EMACSCMD:(test-face "function (" font-lock-keyword-face)
   --EMACSCMD:(test-face "(" 'default)
   --EMACSCMD:(test-face "A_Param" 'default)
   --EMACSCMD:(test-face ":" 'default)
   --EMACSCMD:(test-face "in" font-lock-keyword-face)
   --EMACSCMD:(test-face "Float" font-lock-type-face)
   --EMACSCMD:(test-face "return" font-lock-keyword-face)
   --EMACSCMD:(test-face "Standard.Float" font-lock-type-face)
   type Function_Access_Type_1a is access protected function (A_Param : in Float) return Standard.Float;
   --EMACSCMD:(test-face "access Standard.Float" font-lock-keyword-face)
   --EMACSCMD:(test-face "Standard.Float" font-lock-type-face)
   type Function_Access_Type_1b is access protected function (A_Param : in Float) return access Standard.Float;
   --EMACSCMD:(test-face "constant Standard.Float" font-lock-keyword-face)
   --EMACSCMD:(test-face "Standard.Float" font-lock-type-face)
   type Function_Access_Type_1d is access protected function (A_Param : in Float) return access constant Standard.Float;

   -- no 'protected' to test that font-lock case
   --EMACSCMD:(test-face "function (" font-lock-keyword-face)
   type Function_Access_Type_2a is access function (A_Param : in Float) return Standard.
     Float;
   type Function_Access_Type_2b is access protected function (A_Param : in Float) return Standard
     .Float;
   type Function_Access_Type_2c is access protected function (A_Param : in Float) return
     Standard.Float;
   type Function_Access_Type_2d is access protected function (A_Param : in Float) return access
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

   -- A more typical case
   type Function_Access_Type_10 is access
     protected function
       (A_Param : in Float)
       return Standard.Float;

   -- a pathological case
   type Function_Access_Type_11 is access
     protected function
       (A_Param : in Float)
       return access function
         (A_Param : in Float)
         return
         Standard.Float;

   --EMACSCMD:(test-face "array (" font-lock-keyword-face)
   --EMACSCMD:(test-face "Integer" 'default)
   --EMACSCMD:(test-face "range" font-lock-keyword-face)
   --EMACSCMD:(test-face "of" font-lock-keyword-face)
   --EMACSCMD:(test-face "Object_Access_Type_1" font-lock-type-face)
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

   --EMACSCMD:(test-face "abstract" font-lock-keyword-face)
   --EMACSCMD:(test-face "tagged" font-lock-keyword-face)
   --EMACSCMD:(test-face "limited" font-lock-keyword-face)
   --EMACSCMD:(test-face "private;" font-lock-keyword-face)
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

   --EMACSCMD:(test-face "new" font-lock-keyword-face)
   --EMACSCMD:(test-face "Private_Type_1" font-lock-type-face)
   --EMACSCMD:(test-face "with" font-lock-keyword-face)
   --EMACSCMD:(test-face "private;" font-lock-keyword-face)
   type Limited_Derived_Type_1 is abstract limited new Private_Type_1 with private;
   type Limited_Derived_Type_2 is abstract limited new Private_Type_1 with
     private;
   -- rest of Limited_Derived below, due to freezing rules

   --EMACSCMD:(test-face "null record" font-lock-keyword-face)
   --EMACSCMD:(test-face "record;" font-lock-keyword-face)
   type Null_Record_Type_1 is null record;
   type Null_Record_Type_2 is null
      record;
   type Null_Record_Type_3 is
     null record;
   type Null_Record_Type_4
     is null record;

   type Discrete_Type_1 is (A, B, C);
   type Discrete_Type_2 is
     (A, B, C);
   type Discrete_Type_3
     is (A, B, C);
   type
     Discrete_Type_4 is (A, B, C);

   ----------
   -- Numeric types
   --EMACSCMD:(test-face "delta" font-lock-keyword-face)
   --EMACSCMD:(test-face "0.10" font-lock-constant-face)
   --EMACSCMD:(test-face "digits" font-lock-keyword-face)
   --EMACSCMD:(test-face "10;" font-lock-constant-face)
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
   --EMACSCMD:(test-face "mod " font-lock-keyword-face)
   type Modular_Type is mod 10;
   --EMACSCMD:(test-face "10.0" font-lock-constant-face)
   type Ordinary_Fixed_Point is delta 0.10 range 10.0 .. 11.0;
   type Signed_Integer_Type is range 10 .. 21;

   --EMACSCMD:(test-face "Subtype_1" font-lock-type-face)
   --EMACSCMD:(test-face "Signed_Integer_Type" font-lock-type-face)
   subtype Subtype_1 is Signed_Integer_Type range 10 .. 20;
   subtype Subtype_2 is Signed_Integer_Type range 10 ..
     20;
   subtype Subtype_3 is Signed_Integer_Type range 10
     .. 20;
   subtype Subtype_4 is Signed_Integer_Type range
     10 .. 20;
   subtype Subtype_5 is Signed_Integer_Type
     range 10 .. 20;
   subtype Subtype_6 is
     Signed_Integer_Type range 10 .. 20;
   subtype
     Subtype_7 is Signed_Integer_Type range 10 .. 20;

   protected type Protected_1 is
      -- only two examples, to get 'protected' and 'is-entry_body' into grammar

      function F1 return Integer;
      --EMACSCMD:(test-face "Discrete_Type_1" font-lock-type-face)
      function F2 (Param_1 : Discrete_Type_1; B : Float) return Float;
      entry E1
        (X : Integer);
      entry E2 (X : Integer);
      procedure P1;
      procedure P2
        (A : Float;
         B : Float);

      -- This is a comment just before 'private'; broken versions of the
      -- indentation engine aligned this with 'private'.
   private

      -- More than three objects, to be sure we are handling
      -- indefinite lists of objects properly
      --EMACSCMD:(test-face "Integer" font-lock-type-face)
      Local_1 : Integer;
      Local_2 : Integer;
      Local_3 : Integer;
      Local_4 : Integer;

      -- A comment just before 'end'
   end Protected_1;

   -- Ici l'exemple du chapitre 9 du RM sur le tasking

   protected Buffer is
      -- a single_protected_type
      --EMACSCMD:(test-face "Character" font-lock-type-face)
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
     Integer_I : Integer; -- ada-mode 4.01 uses broken-indent

   Integer_J,
     Integer_K,
     Integer_L, Integer_M : Integer;

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

   -- FIXME: anonymous array ...

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

   task type Task_Type_1 (Name : access String) is
      entry Start (Discrete_Type_1) (Param_1 : in integer);
      entry Middle_1 (Param_1 : in integer);
      entry Middle_2 (Param_1 : in integer);
      entry Finish;
   end Task_Type_1;

   ----------
   -- Subprograms
   --
   -- most 'is null' so we don't have to declare a body

   -- We make these procedures primitive operations, so we can test
   -- 'overriding' in ada_mode-nominal-child.ads

   type Parent_Type_1 is tagged record
      Parent_Element_1 : Integer;
      Parent_Element_2 : Float;
      Parent_Element_3 : Boolean;
   end record;

   procedure Procedure_1a (Item  : in out Parent_Type_1);

   procedure Procedure_1b
     (Item  : in out Parent_Type_1) is null;

   procedure
     Procedure_1c (Item  : in out Parent_Type_1) is null;

   procedure Procedure_1d
     (Item   : in out Parent_Type_1;
      Item_1 : in     Character;
      Item_2 : out    Character)
     is null;

   procedure Procedure_1e (Item   : in out Parent_Type_1;
                           Item_1 : in Character;
                           Item_2 : out Character)
     is null;

   function Function_2a (Param : in Parent_Type_1) return Float;
   function Function_2b (Param : in Parent_Type_1) return
     Float;
   function Function_2c (Param : in Parent_Type_1)
                        return Float;
   function Function_2d
     (Param : in Parent_Type_1) return Float;
   function
     Function_2e (Param : in Parent_Type_1) return Float;

   function Function_2f
     (Param : in Parent_Type_1)
     return Float;

   function Function_2g
     (Param : in Private_Type_1)
     return Float
     is abstract;
   --  comment after 'is abstract', aligned with 'function'

   Default_Parent : constant Parent_Type_1 :=
     (Parent_Element_1 => 1,
      Parent_Element_2 => 2.0,
      Parent_Element_3 => False);

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

   ----------
   -- nested packages

   package Separate_Package_1 is
      procedure Separate_Procedure_1;
      procedure Separate_Procedure_2 (Item : in Integer);
   end Separate_Package_1;

private
   type Private_Type_1 is abstract tagged limited null record;
   type Private_Type_2 is abstract tagged limited
      record
         Component_1 : Integer;
         Component_2 : Integer;
         Component_3 : Integer;
      end record; -- Ada mode 4.01 aligned this with "type"; this is better

   type Limited_Derived_Type_1 is abstract limited new Private_Type_1
      with record
         Component_1 : Integer;
         Component_2 : Integer;
         Component_3 : Integer;
      end record
     with Pack => True; -- FIXME: hanging; ok? ask list, check GPS

   type Limited_Derived_Type_1a is abstract limited new
      Private_Type_1 with record
         Component_1 : Integer;
         Component_2 : Integer;
         Component_3 : Integer;
      end record;

   type Limited_Derived_Type_1b is abstract limited
      new Private_Type_1 with record
         Component_1 : Integer;
         Component_2 : Integer;
         Component_3 : Integer;
      end record;

   type Limited_Derived_Type_1c is abstract
      limited new Private_Type_1 with record -- Ada mode 4.01 aligned this with "type"; this is better
         Component_1 : Integer;
         Component_2 : Integer;
         Component_3 : Integer;
      end record;

   type Limited_Derived_Type_1d is
      abstract limited new Private_Type_1 with record
         Component_1 : Integer;
         Component_2 : Integer;
         Component_3 : Integer;
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
