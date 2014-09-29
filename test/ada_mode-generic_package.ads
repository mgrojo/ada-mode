-- This is to test the indentation of declarations in generics package declarations
--EMACSCMD:(font-lock-fontify-buffer)
--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")

--EMACSCMD:(ada-which-function)
--EMACSRESULT:(if (featurep 'ada-wisi) "" nil)
pragma License (GPL);

with Ada.Text_IO;
--EMACSCMD:(progn (forward-line 2)(ada-find-other-file nil)(looking-at "package Ada.Strings.Unbounded is"))
--EMACSRESULT: t
with Ada.Strings.Unbounded;
--EMACSCMD:(ada-which-function)
--EMACSRESULT:(if (featurep 'ada-wisi) "" nil)

--EMACSCMD:(progn (ada-next-statement-keyword)(looking-at "generic$"))
--EMACSRESULT: (if (featurep 'ada-wisi) t nil)
--EMACSCMD:(progn (forward-line 2) (ada-next-statement-keyword)(looking-at "package Ada_Mode.Generic_Package"))
--EMACSRESULT:(if (featurep 'ada-wisi) t nil)
generic
   --EMACSCMD:(ada-which-function)
   --EMACSRESULT:(if (featurep 'ada-wisi) "Ada_Mode.Generic_Package" nil)

   -- one of each kind of generic_formal_parameter_definition from arm Annex P

   -- Types

   type Object_Formal_Access_Type is not null access all Integer;
   type Procedure_Formal_Access_Type is access protected procedure (A_Param : out Integer);
   type Function_Formal_Access_Type is access protected function (A_Param : in Float) return Standard.Float;
   type Unconstrained_Formal_Array_Type is array (Integer range <>, Standard.Character range <>) of
     Object_Formal_Access_Type;
   type Constrained_Formal_Array_Type is array (Character) of Ada.Text_IO.Count;
   type Formal_Private_Type is abstract tagged limited private;
   type Interface_Type is task interface;

   --EMACSCMD:(font-lock-fontify-buffer);; don't wait for jit-lock

   --EMACSCMD:(test-face "Formal_Private_Type" 'font-lock-type-face)
   type Limited_Formal_Derived_Type is abstract limited new Formal_Private_Type with private;

   --EMACSCMD:(test-face "Interface_Type" 'font-lock-type-face)
   type Synchronized_Formal_Derived_Type is abstract synchronized new Formal_Private_Type and Interface_Type
     with private;
   type Incomplete_Type (<>) is tagged;
   type Formal_Discrete_Type is (<>);

   -- Numeric types
   type Formal_Decimal_Fixed_Point is delta <> digits <>;
   type Formal_Floating_Point is digits <>;
   type Formal_Modular_Type is mod <>;
   type Formal_Ordinary_Fixed_Point is delta <>;
   type Formal_Signed_Integer_Type is range <>;

   -- Objects
   A, B, C : Integer;
   F : in out Float;
   --EMACSCMD:(test-face "in" font-lock-keyword-face)
   --EMACSCMD:(test-face "not" font-lock-keyword-face)
   --EMACSCMD:(test-face "null" font-lock-keyword-face)
   --EMACSCMD:(test-face "Ada" font-lock-type-face)
   P : in not null Ada.Strings.Unbounded.String_Access;
   --EMACSCMD:(test-face "in" font-lock-keyword-face)
   --EMACSCMD:(test-face "out" font-lock-keyword-face)
   --EMACSCMD:(test-face "not" font-lock-keyword-face)
   --EMACSCMD:(test-face "null" font-lock-keyword-face)
   Q : in out not null Ada.Strings.Unbounded.String_Access;

   -- Subprograms
   with procedure Concrete_Defaulted_Procedure
     (Item  : in out Ada.Strings.Unbounded.Unbounded_String;
      New_Item : Character)
     is Ada.Strings.Unbounded.Append;

   --EMACSCMD:(test-face "with" font-lock-keyword-face)
   --EMACSCMD:(test-face "procedure" font-lock-keyword-face)
   --EMACSCMD:(test-face "Concrete_Defaulted_Procedure_2" font-lock-function-name-face)
   --EMACSCMD:(test-face "is" font-lock-keyword-face)
   with procedure Concrete_Defaulted_Procedure_2 is <>;
   with procedure Concrete_Defaulted_Procedure_3 is null;
   with procedure Concrete_Procedure;
   with procedure Abstract_Defaulted_Procedure (Item : access Formal_Private_Type) is abstract <>;
   with procedure Abstract_Procedure (Item : out Formal_Private_Type) is abstract;
   --EMACSCMD:(test-face "with" font-lock-keyword-face)
   --EMACSCMD:(test-face "function" font-lock-keyword-face)
   --EMACSCMD:(test-face "Concrete_Function_1" font-lock-function-name-face)
   --EMACSCMD:(test-face "return" font-lock-keyword-face)
   -- "Float" is _not_ font-lock-type-face, because that requires parsing, which we don't do just for font-lock
   --EMACSCMD:(test-face "is" font-lock-keyword-face)
   with function Concrete_Function_1 return Float is <>;
   with function Concrete_Function_2 return Float
     is <>;
   with function Concrete_Function_3
          return Float is <>;
   with function Concrete_Function_4
     (Foo : in Integer)
     return Float is <>;

   -- Packages

   --EMACSCMD:(test-face "Ada.Text_IO.Integer_IO" 'font-lock-function-name-face)
   with package A_Package_1 is new Ada.Text_IO.Integer_IO (Num => Formal_Signed_Integer_Type);
   with package A_Package_2 is new Ada.Text_IO.Integer_IO (Num =>
                                                             Formal_Signed_Integer_Type);
   with package A_Package_3 is new Ada.Text_IO.Integer_IO (
                                                           Num => Formal_Signed_Integer_Type);

   --EMACSCMD:(test-face "Ada.Text_IO.Integer_IO" 'font-lock-function-name-face)
   with package A_Package_4 is new Ada.Text_IO.Integer_IO
     (Num => Formal_Signed_Integer_Type);
   with package A_Package_5 is new Ada.Text_IO.
     Integer_IO (Num => Formal_Signed_Integer_Type);

   -- multi-line font-lock doesn't work
   with package A_Package_6 is new
     Ada.Text_IO.Integer_IO (Num => Formal_Signed_Integer_Type);
   with package A_Package_7 is
     new Ada.Text_IO.Integer_IO (Num => Formal_Signed_Integer_Type);
   with package A_Package_8
     is new Ada.Text_IO.Integer_IO (Num => Formal_Signed_Integer_Type);

   --EMACSCMD:(progn (forward-line 2)(test-face "Ada.Text_IO.Integer_IO" 'font-lock-function-name-face))
   with package
     A_Package_9 is new Ada.Text_IO.Integer_IO (Num => Formal_Signed_Integer_Type);
   with
     package A_Package_10 is new Ada.Text_IO.Integer_IO (Num => Formal_Signed_Integer_Type);
   pragma Unreferenced (A_Package_10);

package Ada_Mode.Generic_Package is
   -- See ada_mode-generic_parent.ads for generic subprograms
end Ada_Mode.Generic_Package;
