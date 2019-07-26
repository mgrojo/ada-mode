--  File is saved with DOS line endings, to test lexer with that.
--
--EMACSCMD:(test-face "Ada" font-lock-function-name-face)
--EMACSCMD:(test-face "Text_IO" font-lock-function-name-face)
with
  Ada.Text_IO;

-- don't indent this comment with the previous; blank line between
--
-- No comment on the first line, to make sure we can handle that :)
-- blank on first line, to test beginning-of-buffer logic for "with-context"

-- testing ada-goto-declaration, ada-find-other-file with an Emacs
-- Ada project file.
--
-- .adp and .gpr is tested here
-- .gpr only is tested in ada_mode-find_file.adb
-- .adp only is tested in -- ada_mode-generic_instantiation.ads
--
-- 'eval' is not a safe local variable, so we can't use local
-- variables for this in batch mode.
--
--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")
--EMACSCMD:ada-prj-current-file
--EMACSRESULT:(expand-file-name "subdir/ada_mode.adp")

--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" font-lock-function-name-face)
--EMACSCMD:(progn (ada-goto-end)(looking-at "; -- end 1"))
--EMACSRESULT:t
with Ada.Strings.Unbounded; -- end 1

--EMACSCMD:(test-face "limited" font-lock-keyword-face)
--EMACSCMD:(test-face "private" font-lock-keyword-face)
--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" font-lock-function-name-face)
--EMACSCMD:(test-face "Streams" font-lock-function-name-face)
limited private with Ada.Streams,
  --EMACSCMD:(test-face "Ada" font-lock-function-name-face)
  --EMACSCMD:(test-face "Containers" font-lock-function-name-face)
  Ada.Containers;
--EMACSCMD:(test-face "limited" font-lock-keyword-face)
--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" font-lock-function-name-face)
--EMACSCMD:(progn (forward-line 1)(ada-find-other-file)(looking-at "package Ada.Strings.Bounded"))
limited with Ada.Strings.Bounded;
--EMACSRESULT:t
--EMACSCMD:(test-face "private" font-lock-keyword-face)
--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" font-lock-function-name-face)
--EMACSCMD:(test-face "Containers" font-lock-function-name-face)
--EMACSCMD:(test-face "Vectors" font-lock-function-name-face)
--EMACSCMD:(progn  (forward-line 2)(ada-find-other-file)(looking-at "package Ada.Containers.Vectors"))
--EMACSRESULT:t
private with Ada.Containers.Vectors,
  --EMACSCMD:(test-face "Ada" font-lock-function-name-face)
  --EMACSCMD:(test-face "Containers" font-lock-function-name-face)
  --EMACSCMD:(test-face "Bounded_Doubly_Linked_Lists" font-lock-function-name-face)
  Ada.Containers.Bounded_Doubly_Linked_Lists;

-- test ada-find-other-file on 'with subprogram-body'
--EMACSCMD:(progn (forward-line 1)(ada-find-other-file)(looking-at "function Ada_Mode.Library_Function return Integer; -- spec"))
with Ada_Mode.Library_Function;
--EMACSRESULT:t
--EMACSCMD:(progn (forward-line -2)(forward-word 4)(ada-goto-declaration)(looking-at "Library_Function return Integer; -- spec"))
--EMACSRESULT:t
--EMACSCMD:(progn (forward-line 1)(ada-find-other-file)(looking-at "procedure Ada_Mode.Library_Procedure is"))
with Ada_Mode.Library_Procedure;
--EMACSRESULT:t
-- test ada-find-other-file on 'with subprogram-spec'
--EMACSCMD:(progn (forward-line 1)(ada-find-other-file)(looking-at "function Ada_Mode.Function_2 return Boolean;"))
with Ada_Mode.Function_2;
--EMACSRESULT:t
--EMACSCMD:(progn (ada-goto-end)(looking-back "end Ada_Mode.Nominal"))
--EMACSRESULT:t
--EMACSCMD:(progn (beginning-of-line 3) (forward-sexp)(looking-at "is -- target 0"))
--EMACSRESULT:t
package Ada_Mode.Nominal
  with
    Spark_Mode => On
is -- target 0
   --EMACSCMD:(progn (beginning-of-line -0) (forward-sexp)(looking-at "private -- Ada_Mode.Nominal"))
   --EMACSRESULT:t

   --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 0"))

   --EMACSCMD:(test-face "pragma" font-lock-keyword-face)
   --EMACSCMD:(test-face "Elaborate_Body" font-lock-function-name-face)
   --EMACSCMD:(test-face "Ada_Mode" '(nil default))
   pragma Elaborate_Body (Ada_Mode.Nominal);

   -- Comment after one line of code; broken versions of the
   -- indentation engine aligned this with 'package'.

   -- Extensive coverage of type declarations
   --
   -- Most of these indentations are not likely to occur in practice
   -- (so we really don't care if they are 'right'); we are making
   -- sure the indentation engine doesn't hang or crash for odd cases.

   -- access to object
   -- some of the font-lock for this tested by access to function below
   --EMACSCMD:(test-face "type" font-lock-keyword-face)
   --EMACSCMD:(test-face "Object_Access_Type_0a" font-lock-type-face)
   --EMACSCMD:(test-face "is" font-lock-keyword-face)
   --EMACSCMD:(progn (end-of-line 3)(backward-word 2)(test-face "access" font-lock-keyword-face))
   --EMACSCMD:(test-face "Float" font-lock-type-face)
   type Object_Access_Type_0a is access Float;
   --EMACSCMD:(test-face "all" font-lock-keyword-face)
   --EMACSCMD:(test-face "Integer" font-lock-type-face)
   type Object_Access_Type_0b is access all Integer;
   --EMACSCMD:(test-face "not" font-lock-keyword-face)
   --EMACSCMD:(test-face "null" font-lock-keyword-face)
   --EMACSCMD:(progn (end-of-line 4)(backward-word 3)(test-face "access" font-lock-keyword-face))
   --EMACSCMD:(test-face "all" font-lock-keyword-face)
   --EMACSCMD:(test-face "Integer" font-lock-type-face)
   type Object_Access_Type_0c is not null access all Integer;
   --EMACSCMD:(test-face "constant" font-lock-keyword-face)
   --EMACSCMD:(test-face "Integer" font-lock-type-face)
   type Object_Access_Type_0d is not null access constant Integer;
   --EMACSCMD:(test-face "Integer" font-lock-type-face)
   type Object_Access_Type_0e is access constant Integer;
   type Object_Access_Type_0f is not null access constant Integer;
   type Object_Access_Type_1 is not null access all Integer
   ; -- we don't really care
   type Object_Access_Type_2a is not null access all
     Integer;
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(forward-char 3)(ada-identifier-at-point))
   type Object_Access_Type_2b is not null access constant
     Integer;
   --EMACSRESULT:"Object_Access_Type_2b"
   type Object_Access_Type_2c is not null access
     Integer;
   type Object_Access_Type_3a is not null access
     all Integer; -- font-lock not worth fixing here
   type Object_Access_Type_3b is not null access
     -- Comment between keywords
     constant Integer;
   type Object_Access_Type_4 is not null
     --EMACSCMD:(test-face "Integer" font-lock-type-face)
     access all Integer; -- it no longer matters whether this is 'all' or 'constant'

   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(downcase-word 4)(ada-case-adjust))
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(upcase-word 4)(ada-case-adjust))
   type Object_Access_Type_5a is not
     --EMACSCMD:(progn (forward-line -1)(forward-word 1)(forward-char 1)(let ((case-fold-search nil))(looking-at "Object_Access_Type_5a")))
     --EMACSRESULT:t
     --EMACSCMD:(progn (forward-line 1)(upcase-word 1)(ada-case-adjust)(let ((case-fold-search nil))(looking-back "null")))
     null access all Integer;
   --EMACSRESULT:t
   type Object_Access_Type_6 is
     not null access all Integer;
   type Object_Access_Type_6b is
     access all Integer;
   type Object_Access_Type_7
     is access all Integer;
   --EMACSCMD:(ada-which-function)
   --EMACSRESULT:"Ada_Mode.Nominal"

   --  Test case-adjust of keyword in comment
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(downcase-word -1)(ada-case-adjust-at-point t)(let ((case-fold-search nil))(looking-back "New")))
   --  New is a keyword in code, but not here

   type
     Object_Access_Type_8 is access all Integer;

   -- Not significantly different than previous, so we only do one.
   type Object_Access_Type_9 is access Integer;

   -- access to procedure
   -- most of the font-lock for this is tested by access to function below
   --EMACSCMD:(progn (forward-line 3)(search-forward "is")(test-face "access" font-lock-keyword-face))
   --EMACSCMD:(progn (forward-line 2)(search-forward "is")(test-face "protected" font-lock-keyword-face))
   --EMACSCMD:(progn (forward-line 1)(search-forward "is")(test-face "procedure" font-lock-keyword-face))
   type Procedure_Access_Type_1 is access protected procedure (A_Param : out Integer);
   --EMACSCMD:(ada-which-function)
   --EMACSRESULT:"Ada_Mode.Nominal"

   -- we don't put newline inside the paren here
   type Procedure_Access_Type_2 is access protected procedure
     (A_Param : out Integer);
   type Procedure_Access_Type_3 is access protected
     procedure (A_Param : out Integer);
   type Procedure_Access_Type_4 is access
     protected procedure (A_Param : out Integer);
   type Procedure_Access_Type_5 is
     -- no 'protected' to test that font-lock case
     --EMACSCMD:(test-face "procedure" font-lock-keyword-face)
     access procedure (A_Param : out Integer);
   type Procedure_Access_Type_6
     is access protected procedure (A_Param : out Integer);
   type
     Procedure_Access_Type_7 is access protected procedure (A_Param : out Integer);

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
   --EMACSCMD:(test-face-1 "is" "access" font-lock-keyword-face)
   --EMACSCMD:(test-face "protected" font-lock-keyword-face)
   --EMACSCMD:(test-face-1 "is" "function" font-lock-keyword-face)
   --EMACSCMD:(test-face "(" '(nil default))
   --EMACSCMD:(test-face "A_Param" '(nil default))
   --EMACSCMD:(test-face ":" '(nil default))
   --EMACSCMD:(test-face "in" font-lock-keyword-face)
   --EMACSCMD:(test-face "Float" font-lock-type-face)
   --EMACSCMD:(test-face "return" font-lock-keyword-face)
   --EMACSCMD:(test-face-1 "return" "Standard" font-lock-function-name-face)
   --EMACSCMD:(test-face-1 "return" "Float" font-lock-type-face)
   type Function_Access_Type_1a is access protected function (A_Param : in Float) return Standard.Float;
   --EMACSCMD:(test-face-1 "is" "access" font-lock-keyword-face)
   --EMACSCMD:(test-face "Standard" font-lock-function-name-face)
   --EMACSCMD:(test-face "Float" font-lock-type-face)
   type Function_Access_Type_1b is access protected function (A_Param : in Float) return access Standard.Float;
   --EMACSCMD:(test-face-1 "is" "constant" font-lock-keyword-face)
   --EMACSCMD:(test-face "Standard" font-lock-function-name-face)
   --EMACSCMD:(test-face "Float" font-lock-type-face)
   type Function_Access_Type_1d is access protected function (A_Param : in Float) return access constant Standard.Float;

   -- no 'protected' to test that font-lock case
   --EMACSCMD:(test-face-1 "is" "function" font-lock-keyword-face)
   type Function_Access_Type_2a is access function (A_Param : in Float) return Standard.
     Float;
   type Function_Access_Type_2b is access protected function (A_Param : in Float) return Standard
     .Float;
   type Function_Access_Type_2c is access protected function (A_Param : in Float) return
     Standard.Float;
   type Function_Access_Type_2d is access protected function (A_Param : in Float) return access
     Standard.Float;
   type Function_Access_Type_2g is access protected function return
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
   --EMACSCMD:(progn (forward-line -1)(ada-which-function))
   --EMACSRESULT:"Ada_Mode.Nominal"

   -- a pathological case
   type Function_Access_Type_11 is access
     protected function
       (A_Param : in Float)
       return access function
         (A_Param : in Float)
         return
           Standard.Float;

   --EMACSCMD:(test-face-1 "is" "array" font-lock-keyword-face)
   --EMACSCMD:(test-face "Integer" '(nil default))
   --EMACSCMD:(test-face "range" font-lock-keyword-face)
   --EMACSCMD:(test-face "of" font-lock-keyword-face)
   --EMACSCMD:(test-face "Object_Access_Type_1" font-lock-type-face)
   type Unconstrained_Array_Type_1 is array (Integer range <>, Standard.Character range <>) of Object_Access_Type_1;
   type Access_Unconstrained_Array_Type_1 is access Unconstrained_Array_Type_1 (1 .. 10, 'A' .. 'D');
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
   --EMACSCMD:(test-face-1 "is" "private" font-lock-keyword-face)
   type Private_Type_1 is abstract tagged limited private;
   --EMACSCMD:(progn (forward-line -1)(forward-word 1)(forward-char 1)(ada-goto-declaration)(looking-at "Private_Type_1 is abstract tagged limited null record;"))
   --EMACSRESULT:t
   -- result in same file

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
   --EMACSCMD:(test-face-1 "with" "private" font-lock-keyword-face)
   type Limited_Derived_Type_1 is abstract limited new Private_Type_1 with private;
   type Limited_Derived_Type_2 is abstract limited new Private_Type_1 with
     private;
   type Limited_Derived_Type_3 is abstract limited
     new Private_Type_1 with private;
   -- rest of Limited_Derived below, due to freezing rules

   --EMACSCMD:(test-face-1 "is" "null" font-lock-keyword-face)
   --EMACSCMD:(test-face-1 "is" "record" font-lock-keyword-face)
   type Null_Record_Type_1 is null record;
   type Null_Record_Type_2 is null
     record;
   type Null_Record_Type_3 is
     null record;
   type Null_Record_Type_4
     is null record;

   type Record_Type_1 is record
      --EMACSCMD:(progn (forward-line 1)(forward-word 2)(insert "   ")(ada-align))
      Component_1   : Integer := 1;
      Component_2   : Integer := 2;
      Component_356 : Float   := 3.0;
      -- longer component name, shorter type name for align test
   end record;
   --EMACSCMD:(test-face "Record_Type_1" font-lock-type-face)
   for Record_Type_1 use
      record
         --EMACSCMD:(progn (forward-line 1)(forward-word 2)(insert "   ")(ada-align))
         Component_1   at 0 range  0 .. 31;
         Component_2   at 0 range 32 .. 63;
         Component_356 at 0 range 64 .. 95;
      end record;
   for Record_Type_1'Size use 32 * 3;
   type Record_Type_2 is limited record
      Component_1 : Integer := 1;
      Component_2 : Integer := 2;
      Component_3 : Integer := 3;
   end record;
   for Record_Type_2 use record
      at mod 4;
      Component_1 at 0 range 0 .. 31;
      Component_2 at 0 range 32 .. 63;
      Component_3 at 0 range 64 .. 95;
   end record;
   for Record_Type_2'Size use 32 * 3;

   --EMACSCMD:(progn (ada-goto-end)(looking-back "end record"))
   --EMACSRESULT:t
   --EMACSCMD:(test-face-1 "access" "Standard" font-lock-function-name-face)
   --EMACSCMD:(test-face-1 "access" "Integer" font-lock-type-face)
   type Record_Type_3
     (Discriminant_1 : access Standard.Integer;
      --EMACSCMD:(test-face "Standard" font-lock-function-name-face)
      --EMACSCMD:(test-face "Integer" font-lock-type-face)
      Discriminant_2 : Standard.Integer;
      --EMACSCMD:(test-face "Ada_Mode" font-lock-function-name-face)
      --EMACSCMD:(test-face "Nominal" font-lock-function-name-face)
      --EMACSCMD:(test-face "Object_Access_Type_0a" font-lock-type-face)
      Discriminant_3 : not null Ada_Mode.Nominal.Object_Access_Type_0a)
      is tagged record
         --EMACSCMD:(progn (ada-goto-end)(looking-at "; -- end 2"))
         --EMACSRESULT:t
         Component_1 : Integer; -- end 2
         Component_2 :
           Integer;
         Component_3
           : Integer;
      end record;

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
   --EMACSCMD:(test-face-1 "digits" "10" font-lock-constant-face)
   type Decimal_Fixed_Point_1 is delta 0.10 digits 10;
   type Decimal_Fixed_Point_2 is delta 0.10 digits
     10;
   type Decimal_Fixed_Point_3 is delta 0.10
     digits 10;
   --EMACSCMD:(progn (beginning-of-line)(forward-line -1)(ada-which-function))
   --EMACSRESULT:"Ada_Mode.Nominal"

   type Decimal_Fixed_Point_4 is delta
     0.10 digits 10;
   type Decimal_Fixed_Point_5 is
     delta 0.10 digits 10;
   type Decimal_Fixed_Point_6
     is delta 0.10 digits 10;
   type
     Decimal_Fixed_Point_7 is delta 0.10 digits 10;

   -- These are not signicantly different, so only one.
   --EMACSCMD:(test-face "10" font-lock-constant-face)
   --EMACSCMD:(test-face "1.00e-6" font-lock-constant-face) ;; does not include '-'
   --EMACSCMD:(test-face "16#AF.42#" font-lock-constant-face)
   type Floating_Point is digits 10 range -1.00e-6 .. 16#AF.42#;

   --EMACSCMD:(test-face-1 "is" "mod" font-lock-keyword-face)
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

   -- result in other file
   --EMACSCMD:(progn (end-of-line 5)(backward-word 5)(ada-goto-declaration)(backward-word 1)(looking-at "body Protected_1 is"))
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line 2)(back-to-indentation) (forward-sexp)(looking-at "is -- Protected_1"))
   --EMACSRESULT:t
   protected type Protected_1 is -- Protected_1

      --EMACSCMD:(progn (end-of-line -1)(forward-word -3) (backward-sexp)(looking-at "protected type Protected_1"))
      --EMACSRESULT:t
      --EMACSCMD:(progn (end-of-line -3)(forward-word -3) (forward-sexp)(looking-at "private -- Protected_1"))
      --EMACSRESULT:t

      --EMACSCMD:(ada-which-function)
      --EMACSRESULT:"Protected_1"

      -- only two examples, to get 'protected' and 'is-entry_body' into grammar

      --EMACSCMD:(progn (ada-goto-end)(looking-at "; -- end 3"))
      --EMACSRESULT:t
      function F1 return Integer; -- end 3

      --EMACSCMD:(test-face "Discrete_Type_1" font-lock-type-face)
      function F2 (Param_1 : Discrete_Type_1; B : Float) return Float;
      --EMACSCMD:(progn (forward-line 3)(ada-which-function t))
      --EMACSRESULT:"E1"
      entry E1
        (X : Integer);
      entry E2 (X : Integer);
      entry E3 (X : Integer);
      procedure P1;
      procedure P2
        (A : Float;
         B : Float);

      --EMACSCMD:(progn (ada-goto-end)(looking-back "end Protected_1"))
      --EMACSRESULT:t
      -- This is a comment just before 'private'; broken versions of the
      -- indentation engine aligned this with 'private'.
   private -- Protected_1

      --EMACSCMD:(progn (end-of-line -1)(forward-word -3) (backward-sexp)(looking-at "is -- Protected_1"))
      --EMACSRESULT:t
      --EMACSCMD:(progn (end-of-line -3)(forward-word -3) (forward-sexp)(looking-at "; -- Protected_1"))
      --EMACSRESULT:t

      -- More than three objects, to be sure we are handling
      -- indefinite lists of objects properly
      --EMACSCMD:(test-face "Integer" font-lock-type-face)
      Local_1 : Integer;
      Local_2 : Integer;
      Local_3 : Integer;
      Local_4 : Integer;

      -- A comment just before 'end'
   end Protected_1; -- Protected_1

   type Protected_Interface_1 is protected interface;

   protected type Protected_Child_1
   with Convention => Ada
   is new Protected_Interface_1 with
      entry E1 (X : Integer);
   end Protected_Child_1;

   -- Ici l'exemple du chapitre 9 du RM sur le tasking

   --EMACSCMD:(progn (forward-line 2)(ada-find-other-file)(looking-at "protected body Protected_Buffer"))
   protected Protected_Buffer is
      --EMACSRESULT:t
      -- a single_protected_type

      --EMACSCMD:(ada-which-function)
      --EMACSRESULT:"Protected_Buffer"

      --EMACSCMD:(test-face "Character" font-lock-type-face)
      --EMACSCMD:(progn (end-of-line 2)(backward-word 1)(ada-align))
      entry Read (C : out Character);
      --EMACSCMD:(progn (end-of-line 2)(backward-word 1)(ada-align))
      entry Write (C : in Character);
   private
      --EMACSCMD:(progn (forward-line 2)(ada-align))
      -- align section does _not_ include entries above 'private'. result tested by diff
      Pool                : String(1 .. 100);
      Count               : Natural  := 0;
      In_Index, Out_Index : Positive := 1;
   end Protected_Buffer;

   ----------
   -- Objects

   Integer_A, Integer_B, Integer_C : Integer;
   Integer_D, Integer_E, Integer_F :
     Integer;
   Integer_G, Integer_H,
     Integer_I : Integer; -- different from ada-mode 4.01

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

   Anon_Array_1 : array (1 .. 10) of Integer;
   Anon_Array_2 : array (1 .. 10) of
     Integer;
   Anon_Array_3 : array (1 .. 10)
     of Integer;
   Anon_Array_4 : array
     (1 .. 10) of Integer;
   Anon_Array_5 :
     array (1 .. 10) of Integer;

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

   task type Task_Type_1 (Name : access String)
   with
     Storage_Size => 512 + 256
   is
      --EMACSCMD:(ada-which-function)
      --EMACSRESULT:"Task_Type_1"

      --EMACSCMD:(test-face "Start" 'font-lock-function-name-face)
      --EMACSCMD:(test-face "Discrete_Type_1" 'font-lock-type-face)
      entry Start (Discrete_Type_1) (Param_1 : in Integer);
      entry Middle_1 (Param_1 : in Integer);
      entry Middle_2 (Param_1 : in Integer);
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

   -- test that comment prefix is properly fontified, and that fill
   -- paragraph doesn't cause problems
   --
   --EMACSCMD:(progn (end-of-line 4)(delete-forward-char 6)(ada-fill-comment-paragraph)(font-lock-ensure)(forward-char 4)(syntax-class (syntax-after (point))))
   --EMACSRESULT: 11

   -- a filled comment. Now is the time for all good parsers to come
   -- to the aid of programmers.

   --EMACSCMD:(progn (forward-line 2)(forward-word 2)(insert "    ")(ada-fill-comment-paragraph 'full))

   -- a filled  and justified comment.  Now  is the time for  all good
   -- parsers to come to the aid of programmers.

   --EMACSCMD:(progn (forward-line 2)(forward-word 2)(insert "    ")(ada-fill-comment-paragraph 'full t))

   -- a filled and  justified postfix comment. Now is  the time for --
   -- all good parsers to come to the aid of programmers.           --

   not overriding procedure Procedure_1a (Item  : in out Parent_Type_1);
   --EMACSCMD:(progn (forward-line -1)(forward-word)(ada-which-function))
   --EMACSRESULT:"Procedure_1a"
   --EMACSCMD:(ada-which-function)
   --EMACSRESULT:"Ada_Mode.Nominal"

   not overriding
   procedure Procedure_1b
     (Item  : in out Parent_Type_1) is null;

   not
   overriding
   procedure
     Procedure_1c (Item  : in out Parent_Type_1);

   procedure Procedure_1d
     (Item   : in out Parent_Type_1;
      Item_1 : in     Character;
      Item_2 : out    Character)
     is null;
   --EMACSCMD:(progn (forward-line -5)(ada-which-function))
   --EMACSRESULT:"Procedure_1c"
   --EMACSCMD:(progn (forward-line -6)(ada-which-function))
   --EMACSRESULT:"Procedure_1d"
   --EMACSCMD:(progn (forward-line -7)(ada-which-function))
   --EMACSRESULT:"Procedure_1d"
   --EMACSCMD:(progn (forward-line -8)(ada-which-function))
   --EMACSRESULT:"Procedure_1d"
   --EMACSCMD:(progn (forward-line -9)(ada-which-function))
   --EMACSRESULT:"Procedure_1d"

   procedure Procedure_1e (Item   : in out Parent_Type_1;
                           Item_1 : in Character;
                           Item_2 : out Character)
     is null;

   not overriding
   procedure Procedure_1f (Item : in out Parent_Type_1);

   function Function_2a (Param : in Parent_Type_1) return Float;
   function Function_2b (Param : in Parent_Type_1) return
     Float;
   function Function_2c (Param : in Parent_Type_1)
                        return Float;
   function Function_2d
     (Param : in Parent_Type_1) return Float;
   not overriding function
     Function_2e (Param : in Parent_Type_1) return Float;

   --EMACSCMD:(progn (ada-goto-end)(looking-at "; -- end 5"))
   --EMACSRESULT:t
   not overriding
   function Function_2f
     (Param : in Parent_Type_1)
     return Float; -- end 5

   --EMACSCMD:(progn (end-of-line 3)(ada-which-function))
   --EMACSRESULT:"Function_2g"
   function Function_2g
     (Param : in Private_Type_1)
     return Float
     is abstract;
   --  comment after 'is abstract', aligned with 'function'

   function Function_2h (Param : in Parent_Type_1) return Float is (1.0); -- expression function

   Default_Parent : constant Parent_Type_1 :=
     (Parent_Element_1 => 1,
      Parent_Element_2 => 2.0,
      Parent_Element_3 => False);
   --EMACSCMD:(progn (forward-line -2)(ada-which-function))
   --EMACSRESULT:"Ada_Mode.Nominal"

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

   --EMACSCMD:(progn (ada-goto-end)(looking-back "end Ada_Mode.Nominal"))
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line 2) (forward-sexp)(looking-at "; -- Ada_Mode.Nominal"))
   --EMACSRESULT:t
private -- Ada_Mode.Nominal

   --EMACSCMD:(progn (forward-line -2) (backward-sexp)(looking-at "is -- target 0"))
   --EMACSRESULT:t

   type Private_Type_1 is abstract tagged limited null record;
   type Private_Type_2 is abstract tagged limited
      record
         Component_1 : Integer;
         Component_2 : Integer;
         Component_3 : Integer;
      end record; -- Ada mode 4.01 aligned this with "type"; this is better

   --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- Ada_Mode.Nominal"))

   type Limited_Derived_Type_1 is abstract limited new Private_Type_1 with
      record
         Component_1 : Integer := 0;
         Component_2 : Integer := 1;
         Component_3 : Integer := 2;
      end record
   with Pack => True;

   overriding function Function_2g (Param : in Limited_Derived_Type_1) return Float is abstract;
   overriding procedure Abstract_Procedure_1 (Item : access Limited_Derived_Type_1) is abstract;
   overriding procedure Abstract_Procedure_2 (Item : access Limited_Derived_Type_1) is abstract;
   overriding procedure Abstract_Procedure_3 (Item : access Limited_Derived_Type_1) is abstract;
   overriding procedure Abstract_Procedure_4 (Item : access Limited_Derived_Type_1) is abstract;
   overriding procedure Abstract_Procedure_5 (Item : access Limited_Derived_Type_1) is abstract;


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
     abstract limited new Private_Type_1 with
      record
         Component_1 : Integer;
         Component_2 : Integer;
         Component_3 : Integer;
      end record;

   type Limited_Derived_Type_2 is abstract limited new Private_Type_1 with null record;

   overriding function Function_2g (Param : in Limited_Derived_Type_2) return Float is abstract;
   overriding procedure Abstract_Procedure_1 (Item : access Limited_Derived_Type_2) is abstract;
   overriding procedure Abstract_Procedure_2 (Item : access Limited_Derived_Type_2) is abstract;
   overriding procedure Abstract_Procedure_3 (Item : access Limited_Derived_Type_2) is abstract;
   overriding procedure Abstract_Procedure_4 (Item : access Limited_Derived_Type_2) is abstract;
   overriding procedure Abstract_Procedure_5 (Item : access Limited_Derived_Type_2) is abstract;

   type Limited_Derived_Type_2a is abstract limited new Private_Type_1
      with record
         Component_1 : Integer;
      end record;

   type Limited_Derived_Type_3 is abstract limited new Private_Type_1
     with null record;

   overriding function Function_2g (Param : in Limited_Derived_Type_3) return Float is abstract;
   overriding procedure Abstract_Procedure_1 (Item : access Limited_Derived_Type_3) is abstract;
   overriding procedure Abstract_Procedure_2 (Item : access Limited_Derived_Type_3) is abstract;
   overriding procedure Abstract_Procedure_3 (Item : access Limited_Derived_Type_3) is abstract;
   overriding procedure Abstract_Procedure_4 (Item : access Limited_Derived_Type_3) is abstract;
   overriding procedure Abstract_Procedure_5 (Item : access Limited_Derived_Type_3) is abstract;

   type Limited_Derived_Type_4 is abstract limited new
     Private_Type_1 with null record;

   overriding function Function_2g (Param : in Limited_Derived_Type_4) return Float is abstract;
   overriding procedure Abstract_Procedure_1 (Item : access Limited_Derived_Type_4) is abstract;
   overriding procedure Abstract_Procedure_2 (Item : access Limited_Derived_Type_4) is abstract;
   overriding procedure Abstract_Procedure_3 (Item : access Limited_Derived_Type_4) is abstract;
   overriding procedure Abstract_Procedure_4 (Item : access Limited_Derived_Type_4) is abstract;
   overriding procedure Abstract_Procedure_5 (Item : access Limited_Derived_Type_4) is abstract;

   type Limited_Derived_Type_5 is abstract limited
     new Private_Type_1 with null record;

   overriding function Function_2g (Param : in Limited_Derived_Type_5) return Float is abstract;
   overriding procedure Abstract_Procedure_1 (Item : access Limited_Derived_Type_5) is abstract;
   overriding procedure Abstract_Procedure_2 (Item : access Limited_Derived_Type_5) is abstract;
   overriding procedure Abstract_Procedure_3 (Item : access Limited_Derived_Type_5) is abstract;
   overriding procedure Abstract_Procedure_4 (Item : access Limited_Derived_Type_5) is abstract;
   overriding procedure Abstract_Procedure_5 (Item : access Limited_Derived_Type_5) is abstract;

   type Limited_Derived_Type_6 is abstract
     limited new Private_Type_1 with null record;
   --EMACSCMD:(progn (forward-line -2)(ada-add-log-current-function))
   --EMACSRESULT:"Limited_Derived_Type_6"
   --EMACSCMD:(progn (forward-line -4)(ada-which-function))
   --EMACSRESULT:"Abstract_Procedure_5"

   overriding function Function_2g (Param : in Limited_Derived_Type_6) return Float is abstract;
   overriding procedure Abstract_Procedure_1 (Item : access Limited_Derived_Type_6) is abstract;
   overriding procedure Abstract_Procedure_2 (Item : access Limited_Derived_Type_6) is abstract;
   overriding procedure Abstract_Procedure_3 (Item : access Limited_Derived_Type_6) is abstract;
   overriding procedure Abstract_Procedure_4 (Item : access Limited_Derived_Type_6) is abstract;
   overriding procedure Abstract_Procedure_5 (Item : access Limited_Derived_Type_6) is abstract;

   -- rest covered by Private_Type_n

   type Incomplete_Type_1 (<>) is tagged;
   type Incomplete_Type_2 (<>) is
     tagged;
   type Incomplete_Type_3 (<>)
     is tagged;
   type Incomplete_Type_4
     (<>) is tagged;

end Ada_Mode.Nominal; -- Ada_Mode.Nominal

--EMACSCMD:(progn (forward-line -2) (forward-sexp)(backward-sexp)(looking-at "private -- Ada_Mode.Nominal"))
--EMACSRESULT:t
-- Local Variables:
-- fill-column: 70
-- End:
