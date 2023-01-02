
-- No comment on the first line, to make sure we can handle that :)
-- blank on first line, to test beginning-of-buffer logic for "with-context"

-- testing xref-find-definitions, ada-find-other-file with an Emacs
-- Ada project file.
--
-- .adp and .gpr is tested here
-- .gpr only is tested in ada_mode-find_file.adb
-- .adp only is tested in ada_mode-generic_instantiation.ads

-- eglot needs a gpr file to start the language server, so it needs a
-- wisi project before test-face.
--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))

--EMACSCMD:(when (eql ada-xref-backend 'eglot) (ada-eglot-wait-indexing-done))

--EMACSCMD:(test-face "Ada" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
--EMACSCMD:(test-face "Text_IO" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
with -- context_clause_start
  Ada.Text_IO;

--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
--EMACSCMD:(when wisi-parser-shared (wisi-goto-statement-end)(looking-at "; -- end 1"))
--EMACSRESULT:(not (null wisi-parser-shared))
with Ada.Strings.Unbounded; -- end 1

--EMACSCMD:(test-face "limited" font-lock-keyword-face)
--EMACSCMD:(test-face "private" font-lock-keyword-face)
--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
--EMACSCMD:(test-face "Streams" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
limited private with Ada.Streams,
  --EMACSCMD:(test-face "Ada" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
  --EMACSCMD:(test-face "Containers" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
  Ada.Containers;
--EMACSCMD:(test-face "limited" font-lock-keyword-face)
--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
--EMACSCMD:(progn (forward-line 1)(ada-find-other-file)(looking-at "package Ada.Strings.Bounded"))
limited with Ada.Strings.Bounded;
--EMACSRESULT:t
--EMACSCMD:(test-face "private" font-lock-keyword-face)
--EMACSCMD:(test-face "with" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
--EMACSCMD:(test-face "Containers" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
--EMACSCMD:(test-face "Vectors" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
--EMACSCMD:(progn  (forward-line 2)(ada-find-other-file)(looking-at "package Ada.Containers.Vectors"))
--EMACSRESULT:t
private with Ada.Containers.Vectors,
  --EMACSCMD:(test-face "Ada" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
  --EMACSCMD:(test-face "Containers" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
  --EMACSCMD:(test-face "Bounded_Doubly_Linked_Lists" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
  Ada.Containers.Bounded_Doubly_Linked_Lists;

-- test ada-find-other-file on 'with subprogram-body'
--EMACSCMD:(progn (forward-line 2)(ada-find-other-file)(looking-at "function Ada_Mode.Library_Function return Integer; -- spec"))
--EMACSRESULT:t
with Ada_Mode.Library_Function;
--EMACSCMD:(when wisi-parser-shared (forward-line -1)(forward-word 4)(call-interactively 'wisi-goto-spec/body)(looking-at "Library_Function return Integer; -- spec"))
--EMACSRESULT:(not (null wisi-parser-shared))
--EMACSCMD:(progn (forward-line -4)(test-all-defs "Ada_Mode.Library_Function"))
--EMACSRESULT_START:(cl-ecase ada-xref-backend (eglot '("ada_mode-library_function.ads" "function Ada_Mode.Library_Function return Integer; -- spec")) (gpr_query '("ada_mode-library_function.ads" "Library_Function function")) (gnat '("ada_mode-library_function.ads" "Library_Function spec")))
--EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-library_function.adb" "Library_Function body")) (gnat '("ada_mode-library_function.adb" "Library_Function body")))
--EMACSRESULT_FINISH:

--EMACSCMD:(progn (forward-line 1)(ada-find-other-file)(looking-at "procedure Ada_Mode.Library_Procedure is"))
with Ada_Mode.Library_Procedure;
--EMACSRESULT:t
-- test ada-find-other-file on 'with subprogram-spec'
--EMACSCMD:(progn (forward-line 1)(ada-find-other-file)(looking-at "function Ada_Mode.Function_2 return Boolean;"))
with Ada_Mode.Function_2; -- context_clause_end

--EMACSCMD:(when wisi-parser-shared (goto-char (car (ada-context-clause-region)))(looking-at "with -- context_clause_start"))
--EMACSRESULT:(not (null wisi-parser-shared))
--EMACSCMD:(when wisi-parser-shared (goto-char (cdr (ada-context-clause-region)))(looking-at "package Ada_Mode.Nominal -- context_clause_end"))
--EMACSRESULT:(not (null wisi-parser-shared))

--EMACSCMD:(when wisi-parser-shared (wisi-goto-statement-end)(looking-back "end Ada_Mode.Nominal"))
--EMACSRESULT:(not (null wisi-parser-shared))
--EMACSCMD:(when wisi-parser-shared (beginning-of-line 3) (forward-sexp)(looking-at "is -- target 0"))
--EMACSRESULT:(not (null wisi-parser-shared))
package Ada_Mode.Nominal -- context_clause_end
with
  Spark_Mode => On
is -- target 0
   --EMACSCMD:(when wisi-parser-shared (beginning-of-line -0) (forward-sexp)(looking-at "private -- Ada_Mode.Nominal"))
   --EMACSRESULT:(not (null wisi-parser-shared))

   --EMACSCMD:(when wisi-parser-shared (ada-goto-declarative-region-start)(looking-at " -- target 0"))
   --EMACSRESULT:(not (null wisi-parser-shared))

   --EMACSCMD:(test-face "pragma" font-lock-keyword-face)
   --EMACSCMD:(test-face "Elaborate_Body" (cl-ecase ada-face-backend (eglot '()) (wisi font-lock-function-name-face)))
   --EMACSCMD:(test-face "Ada_Mode" (cl-ecase ada-face-backend (eglot 'lsp-namespace) (wisi nil)))
   --EMACSCMD:(test-face "Nominal" (cl-ecase ada-face-backend (eglot 'lsp-namespace) (wisi nil)))
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
   --EMACSCMD:(test-face "Object_Access_Type_0a" (cl-ecase ada-face-backend (eglot '(lsp-declaration lsp-type)) (wisi font-lock-type-face)))
   --EMACSCMD:(test-face "is" font-lock-keyword-face)
   --EMACSCMD:(test-face-1 "is" "access" font-lock-keyword-face)
   --EMACSCMD:(test-face "Float" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-static lsp-type)) (wisi nil)))
   type Object_Access_Type_0a is access Float;
   --EMACSCMD:(test-face "all" font-lock-keyword-face)
   type Object_Access_Type_0b is access all Integer;
   --EMACSCMD:(test-face "not" font-lock-keyword-face)
   --EMACSCMD:(test-face "null" font-lock-keyword-face)
   --EMACSCMD:(test-face-1 "is" "access" font-lock-keyword-face)
   --EMACSCMD:(test-face "all" font-lock-keyword-face)
   --EMACSCMD:(test-face "Integer" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-static lsp-type)) (wisi nil)))
   type Object_Access_Type_0c is not null access all Integer;
   --EMACSCMD:(test-face "constant" font-lock-keyword-face)
   type Object_Access_Type_0d is not null access constant Integer;
   type Object_Access_Type_0e is access constant Integer;
   type Object_Access_Type_0f is not null access constant Integer;
   type Object_Access_Type_1 is not null access all Integer
     ; -- we don't really care about indent here
   type Object_Access_Type_2a is not null access all
     Integer;
   --EMACSCMD:(when wisi-parser-shared (forward-line 1)(forward-word 1)(forward-char 3)(nth 2 (wisi-prj-identifier-at-point (project-current))))
   type Object_Access_Type_2b is not null access constant
     Integer;
   --EMACSRESULT:(when wisi-parser-shared "Object_Access_Type_2b")
   type Object_Access_Type_2c is not null access
     Integer;
   type Object_Access_Type_3a is not null access
     all Integer;
   type Object_Access_Type_3b is not null access
     -- Comment between keywords
     constant Integer;
   type Object_Access_Type_4 is not null
     access all Integer; -- it no longer matters whether this is 'all' or 'constant'

   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(downcase-word 4)(wisi-case-adjust))
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(upcase-word 4)(wisi-case-adjust))
   type Object_Access_Type_5a is not
     --EMACSCMD:(progn (forward-line -1)(forward-word 1)(forward-char 1)(let ((case-fold-search nil))(looking-at "Object_Access_Type_5a")))
     --EMACSRESULT:t
     --EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(let ((case-fold-search nil))(looking-back "null")))
     null access all Integer;
   --EMACSRESULT:t
   type Object_Access_Type_6 is
     not null access all Integer;
   type Object_Access_Type_6b is
     access all Integer;
   type Object_Access_Type_7
     is access all Integer;
   --EMACSCMD:(ada-which-function)
   --EMACSRESULT:(if wisi-parser-shared "Ada_Mode.Nominal" "")

   --  Test case-adjust of keyword in comment
   --EMACSCMD:(progn (forward-line 1)(forward-word 1)(downcase-word -1)(wisi-case-adjust-at-point t)(let ((case-fold-search nil))(looking-back "New")))
   --  New is a keyword in code, but not here

   type
     Object_Access_Type_8 is access all Integer;

   -- Not significantly different than previous, so we only do one.
   type Object_Access_Type_9 is access Integer;

   -- access to procedure
   -- most of the font-lock for this is tested by access to function below
   --EMACSCMD:(test-face-1 "is" "access" font-lock-keyword-face)
   --EMACSCMD:(test-face-1 "is" "protected" font-lock-keyword-face)
   --EMACSCMD:(test-face-1 "is" "procedure" font-lock-keyword-face)
   type Procedure_Access_Type_1 is access protected procedure (A_Param : out Integer);
   --EMACSCMD:(ada-which-function)
   --EMACSRESULT:(if wisi-parser-shared "Ada_Mode.Nominal" "")

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
   --EMACSCMD:(test-face "Function_Access_Type_1a" (cl-ecase ada-face-backend (eglot '(lsp-declaration lsp-type)) (wisi font-lock-type-face)))
   --EMACSCMD:(test-face "is" font-lock-keyword-face)
   --EMACSCMD:(test-face-1 "is" "access" font-lock-keyword-face)
   --EMACSCMD:(test-face "protected" font-lock-keyword-face)
   --EMACSCMD:(test-face-1 "is" "function" font-lock-keyword-face)
   --EMACSCMD:(test-face "(" nil)
   --EMACSCMD:(test-face "A_Param" (cl-ecase ada-face-backend (eglot '(lsp-readonly lsp-declaration lsp-parameter)) (wisi nil)))
   --EMACSCMD:(test-face ":" nil)
   --EMACSCMD:(test-face "in" font-lock-keyword-face)
   --EMACSCMD:(test-face "Float" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-static lsp-type)) (wisi font-lock-type-face)))
   --EMACSCMD:(test-face "return" font-lock-keyword-face)
   --EMACSCMD:(test-face-1 "return" "Standard" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
   --EMACSCMD:(test-face-1 "return" "Float" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-static lsp-type)) (wisi font-lock-type-face)))
   type Function_Access_Type_1a is access protected function (A_Param : in Float) return Standard.Float;
   --EMACSCMD:(test-face-1 "is" "access" font-lock-keyword-face)
   --EMACSCMD:(test-face "Standard" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
   --EMACSCMD:(test-face "Float" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-static lsp-type)) (wisi font-lock-type-face)))
   type Function_Access_Type_1b is access protected function (A_Param : in Float) return access Standard.Float;
   --EMACSCMD:(test-face-1 "is" "constant" font-lock-keyword-face)
   --EMACSCMD:(test-face "Standard" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
   --EMACSCMD:(test-face "Float" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-static lsp-type)) (wisi font-lock-type-face)))
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
   --EMACSRESULT:(if wisi-parser-shared "Ada_Mode.Nominal" "")

   -- a pathological case
   type Function_Access_Type_11 is access
     protected function
       (A_Param : in Float)
       return access function
         (A_Param : in Float)
         return
           Standard.Float;

   --EMACSCMD:(test-face-1 "is" "array" font-lock-keyword-face)
   --EMACSCMD:(test-face "Integer" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-static lsp-type)) (wisi nil)))
   --EMACSCMD:(test-face "range" font-lock-keyword-face)
   --EMACSCMD:(test-face "of" font-lock-keyword-face)
   --EMACSCMD:(test-face "Object_Access_Type_1" (cl-ecase ada-face-backend (eglot 'lsp-type) (wisi nil)))
   type Unconstrained_Array_Type_1 is array (Integer range <>, Standard.Character range <>) of Object_Access_Type_1;
   --EMACSCMD:(test-face "10" (cl-ecase ada-face-backend (eglot nil) (wisi font-lock-constant-face)))
   --EMACSCMD:(test-face "'A'" font-lock-string-face)
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
   --EMACSCMD:(when wisi-parser-shared (forward-line -1)(forward-word 1)(forward-char 1)(call-interactively 'wisi-goto-spec/body)(looking-at "Private_Type_1 is abstract tagged limited null record;"))
   --EMACSRESULT:(not (null wisi-parser-shared))
   --EMACSCMD:(progn (forward-line -3)(test-all-defs "^   type Private_Type_1"))
   --EMACSRESULT_START:(list "ada_mode-nominal.ads" (cl-ecase ada-xref-backend (eglot "   type Private_Type_1 is abstract tagged limited null record;") (gpr_query "Private_Type_1 abstract record type") (gnat "Private_Type_1 spec")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Private_Type_1 full declaration")) (gnat '("ada_mode-nominal.ads" "Private_Type_1 body")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_1 abstract record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_1 full declaration")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_1a abstract record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_1b abstract record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_1c abstract record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_1d abstract record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_2 abstract record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_2 full declaration")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_2a abstract record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_3 abstract record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_3 full declaration")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_4 abstract record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_5 abstract record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Limited_Derived_Type_6 abstract record type")) (gnat nil))
   --EMACSRESULT_FINISH:

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
   --EMACSCMD:(test-face "Private_Type_1" (cl-ecase ada-face-backend (eglot 'lsp-class) (wisi nil)))
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

   function A_Null_Record_Type_1 return Null_Record_Type_1 is (null record);
   --  expression function returning 'null record'

   type Record_Type_1 is
      -- Comment after 'is' before 'record'
      record
         --EMACSCMD:(progn (forward-line 1)(forward-word 2)(insert "   ")(ada-align))
         Component_1   : Integer := 1;
         Component_2   : Integer := 2;
         Component_356 : Float   := 3.0;
         -- longer component name, shorter type name for align test
      end record;

   --EMACSCMD:(test-face "Record_Type_1" (cl-ecase ada-face-backend (eglot 'lsp-struct) (wisi font-lock-type-face)))
   for Record_Type_1 use
      record
         --EMACSCMD:(progn (forward-line 1)(forward-word 2)(insert "   ")(ada-align))
         Component_1   at 0 range  0 .. 31;
         Component_2   at 0 range 32 .. 63;
         Component_356 at 0 range 64 .. 95;
         --  Comment before 'end'.
      end record;
   for Record_Type_1'Size
     use 32 * 3;

   function "+" (Left, Right : in Record_Type_1) return Record_Type_1;
   --EMACSCMD:(progn (forward-line -1)(forward-word 1)(forward-char 2)(xref-backend-identifier-at-point (project-current)))
   --EMACSRESULT: "\"+\""
   --EMACSCMD:(when wisi-parser-shared (forward-line -3)(forward-word 1)(forward-char 2)(call-interactively 'wisi-goto-spec/body)(looking-at "+\" (Left, Right : in Record_Type_1) return Record_Type_1 -- body"))
   --EMACSRESULT:(not (null wisi-parser-shared))

   function "and" (Left, Right : in Record_Type_1) return Boolean;
   --EMACSCMD:(progn (forward-line -1)(forward-word 1)(forward-char 2)(xref-backend-identifier-at-point (project-current)))
   --EMACSRESULT: "\"and\""
   --EMACSCMD:(when wisi-parser-shared (forward-line -3)(forward-word 1)(forward-char 2)(call-interactively 'wisi-goto-spec/body)(looking-at "and\" (Left, Right : in Record_Type_1) return Boolean -- body"))
   --EMACSRESULT:(not (null wisi-parser-shared))

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

   --EMACSCMD:(when wisi-parser-shared (wisi-goto-statement-end)(looking-back "end record"))
   --EMACSRESULT:(not (null wisi-parser-shared))
   --EMACSCMD:(test-face-1 "access" "Standard" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
   --EMACSCMD:(test-face-1 "access" "Integer" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-static lsp-type)) (wisi font-lock-type-face)))
   type Record_Type_3
     (Discriminant_1 : access Standard.Integer;
      --EMACSCMD:(test-face "Standard" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-namespace)) (wisi font-lock-function-name-face)))
      --EMACSCMD:(test-face "Integer" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-static lsp-type)) (wisi font-lock-type-face)))
      Discriminant_2 : Standard.Integer;
      --EMACSCMD:(test-face "Ada_Mode" (cl-ecase ada-face-backend (eglot 'lsp-namespace) (wisi font-lock-function-name-face)))
      --EMACSCMD:(test-face "Nominal" (cl-ecase ada-face-backend (eglot 'lsp-namespace) (wisi font-lock-function-name-face)))
      --EMACSCMD:(test-face "Object_Access_Type_0a" (cl-ecase ada-face-backend (eglot 'lsp-type) (wisi font-lock-type-face)))
      Discriminant_3 : not null Ada_Mode.Nominal.Object_Access_Type_0a)
      is tagged record
         --EMACSCMD:(when wisi-parser-shared (wisi-goto-statement-end)(looking-at "; -- end 2"))
         --EMACSRESULT:(not (null wisi-parser-shared))
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
   --EMACSCMD:(test-face "0.10" (cl-ecase ada-face-backend (eglot nil) (wisi font-lock-constant-face)))
   --EMACSCMD:(test-face "digits" font-lock-keyword-face)
   --EMACSCMD:(test-face-1 "digits" "10" (cl-ecase ada-face-backend (eglot nil) (wisi font-lock-constant-face)))
   type Decimal_Fixed_Point_1 is delta 0.10 digits 10;
   type Decimal_Fixed_Point_2 is delta 0.10 digits
     10;
   type Decimal_Fixed_Point_3 is delta 0.10
     digits 10;
   --EMACSCMD:(progn (beginning-of-line)(forward-line -1)(ada-which-function))
   --EMACSRESULT:(if wisi-parser-shared "Ada_Mode.Nominal" "")

   type Decimal_Fixed_Point_4 is delta
     0.10 digits 10;
   type Decimal_Fixed_Point_5 is
     delta 0.10 digits 10;
   type Decimal_Fixed_Point_6
     is delta 0.10 digits 10;
   type
     Decimal_Fixed_Point_7 is delta 0.10 digits 10;

   -- These are not signicantly different, so only one.
   --EMACSCMD:(test-face "10" (cl-ecase ada-face-backend (eglot nil) (wisi font-lock-constant-face)))
   --EMACSCMD:(test-face "1.00e-6" (cl-ecase ada-face-backend (eglot nil) (wisi font-lock-constant-face))) ;; does not include '-'
   --EMACSCMD:(test-face "16#AF.42" (cl-ecase ada-face-backend (eglot nil) (wisi font-lock-constant-face)))
   type Floating_Point is digits 10 range -1.00e-6 .. 16#AF.42#;

   --EMACSCMD:(test-face-1 "is" "mod" font-lock-keyword-face)
   type Modular_Type is mod 10;
   --EMACSCMD:(test-face "10.0" (cl-ecase ada-face-backend (eglot nil) (wisi font-lock-constant-face)))
   type Ordinary_Fixed_Point is delta 0.10 range 10.0 .. 11.0;
   type Signed_Integer_Type is range 10 .. 21;

   --EMACSCMD:(test-face "Subtype_1" (cl-ecase ada-face-backend (eglot '(lsp-static lsp-declaration lsp-type)) (wisi font-lock-type-face)))
   --EMACSCMD:(test-face "Signed_Integer_Type" (cl-ecase ada-face-backend (eglot '(lsp-static lsp-type)) (wisi nil)))
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

   --EMACSCMD:(when wisi-parser-shared (end-of-line 5)(backward-word 5)(call-interactively 'wisi-goto-spec/body)(backward-word 1)(looking-at "body Protected_1 is"))
   --EMACSRESULT:(not (null wisi-parser-shared))
   --EMACSCMD:(when wisi-parser-shared (forward-line 2)(back-to-indentation) (forward-sexp)(looking-at "is -- Protected_1"))
   --EMACSRESULT:(not (null wisi-parser-shared))
   protected type Protected_1 is -- Protected_1

      --EMACSCMD:(progn (forward-line -2)(test-all-defs "Protected_1"))
      --EMACSRESULT_START:(cl-ecase ada-xref-backend (eglot '("ada_mode-nominal.adb" "   protected body Protected_1 is -- target 2")) (gpr_query '("ada_mode-nominal.ads" "Protected_1 protected type")) (gnat '("ada_mode-nominal.ads" "Protected_1 spec")))
      --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) ((gpr_query gnat) '("ada_mode-nominal.adb" "Protected_1 body")))
      --EMACSRESULT_FINISH:

      --EMACSCMD:(when wisi-parser-shared (end-of-line -5)(forward-word -3) (backward-sexp)(looking-at "protected type Protected_1"))
      --EMACSRESULT:(not (null wisi-parser-shared))
      --EMACSCMD:(when wisi-parser-shared (end-of-line -7)(forward-word -3) (forward-sexp)(looking-at "private -- Protected_1"))
      --EMACSRESULT:(not (null wisi-parser-shared))

      --EMACSCMD:(ada-which-function)
      --EMACSRESULT:(if wisi-parser-shared "Protected_1" "")

      -- only two examples, to get 'protected' and 'is-entry_body' into grammar

      --EMACSCMD:(when wisi-parser-shared (wisi-goto-statement-end)(looking-at "; -- end 3"))
      --EMACSRESULT:(not (null wisi-parser-shared))
      function F1 return Integer; -- end 3

      --EMACSCMD:(test-face "Discrete_Type_1" (cl-ecase ada-face-backend (eglot '(lsp-static lsp-enum)) (wisi font-lock-type-face)))
      function F2 (Param_1 : Discrete_Type_1; B : Float) return Float
      with Pre => B > 0.0;
      --EMACSCMD:(when wisi-parser-shared (end-of-line 0)(ada-goto-declarative-region-start)(looking-at " -- Protected_1"))
      --EMACSRESULT:(not (null wisi-parser-shared))

      --EMACSCMD:(progn (forward-line 3)(ada-which-function t))
      --EMACSRESULT:(if wisi-parser-shared "E1" "")
      entry E1
        (X : Integer);
      entry E2 (X : Integer);
      entry E3 (X : Integer);
      procedure P1;
      procedure P2
        (A : Float;
         B : Float);

      --EMACSCMD:(when wisi-parser-shared (wisi-goto-statement-end)(looking-back "end Protected_1"))
      --EMACSRESULT:(not (null wisi-parser-shared))
      -- This is a comment just before 'private'; broken versions of the
      -- indentation engine aligned this with 'private'.
   private -- Protected_1

      --EMACSCMD:(when wisi-parser-shared (end-of-line -1)(forward-word -3) (backward-sexp)(looking-at "is -- Protected_1"))
      --EMACSRESULT:(not (null wisi-parser-shared))
      --EMACSCMD:(when wisi-parser-shared (end-of-line -3)(forward-word -3) (forward-sexp)(looking-at "; -- Protected_1"))
      --EMACSRESULT:(not (null wisi-parser-shared))

      -- More than three objects, to be sure we are handling
      -- indefinite lists of objects properly
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

   --EMACSCMD:(progn (forward-line 2) (ada-find-other-file) (looking-at (cl-ecase ada-xref-backend (eglot "-- A comment before the first code") ((gpr_query gnat) "protected body Protected_Buffer"))))
   protected Protected_Buffer is
      --EMACSRESULT:t
      -- a single_protected_type

      --EMACSCMD:(ada-which-function)
      --EMACSRESULT:(if wisi-parser-shared "Protected_Buffer" "")

      --EMACSCMD:(test-face "Character" (cl-ecase ada-face-backend (eglot '(lsp-defaultLibrary lsp-static lsp-enum)) (wisi font-lock-type-face)))
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

   --EMACSCMD:(progn (forward-line 4)(test-all-defs "^   task type Task_Type_1"))
   --EMACSRESULT_START:(cl-ecase ada-xref-backend (eglot '("ada_mode-nominal.adb" "   task body Task_Type_1 is")) (gpr_query '("ada_mode-nominal.ads" "Task_Type_1 task type")) (gnat '("ada_mode-nominal.ads" "Task_Type_1 spec")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) ((gpr_query gnat) '("ada_mode-nominal.adb" "Task_Type_1 body")))
   --EMACSRESULT_FINISH:
   task type Task_Type_1 (Name : access String)
   with
     Storage_Size => 512 + 256
   is
      --EMACSCMD:(ada-which-function)
      --EMACSRESULT:(if wisi-parser-shared "Task_Type_1" "")

      --EMACSCMD:(test-face "Start" (cl-ecase ada-face-backend (eglot '(lsp-declaration lsp-function)) (wisi font-lock-function-name-face)))
      --EMACSCMD:(test-face "Discrete_Type_1" (cl-ecase ada-face-backend (eglot '(lsp-static lsp-enum)) (wisi nil)))
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

   --EMACSCMD:(test-all-defs "type Parent_Type_1")
   --EMACSRESULT_START:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Parent_Type_1 record type")) (gnat '("ada_mode-nominal.ads" "Parent_Type_1 spec")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal-child.ads" "Child_Type_1 record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal-child.ads" "Child_Type_2 record type")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal-child.ads" "Child_Type_2 full declaration")) (gnat nil))
   --EMACSRESULT_FINISH:
   --EMACSCMD:(test-face "Parent_Type_1" (cl-ecase ada-face-backend (eglot '(lsp-declaration lsp-class)) (wisi font-lock-type-face)))
   type Parent_Type_1 is tagged record
      Parent_Element_1 : Integer;
      Parent_Element_2 : Float;
      Parent_Element_3 : Boolean;
   end record;

   -- test that comment prefix is properly fontified, and that fill
   -- paragraph doesn't cause problems
   --
   --EMACSCMD:(progn (end-of-line 4)(delete-forward-char 6)(ada-fill-comment-paragraph)(font-lock-ensure)(forward-char 4)(face-at-point))
   --EMACSRESULT: 'font-lock-comment-delimiter-face

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
   --EMACSRESULT:(if wisi-parser-shared "Procedure_1a" "")
   --EMACSCMD:(ada-which-function)
   --EMACSRESULT:(if wisi-parser-shared "Ada_Mode.Nominal" "")

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
   --EMACSRESULT:(if wisi-parser-shared "Ada_Mode.Nominal" "")
   --EMACSCMD:(progn (forward-line -6)(ada-which-function))
   --EMACSRESULT:(if wisi-parser-shared "Procedure_1d" "")
   --EMACSCMD:(progn (forward-line -7)(ada-which-function))
   --EMACSRESULT:(if wisi-parser-shared "Procedure_1d" "")
   --EMACSCMD:(progn (forward-line -8)(ada-which-function))
   --EMACSRESULT:(if wisi-parser-shared "Procedure_1d" "")
   --EMACSCMD:(progn (forward-line -9)(ada-which-function))
   --EMACSRESULT:(if wisi-parser-shared "Procedure_1d" "")

   procedure Procedure_1e (Item   : in out Parent_Type_1;
                           Item_1 : in Character;
                           Item_2 : out Character)
     is null;

   not overriding
   procedure Procedure_1f (Item : in out Parent_Type_1);

   function Function_2a (Param : in Parent_Type_1) return Float;

   --EMACSCMD:(test-all-refs "^   function Function_2b")
   --EMACSRESULT_START:(cl-ecase ada-xref-backend (eglot '("ada_mode-nominal.ads" "   function Function_2b (Param : in Parent_Type_1) return")) (gpr_query '("ada_mode-nominal-child.adb" "Function_2b Parent_Type_1; dispatching call"))(gnat '("ada_mode-nominal.ads" "Function_2b spec")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot '("ada_mode-nominal.adb" "   function Function_2b (Param : in Parent_Type_1) return Float")) (gpr_query '("ada_mode-nominal.adb" "Function_2b Parent_Type_1; body"))(gnat '("ada_mode-nominal.adb" "Function_2b body")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot '("ada_mode-nominal.adb" "      return Function_2b (Parent_Type_1'(1, 1.0, False));")) (gpr_query '("ada_mode-nominal.adb" "Function_2b Parent_Type_1; static call"))(gnat '("ada_mode-nominal-child.adb" "Function_2b")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot '("ada_mode-nominal.adb" "      return Function_2b (Item);")) (gpr_query '("ada_mode-nominal.adb" "Function_2b Parent_Type_1; dispatching call")) (gnat '("ada_mode-nominal.adb" "Function_2b")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot '("ada_mode-nominal-child.ads" "   function Function_2b (Param : in Child_Type_1) return")) (gpr_query '("ada_mode-nominal.ads" "Function_2b Parent_Type_1; declaration"))(gnat '("ada_mode-nominal.adb" "Function_2b")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot '("ada_mode-nominal-child.adb" "      return Function_2b (Item);")) (gpr_query '("ada_mode-nominal-child.adb" "Function_2b Child_Type_1; body")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal-child.adb" "Function_2b Child_Type_1; label on end line")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal-child.adb" "Function_2b Child_Type_1; static call")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal-child.ads" "Function_2b Child_Type_1; declaration")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal-child.adb" "Function_2b Child_Type_2; dispatching call")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.adb" "Function_2b Child_Type_2; body")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.adb" "Function_2b Child_Type_2; static call")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.adb" "Function_2b Child_Type_2; dispatching call")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Function_2b Child_Type_2; declaration")) (gnat nil))
   --EMACSRESULT_FINISH:
   function Function_2b (Param : in Parent_Type_1) return
     Float;
   --EMACSCMD:(progn (forward-line -2)(test-all-defs "function Function_2b"))
   --EMACSRESULT_START:(cl-ecase ada-xref-backend (eglot '("ada_mode-nominal.adb" "   function Function_2b (Param : in Parent_Type_1) return Float")) (gpr_query '("ada_mode-nominal.ads" "Function_2b Ada_Mode.Nominal.Parent_Type_1;(Param) function")) (gnat '("ada_mode-nominal.ads" "Function_2b spec")))
   --EMACSRESULT_ADD: (cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.adb" "Function_2b Ada_Mode.Nominal.Parent_Type_1;(Param) body")) (gnat '("ada_mode-nominal.adb" "Function_2b body")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal-child.ads" "Function_2b Ada_Mode.Nominal.Child.Child_Type_1;(Param) function")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal-child.adb" "Function_2b Ada_Mode.Nominal.Child.Child_Type_1;(Param) body")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.ads" "Function_2b Ada_Mode.Nominal.Child.Child_Type_2;(Param) function")) (gnat nil))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query '("ada_mode-nominal.adb" "Function_2b Ada_Mode.Nominal.Child.Child_Type_2;(Param) body")) (gnat nil))
   --EMACSRESULT_FINISH:

   function Function_2c (Param : in Parent_Type_1)
                        return Float;
   function Function_2d
     (Param : in Parent_Type_1) return Float;
   not overriding function
     Function_2e (Param : in Parent_Type_1) return Float;

   --EMACSCMD:(when wisi-parser-shared (wisi-goto-statement-end)(looking-at "; -- end 5"))
   --EMACSRESULT:(not (null wisi-parser-shared))
   not overriding
   function Function_2f
     (Param : in Parent_Type_1)
     return Float; -- end 5

   --EMACSCMD:(progn (end-of-line 3)(ada-which-function))
   --EMACSRESULT:(if wisi-parser-shared "Function_2g" "")
   function Function_2g
     (Param : in Private_Type_1)
     return Float
     is abstract;
   --  comment after 'is abstract', aligned with 'function'

   function Function_2h (Param : in Parent_Type_1) return Float is (1.0); -- expression function
   function Function_2i (Param : in Parent_Type_1) return Parent_Type_1 is
     (Parent_Element_1 => Param.Parent_Element_1 * 2,
      Parent_Element_2 => 3.0,
      Parent_Element_3 => False); -- expression function returning aggregate

   Default_Parent : constant Parent_Type_1 :=
     (Parent_Element_1 => 1,
      Parent_Element_2 => 2.0,
      Parent_Element_3 => False);
   --EMACSCMD:(progn (forward-line -2)(ada-which-function))
   --EMACSRESULT:(if wisi-parser-shared "Ada_Mode.Nominal" "")

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

   --EMACSCMD:(when wisi-parser-shared (wisi-goto-statement-end)(looking-back "end Ada_Mode.Nominal"))
   --EMACSRESULT:(not (null wisi-parser-shared))
   --EMACSCMD:(when wisi-parser-shared (forward-line 2) (forward-sexp)(looking-at "; -- Ada_Mode.Nominal"))
   --EMACSRESULT:(not (null wisi-parser-shared))
private -- Ada_Mode.Nominal

   --EMACSCMD:(when wisi-parser-shared (forward-line -2) (backward-sexp)(looking-at "is -- target 0"))
   --EMACSRESULT:(not (null wisi-parser-shared))

   type Private_Type_1 is abstract tagged limited null record;
   --EMACSCMD:(when wisi-parser-shared (forward-line -1)(forward-word 2)(call-interactively 'wisi-goto-spec/body)(looking-at "Private_Type_1 is abstract tagged limited private;"))
   --EMACSRESULT: (not (null wisi-parser-shared))
   type Private_Type_2 is abstract tagged limited
      record
         Component_1 : Integer;
         Component_2 : Integer;
         Component_3 : Integer;
      end record; -- Ada mode 4.01 aligned this with "type"; this is better

   --EMACSCMD:(when wisi-parser-shared (ada-goto-declarative-region-start)(looking-at " -- target 0"))
   --EMACSRESULT:(not (null wisi-parser-shared))
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
   --EMACSRESULT:(if wisi-parser-shared "Limited_Derived_Type_6" "")

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

--EMACSCMD:(when wisi-parser-shared (forward-line -2) (forward-sexp)(backward-sexp)(looking-at "private -- Ada_Mode.Nominal"))
--EMACSRESULT:(not (null wisi-parser-shared))
-- Local Variables:
-- fill-column: 70
-- wisi-process-time-out: 6.0
-- ada-which-func-parse-size:100000
-- ada-eglot-gpr-file: nil
-- End:
