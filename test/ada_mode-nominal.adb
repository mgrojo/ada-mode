-- A comment before the first code
--EMACSCMD:(font-lock-fontify-buffer)

--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")

with Ada.Strings; -- test two context clauses
with Ada.Strings.Unbounded;
--EMACSCMD:(test-face "use" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" font-lock-constant-face)
use Ada.Strings.Unbounded;
package body Ada_Mode.Nominal is -- target 0

   --EMACSCMD:(test-face "Ada" font-lock-constant-face)
   use Ada.Strings;

   --EMACSCMD:(progn (forward-line 1) (ada-next-statement-keyword)(looking-at "is -- target 1"))
   function Function_Access_1
     (A_Param : in Float)
     return
     Standard.Float
   is -- target 1
      --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 1"))
      --EMACSRESULT:t
      use type Standard.Float;
      --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 1"))
      --EMACSRESULT:t
   begin
      --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 1"))
      --EMACSRESULT:t
      return 0.0;
   end;

   function Function_Access_11
     (A_Param : in Float)
     --  FIXME: EMACSCMD:(test-face "function" font-lock-keyword-face)
     return access function
       (A_Param : in Float)
       return
     Standard.Float -- Ada mode 4.01, GPS
   is begin
      -- An early implementation of ada-smie-which-function was confused by this declaration.
      --EMACSCMD:(progn (beginning-of-line)(forward-line -7)(ada-which-function))
      --EMACSRESULT:"Function_Access_11"
      --EMACSCMD:(progn (beginning-of-line)(forward-line -8)(ada-which-function))
      --EMACSRESULT:"Function_Access_11"
      --EMACSCMD:(progn (beginning-of-line)(forward-line -9)(ada-which-function))
      --EMACSRESULT:"Function_Access_11"
      --EMACSCMD:(progn (beginning-of-line)(forward-line -10)(ada-which-function))
      --EMACSRESULT:"Function_Access_11"
      --EMACSCMD:(progn (beginning-of-line)(forward-line -11)(ada-which-function))
      --EMACSRESULT:"Function_Access_11"
      --EMACSCMD:(progn (beginning-of-line)(forward-line -12)(ada-which-function))
      --EMACSRESULT:"Function_Access_11"

      --EMACSCMD:(test-face "Function_Access_1" 'default)
      return Function_Access_1'Access;
   end Function_Access_11;

   --EMACSCMD:(progn (forward-line 3)(ada-find-other-file nil)(looking-at "protected type Protected_1"))
   protected body Protected_1 is -- target 2

      --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 2"))
      --EMACSRESULT:t

      --EMACSCMD:(ada-which-function)
      --EMACSRESULT:"Protected_1"

      --EMACSCMD:(test-face "Integer" font-lock-type-face)
      function F1 return Integer is
         -- some people like 'is' on the line with 'function' here

         function Local_Function return Integer
         is -- target 3
            --EMACSCMD:(test-face "exception" font-lock-keyword-face)
            Bad_Thing : exception;
            --EMACSCMD:(test-face "Boolean" font-lock-type-face)
            Dummy : Boolean;
            pragma Unreferenced (Dummy); -- test ada-indent-statement-or-declaration handling of this in refine-begin
         begin
            --EMACSCMD:(progn (forward-line 2)(forward-comment 1)(ada-next-statement-keyword)(looking-at "then"))
            --EMACSRESULT: t
            if True then
               --EMACSCMD:(progn (end-of-line 0)(backward-word 1)(ada-next-statement-keyword)(looking-at "elsif"))
               --EMACSRESULT: t

               --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 3"))
               --EMACSRESULT:t
               begin -- target 4
                     --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at "begin -- target 4"))
                     --EMACSRESULT:t

                  --EMACSCMD:(test-face "Integer" 'default)
                  -- "Integer" is in fact a type, but it would require
                  -- name resolution to figure that out.
                  return Integer (Function_1a);
                  --EMACSCMD:(ada-which-function)
                  --EMACSRESULT:"Local_Function"
               exception
                  --EMACSCMD:(test-face "Constraint_Error" 'default)
                  when E : Constraint_Error =>
                     --EMACSCMD:(test-face "raise" font-lock-keyword-face)
                     --EMACSCMD:(test-face "Constraint_Error" 'default)
                     --EMACSCMD:(test-face "with" font-lock-keyword-face)
                     raise Constraint_Error with
                       "help!";
                  when
                    Bad_Thing -- ada-mode 4.01 indentation
                    =>        -- ""
                     raise Constraint_Error
                       with "help!";
                  when
                    E : others => raise
                      Constraint_Error with "help!";
               end;
            elsif False
            then
               return 1;   -- a comment
                           -- another comment, aligned with previous
            else
               return 0;
            end if;
         end Local_Function;
      begin
         --EMACSCMD:(progn (end-of-line 0)(backward-word 1)(ada-next-statement-keyword)(looking-at "end F1"))
         --EMACSRESULT: t
         return B : Integer := Local_Function;
         -- non-do extended return
      end F1;

      --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 2"))
      --EMACSRESULT:t

      function F2 (Param_1 : Discrete_Type_1; B : Float) return Float
      is
         Local : Object_Access_Type_0a := new Float'(9.0);
      begin
         return D : Float
         do
            -- extended return with do
            case Param_1 is
               -- comment after "is", before "when"
               when A | Nominal.B =>
                  null;
               when C =>
                  --EMACSCMD:(progn (forward-line 2)(forward-word 1)(forward-char 1)(insert "   ")(ada-align))
                  -- result is tested in diff
                  D := B;
                  D := Local.all + B * B;
                  --EMACSCMD:(test-face "goto" font-lock-keyword-face)
                  --EMACSCMD:(test-face "Label_1" font-lock-constant-face)
                  goto Label_1;
               --EMACSCMD:(test-face "Label_1" font-lock-constant-face)
               <<Label_1>>
                  D := D - Float (F1);
            end case;
         --  FIXME: test keyword motion on case
         end return;
      end; -- no F2 on purpose

      entry E1 (X : Integer) when Local_1 = 0 is
         Tmp : Integer := 0;
         Local_4 : Discrete_Type_1 := A;
      begin
         Local_1 :=
           X + Tmp;

         -- A comment after a hanging line

         case Local_4 is
            when
              A | -- continuation line; ada-indent-broken = 2
              B |
              C
              => -- Ada mode 4.01 indentation
               for I in 1 .. 10 loop
                  Local_1 := Local_1 + Local_1;

                  case Local_1 is
                     when 1 =>
                        exit;
                     when 2 => -- at one point, this was mis-refined as "when-exit"
                        Local_4 := B;
                     when others =>
                        null;
                  end case;
               end loop;

            Loop_1 :
               loop
                  Local_1 := Local_1 + Local_1;
                  exit Loop_1 when Local_1 > 0;
               end loop Loop_1;

               loop
                  Local_1 := Local_1 + Local_1;
                  exit when Local_1 > 0;
               end loop;

            Loop_4 : while not (Local_1 > 0) loop
               -- Sort of wrong, but this is what Ada-mode 4.01 does.
               -- Don't put loop label on the same line as loop if
               -- ada-indent-label is not 0.
               Local_1 := Local_1 + 2;
            end loop Loop_4;

         Loop_5 : -- wrong because of previous line
            while not (Local_1 > 0) loop
               Local_1 := Local_1 + 2;
            end loop Loop_5;
         end case;

         -- A comment before 'end'
      end E1;

      entry E2
        (X : Integer)
        -- an expression with 'not' to see if we need that in the
        -- grammar (conflicts with 'not null')
        when Local_1 = 0 and not -- Ada mode 4.01 screws this up.
        (Local_2 = 1)
      is
         Tmp : Integer := 0;
      begin
         Local_2 := Tmp;
      end E2;

      procedure P1 is
      begin
         null;
      end P1;

      procedure P2 (A : Float; B : Float)
      is begin
         null;
      end; -- no P2
   end Protected_1;

   --EMACSCMD:(progn (forward-line 2)(ada-find-other-file nil)(looking-at "protected Protected_Buffer"))
   protected body Protected_Buffer is
      --EMACSCMD:(ada-which-function)
      --EMACSRESULT:"Protected_Buffer"

      entry Write(C : in Character)
        when Count < Pool'Length is
      begin
         Pool(In_Index) := C;
         In_Index := (In_Index mod Pool'Length) + 1;
         Count    := Count + 1;
      end Write;

      entry Read (C : out Character)
        when Count > 0 is
      begin
         C := Pool(Out_Index);
         Out_Index := (Out_Index mod Pool'Length) + 1;
         Count     := Count - 1;
      end Read;
   end Protected_Buffer;

   --------------------------------------------------------
   --  6804-008: problem for indentation after a task declaration
   --  The problem was caused by the task declaration with no
   --  block attached
   --------------------------------------------------------
   task Executive;
   task body Executive is -- target 5
   begin
      --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 5"))
      --EMACSRESULT:t

      null;
   end Executive;

   -- a more typical task
   task body Task_Type_1 is
      Local_1 : Integer;
      Started : Boolean := False;
   begin
      select
         accept Start (A) (Param_1 : in Integer);
         Started := True;
      or
         when Started => -- Ada mode 4.01 ada-when-indent, GPS ada-indent
            accept Middle_1 (Param_1 : in Integer) do
               Local_1 := 0;
            end Middle_1;
      or
         when Started =>
            accept Middle_2
              (Param_1 : in Integer);
            Local_1 := 0;

      or when Started
        =>
         accept Finish; -- Ada mode 4.01
         Local_1 := 5;
      or
         delay 1.0;
         null;
      end select;

      select -- need a separate select to test "else", "terminate" etc
         accept Start (A) (Param_1 : in Integer);
         Local_1 := 0;
      else
         Local_1 := 2;
      end select;

      select
         accept Start (A)
           (Param_1 : in Integer)
         do
            null;
         end Start;
      or
         terminate;
      end select;

      select
         delay 1.0;
      then
         -- The comment after 'null' below has no space between ';'
         -- and '--'; that confused smie-default-forward-token until
         -- we fixed ada-syntax-propertize to set comment-start
         -- syntax.

        abort -- ada-mode 4.01 broken indent
         null;-- ada-mode 4.01 gets this wrong; it uses another broken indent.

      end select;

   end Task_Type_1;

   ----------
   -- subprograms

   not overriding procedure Procedure_1a (Item  : in out Parent_Type_1)
   is begin
      null;
   end Procedure_1a;

   not overriding
   procedure Procedure_1c (Item  : in out Parent_Type_1) is begin null; end;

   not
   overriding
   procedure Procedure_1f (Item : in out Parent_Type_1)
   is -- target 6
   begin
      --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 6"))
      --EMACSRESULT:t
      null;
   end Procedure_1f;

   procedure Procedure_2a is -- target 7
   begin null; end;
   --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 0"))
   --EMACSRESULT:t

   -- Functions can't be 'is null', so we test some indentation issues
   function Function_1a return Float
   is begin
      Procedure_2a;
      Procedure_2a;
      return 1.0;
   end Function_1a;

   function Function_1b return Float
   is
      Local_1 : constant := 3.0;
   begin
      declare -- target 8

         -- no label, zero statements between begin, declare
      begin
         return Local_1;
      exception
         --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 8"))
         --EMACSRESULT:t

         when others =>
            return 0.0;
      end;
   end
     Function_1b;

   function Function_1c return Float
   is
      Local_1 : constant Float := 2.0;
      Local_2 : constant Float
        := Local_1;
      Local_3 : constant Float :=
        Local_2;
      -- Comment after indented line
      Item : Parent_Type_1;
   begin
      Procedure_1a
        (Item);
      declare -- no label, one statement between begin, declare
      begin
         return
           Local_1 +
           Local_2 +
           Local_3;
      end;
   end;

   function Function_1d return Float
   is begin
      Procedure_2a
        ; -- hanging statement, to test that declare does not indent on it
      Procedure_2a;

      declare -- no label, two statements between begin, declare
      begin
         return 1.0;
      end;
   end;

   function Function_2a (Param : in Parent_Type_1) return Float
   is begin
   Block_1:
      declare -- label, no statements between begin, label
         Local_1 : Float := 1.0e-36;
         Local_2 : Integer := 2;
         Local_3 : Character := '\'; -- must override default syntax for \
      begin
         --EMACSCMD:(ada-which-function)
         --EMACSRESULT:"Function_2a"
         Local_1 := 2.0;
         Local_2 := 3;
         Local_3 := '"'; -- quoted quote handled ok
         return 1.0;
      end Block_1;
   end;

   function Function_2b (Param : in Parent_Type_1) return Float
   is
   begin
      Procedure_2a;
   Block_1:-- label, one statements between begin, label
      declare
         --EMACSCMD:(progn (forward-line 2)(forward-word 4)(forward-char 1)(insert "   ")(ada-align))
         Local_1 : Float     := 1.0e-36; -- comments after declarations
         Local_2 : Integer   := 2;       -- are aligned.
         Local_3 : Character := ''';     -- quoted quote handled ok
         Local_4 : String    :=
           "abc:" & ':' & "def";         -- ignores these ':'
      begin
         --EMACSCMD:(progn (forward-line 2)(forward-word 3)(forward-char 1)(insert "   ")(ada-align))
         Local_1 := 2.0; -- comments after code
         Local_2 := 3;   -- are
         Local_3 := '4'; -- also aligned

         return 1.0;
      end Block_1;
   end;

   function Function_2c (Param : in Parent_Type_1) return Float
   is
      type Array_Type_1 is array (1 .. 3) of Float;
      type Array_Type_2 is array (1 .. 3) of Array_Type_1;
      Local_A : Array_Type_2 := (others => (others => 0.0));
   begin
      Procedure_2a
        ;

      -- second begin block
      begin
         Local_A (1)(2) := 1.0;
      end;

   Block_1 :
      declare -- label, two statements between begin, label
         Local_1 : constant Float := Local_A (1)(2); -- test that refine-begin can skip parens
      begin
         return Local_1;
      end Block_1;
   end;

   not
   overriding
   function Function_2d (Param : in Parent_Type_1) return Float
   is
   begin
      begin -- no declare, no statements
         return
           1.0;
      end;
   end;

   not overriding function Function_2e
     (Param : in Parent_Type_1)
     return Float
   is begin
      Procedure_2a;
      begin -- no declare, one statement
         return 1.0;
      end;
   end;

   not overriding
   function Function_2f (Param : in Parent_Type_1)
                        return Float is
   begin
      Procedure_2a
        ;
      Procedure_2a;
      begin -- no declare, two statements
         return 1.0;
      end;
   end;

   package body Separate_Package_1 is separate;

   type Incomplete_Type_1 (Discriminant_1 : Integer) is tagged null record;

   type Incomplete_Type_2 (Discriminant_1 : Integer) is tagged null
      record;

   type Incomplete_Type_3 (Discriminant_1 : Integer) is tagged
     null record;

   type Incomplete_Type_4 (Discriminant_1 : Integer) is
     tagged null record;
   -- rest of newline placement covered in spec

   type Incomplete_Type_5 (Discriminant_1 : access Integer) is tagged record
      -- "record" on same line as "type"; components indented 3 relative to "type"
      Component_1 : Integer;
      Component_2 :
        Integer;
      Component_3
        : Integer; -- don't care
   end record;

begin
   null;
end Ada_Mode.Nominal;
