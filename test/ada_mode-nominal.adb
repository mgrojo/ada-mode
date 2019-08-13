-- A comment before the first code

--EMACSCMD:(font-lock-ensure)

--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")

--EMACSCMD:(progn (forward-line 3)(forward-word 1) (ada-goto-declarative-region-start)(= (point) (point-min)))
--EMACSRESULT:t

--EMACSCMD:(progn (goto-char (car (ada-fix-context-clause))) (looking-at "with Ada.Strings"))
--EMACSRESULT:t
pragma License (Unrestricted); -- for testing ada-wisi-context-clause

with Ada.Strings; -- test two context clauses
with Ada.Strings.Unbounded;
--EMACSCMD:(test-face "use" font-lock-keyword-face)
--EMACSCMD:(test-face "Ada" font-lock-function-name-face)
use Ada.Strings.Unbounded;
--EMACSCMD:(progn (beginning-of-line 3) (forward-sexp)(looking-at "is -- target 0"))
--EMACSRESULT:t
package body Ada_Mode.Nominal
with
  Spark_Mode => On
is -- target 0

   --EMACSCMD:(test-face "Ada" font-lock-function-name-face)
   use Ada.Strings;

   --EMACSCMD:(test-face "access" font-lock-keyword-face)
   --EMACSCMD:(test-face "procedure" font-lock-keyword-face)
   Progress_Reporter : access procedure (Current, Total : Integer) := null;

   Record_1_Object_1 : constant Record_Type_1 :=
     (Component_1 |
        Component_2 => 1 +
          2 +
          3,
      Component_356 =>
        1.0 +
          2.0);

   Record_1_Object_2 : constant Record_Type_1 :=
     (Component_1 |
        Component_2 =>
          1 +
            2 +
            3,
      Component_356 =>
        1.0);

   --EMACSCMD:(progn (forward-line 4) (back-to-indentation) (forward-sexp)(looking-at "is -- target 1"))
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line 3)(forward-word 1) (ada-goto-declarative-region-start)(looking-at " -- target 1"))
   --EMACSRESULT:t
   function Function_Access_1
     (A_Param : in Float)
     return access
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
      if A_Param > 0.0 then
         -- EMACSCMD:(test-face "new" font-lock-keyword-face)
         return new Float'(0.0);
      else
         -- EMACSCMD:(test-face "null" font-lock-keyword-face)
         return null;
      end if;
   end;

   function Function_Access_11
     (A_Param : in Float)
     --EMACSCMD:(test-face "function" font-lock-keyword-face)
     return access function
       (A_Param : in Float)
       return
         access Standard.Float
   is begin
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

      --EMACSCMD:(test-face "Function_Access_1" '(nil default))
      return Function_Access_1'Access;
   end
     Function_Access_11;

   --EMACSCMD:(progn (forward-line 3)(ada-find-other-file)(looking-at "protected type Protected_1"))
   protected body Protected_1 is -- target 2

      --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 2"))
      --EMACSRESULT:t

      --EMACSCMD:(ada-which-function)
      --EMACSRESULT:"Protected_1"

      --  "Integer" is not font-lock-type-face; not parsing for just font-lock
      function F1 return Integer is
         -- some people like 'is' on the line with 'function' here

         function Local_Function return Integer
         is -- target 3
            --EMACSCMD:(test-face "exception" font-lock-keyword-face)
            Bad_Thing : exception;
            --EMACSCMD:(test-face "Boolean" font-lock-type-face)
            Dummy : Boolean;
            Dummy_2 : Boolean;
         begin
            --EMACSCMD:(progn (forward-line 4)(forward-comment 1)(backward-sexp)(looking-at "begin"))
            --EMACSRESULT: t
            --EMACSCMD:(progn (forward-line 2)(forward-comment 1)(forward-sexp)(looking-at "then"))
            --EMACSRESULT: t
            if
              True
              --  Comment before 'then' (does not pass gnat style check)
            then -- 1
                 --EMACSCMD:(progn (end-of-line 0)(backward-word 2)(backward-sexp)(looking-at "if"))
                 --EMACSRESULT: t
                 --EMACSCMD:(progn (end-of-line -2)(backward-word 2)(forward-sexp)(looking-at "elsif Dummy or -- 2"))
                 --EMACSRESULT: t

               --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 3"))
               --EMACSRESULT:t
               begin -- 2
                     --EMASCMD:(progn (forward-line -1)(back-to-indentation)(forward-sexp)(looking-at "when E :"))
                     --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 3"))
                     --EMACSRESULT:t

                  --EMACSCMD:(test-face "Integer" '(nil default))
                  -- "Integer" is in fact a type, but it would require
                  -- name resolution to figure that out.
                  return Integer (Function_1a);
                  --EMACSCMD:(ada-which-function)
                  --EMACSRESULT:"Local_Function"
               exception

                  --EMACSCMD:(test-face "Constraint_Error" '(nil default))
                  when E : Constraint_Error =>
                     --EMASCMD:(progn (forward-line -1)(back-to-indentation)(backward-sexp)(looking-at "begin -- 2"))
                     --EMASCMD:(progn (forward-line -2)(back-to-indentation)(forward-sexp)(looking-at "when -- 2"))
                     --EMACSCMD:(test-face "raise" font-lock-keyword-face)
                     --EMACSCMD:(test-face "Constraint_Error" '(nil default))
                     --EMACSCMD:(test-face "with" font-lock-keyword-face)
                     raise Constraint_Error with
                       "help " &
                       "me!";

                     --EMASCMD:(progn (forward-line 1)(back-to-indentation)(backward-sexp)(looking-at "when E :"))
                  when -- 2
                    Bad_Thing -- ada-mode 4.01 indentation
                    =>        -- ""
                     raise Constraint_Error
                       with Integer'Image (1) &
                         "help!";

                     --EMASCMD:(progn (forward-line 1)(back-to-indentation)(backward-sexp)(looking-at "when -- 2"))
                  when -- 3
                       -- pathological case - should put 'raise' on next line
                       -- just ensure it doesn't raise an error
                    E : others => raise
                       Constraint_Error with "help!";
               end;
               --EMASCMD:(progn (end-of-line 0)(backward-word 2)(backward-sexp)(looking-at "when -- 3"))

               --EMACSCMD:(progn (forward-line 4)(forward-comment 1)(backward-sexp)(looking-at "then -- 1"))
               --EMACSRESULT: t
               --EMACSCMD:(progn (forward-line 2)(forward-comment 1)(forward-sexp)(looking-at "then -- 2"))
               --EMACSRESULT: t
            elsif Dummy or -- 2
              Dummy_2
              --  Comment before 'then' (does not pass gnat style check).
            then -- 2
                 --EMACSCMD:(progn (end-of-line 0)(backward-word 2)(backward-sexp)(looking-at "elsif Dummy or -- 2"))
                 --EMACSRESULT: t
                 --EMACSCMD:(progn (end-of-line -2)(backward-word 2)(forward-sexp)(looking-at "elsif True -- 3"))
                 --EMACSRESULT: t

               -- nested if/then/else; test next-statement-keyword skips this
               if True then
                  null;
               end if;

               return 1;   -- a comment
                           -- another comment, aligned with previous

               --EMACSCMD:(progn (forward-line 4)(forward-comment 1)(backward-sexp)(looking-at "then -- 2"))
               --EMACSRESULT: t
               --EMACSCMD:(progn (forward-line 2)(forward-comment 1)(forward-sexp)(looking-at "then -- 3"))
               --EMACSRESULT: t
            elsif True -- 3
            then -- 3
                 --EMACSCMD:(progn (end-of-line 0)(backward-word 2)(backward-sexp)(looking-at "elsif True -- 3"))
                 --EMACSRESULT: t
                 --EMACSCMD:(progn (end-of-line -2)(backward-word 2)(forward-sexp)(looking-at "else -- 4"))
                 --EMACSRESULT: t
               return 1;

               --EMACSCMD:(progn (forward-line 4)(forward-comment 1)(backward-sexp)(looking-at "then -- 3"))
               --EMACSRESULT: t
               --EMACSCMD:(progn (forward-line 2)(forward-comment 1)(forward-sexp)(looking-at "; -- 5"))
               --EMACSRESULT: t
            else -- 4
               return 0;
               --EMACSCMD:(progn (forward-line 4)(forward-word 2)(backward-sexp)(looking-at "else -- 4"))
               --EMACSRESULT: t
               --EMACSCMD:(progn (forward-line 2)(forward-sexp)(looking-at "; -- 5"))
               --EMACSRESULT: t
            end if; -- 5
         end Local_Function;
      begin
         --EMACSCMD:(progn (end-of-line 0)(backward-word 1)(forward-sexp)(looking-at "; -- 6"))
         --EMACSRESULT: t
         return B : Integer :=
           (Local_Function);
         -- non-do extended return
      end F1; -- 6

      --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 2"))
      --EMACSRESULT:t

      --EMACSCMD:(progn (end-of-line 3)(ada-goto-declarative-region-start)(looking-at " -- target F2"))
      --EMACSRESULT:t
      function F2 (Param_1 : Discrete_Type_1; B : Float) return Float
      is -- target F2
         Local : Object_Access_Type_0a := new Float'(9.0);
      begin
         return D : Float
         do
            -- extended return with do
            --EMACSCMD:(progn(forward-line -3)(back-to-indentation)(forward-sexp)(looking-at "do"))
            --EMACSCMD:(progn(forward-line -3)(back-to-indentation)(forward-sexp)(looking-at "; -- 8"))

            --EMACSCMD:(progn (forward-line 2) (back-to-indentation) (forward-sexp)(looking-at "when A | Nominal.B"))
            --EMACSRESULT:t
            case Param_1 is
               -- comment after "is", before "when"
               --EMACSCMD:(progn (forward-line 2) (back-to-indentation) (forward-sexp)(looking-at "when C"))
               --EMACSRESULT:t
               when A | Nominal.B =>
                  goto Label_2;
                  --EMACSCMD:(progn (forward-line 2) (back-to-indentation) (forward-sexp)(looking-at "; -- 7"))
                  --EMACSRESULT:t
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
                  --  a comment after a label
                  D := D - Float (F1);

                  --  Comment before 'end case'
            end case; -- 7
         <<Label_2>> --  a sequence_of_statements can have a trailing label
         end return; -- 8
                     --EMACSCMD:(progn(forward-line -1)(forward-word 2)(backward-sexp)(looking-at "do"))
      end; -- no F2 on purpose

      --EMACSCMD:(test-face "E1" 'font-lock-function-name-face)
      --EMACSCMD:(progn (forward-line 5)(ada-which-function t))
      --EMACSRESULT:"E1"
      --EMACSCMD:(progn (forward-line 2)(forward-comment 1)(forward-sexp)(looking-at "when Local_1"))
      --EMACSRESULT:t
      entry E1 (X : Integer) when Local_1 = 0 is -- target E1
         Tmp : Integer := 0;
         Local_4 : Discrete_Type_1 := A;
         --EMACSCMD:(progn (forward-line 2)(forward-comment 1)(backward-sexp)(looking-at "is -- target E1"))
         --EMACSCMD:(progn (forward-line 1)(forward-comment 1)(forward-sexp)(looking-at "; -- E1"))
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
                     --EMACSCMD:(progn (forward-line 2)(forward-sexp 2)(looking-at "when 2 =>"))
                     --EMACSRESULT:t
                     when 1 =>
                        exit when Tmp > 1;
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
                  Local_1 := Local_1 + 2;
               end loop Loop_4;

            Loop_5 :
               while not (Local_1 > 0) loop
                  Local_1 := Local_1 + 2;
               end loop Loop_5;
         end case;

         --  A deeply nested case statement; used to cause parser explosion
         case Local_4 is
            when A =>
               case Local_4 is
                  when A =>
                     case Local_4 is
                        when A =>
                           case Local_4 is
                              when A =>
                                 null;
                              when B | C =>
                                 null;
                           end case;
                        when B |C =>
                           null;
                     end case;
                  when B | C =>
                     null;
               end case;
            when B | C =>
               null;
         end case;

         -- A comment before 'end'
         --EMACSCMD:(test-face "E1" 'font-lock-function-name-face)
      end E1; -- E1

      entry E2
        (X : Integer)
        when Local_1 = 0 and not
          (Local_2 = 1)
      is
         Tmp : Integer := 0;
      begin
         Local_2 := Tmp;
      end E2;

      entry E3
        (X : Integer) when Local_1 = 0 and not
          (Local_2 = 1)
      is
      begin
         null;
      end E3;

      procedure P1 is
      begin
         return;
         --EMACSCMD:(test-face "P1" 'font-lock-function-name-face)
      end P1;

      procedure P2 (A : Float; B : Float)
      is begin
         null;
      end; -- no P2
   end Protected_1;

   protected body Protected_Child_1 is
      entry E1 (X : Integer) when True is begin null; end E1;
   end Protected_Child_1;

   --EMACSCMD:(progn (forward-line 2)(ada-find-other-file)(looking-at "protected Protected_Buffer"))
   protected body Protected_Buffer is
      --EMACSRESULT: t
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

   task Executive
   with
     Storage_Size => 512 + 256,
     Priority => 5;
   --EMACSCMD:(progn (ada-which-function))
   --EMACSRESULT:"Ada_Mode.Nominal"

   task body Executive is -- target 5
   begin
      --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 5"))
      --EMACSRESULT:t

      --EMACSCMD:(progn (ada-which-function))
      --EMACSRESULT:"Executive"

      null;
   end Executive;

   task body Task_Type_1 is
      -- a more typical task
      Local_1 : Integer;
      Started : Boolean := False;
   begin
      select
         --  a comment after 'select'
         accept Start (A) (Param_1 : in Integer);
         Started := True;
      or
         when Started => -- Ada mode 4.01 ada-when-indent, GPS ada-indent
            accept Middle_1 (Param_1 : in Integer) do
               --  a comment after 'do'
               Local_1 := 0;
            end Middle_1;
            Local_1 := 0;
      or
         when
           Started =>
            accept Middle_2
              (Param_1 : in Integer)
            do
               Local_1 := 0;
               requeue Finish;
            end Middle_2;

      or when Started
           =>
            accept Finish;
            Local_1 := 5;
      or
         delay 1.0;
         null;
      end select;

      select -- need a separate select to test "else", "terminate" etc
         accept Start (A) (Param_1 : in Integer);
         Local_1 := 0;
      else
         --  comment after select else
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
        -- and '--'

        abort -- ada-mode 4.01 broken indent
         null;-- ada-mode 4.01 gets this wrong; it uses another broken indent.

      end select;

   end Task_Type_1;

   --  From a grammar bug report
   type S is synchronized interface;

   task type T is new S with
   end T;

   task body T is begin null; end;

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
      use Ada.Text_IO;
      File : File_Type;
   begin
      --  complex attribute argument
      raise Constraint_Error with Ada.Text_IO.Count'Image (Line (File)) &
        "foo";
      --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 7"))
      --EMACSRESULT:t
   end;
   --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 0"))
   --EMACSRESULT:t

   function Function_1a return Float
   is begin
      Procedure_2a;
      Procedure_2a;
      return 1.0 +
        Function_2a (Parent_Type_1'(1, 2.0, False)) +
        12.0;
      --EMACSCMD:(test-face "Function_1a" 'font-lock-function-name-face)
   end Function_1a;

   function Function_1b return Float
   is
      Local_1 : constant := 3.0;
   begin
      declare -- target 8

         -- no label, zero statements between begin, declare
         --EMACSCMD:(progn (ada-goto-end) (looking-at "; -- target 9"))
      begin
         return Local_1;
      exception
         --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " -- target 8"))
         --EMACSRESULT:t

         when others =>
            return 0.0;
      end; -- target 9
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
      ; -- pathological case; we don't really care what this indent is
      Procedure_2a;

      declare -- no label, two statements between begin, declare
      begin
         return 1.0;
      end;
   end;

   function Function_2a (Param : in Parent_Type_1) return Float
   is begin
   Block_1 :
      declare -- label, no statements between begin, label

         --EMACSCMD:(test-face "1.0e-36" 'font-lock-constant-face)
         Local_1 : Float := 1.0e-36;
         --EMACSCMD:(test-face "16#2#" 'font-lock-constant-face)
         Local_2 : Integer := 16#2#;
         Local_3 : Character := '\'; -- must override default syntax for \
         Local_4 : constant String := "a nested quote "" is hard";
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
           "abc:" & ':' & "def";
         Local_5 : Character := Character'('a');
         Local_6 : String    := String'('a', 'b');
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
      Local_A : Array_Type_2 :=
        -- indentation style from Ludovic Brenta
        (1 => (others => 1.0),
         2
           => (others
                 => 2.0),
         3
           => (others => 3.0));
   begin
      Procedure_2a;

      -- second begin block
      begin
         Local_A (1)(2) := 1.0;
      end;

   Block_1 :
      -- label, two statements between begin, label
      declare
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
      Procedure_2a;
      Procedure_2a;
      begin -- no declare, two statements
         return 1.0;
      end;
   end;

   package body Separate_Package_1 is separate;

   type Incomplete_Type_1 (Discriminant_1 : Integer) is tagged null record;

   type Incomplete_Type_2 (Discriminant_1 : Integer) is tagged null
     record; -- ada-indent-broken, not ada-indent-record-rel-type, because no record components

   type Incomplete_Type_3 (Discriminant_1 : Integer) is tagged
     null record;

   type Incomplete_Type_4 (Discriminant_1 : Integer) is
     tagged null record;
   -- rest of newline placement covered in spec

   pragma Warnings (Off, "coextension will not be deallocated when its associated owner is deallocated");
   --EMACSCMD:(test-face "new" 'font-lock-keyword-face)
   --EMACSCMD:(test-face-1 ", 1," "new" 'font-lock-keyword-face))
   --EMACSCMD:(test-face "Record_Type_3" 'font-lock-type-face)
   --EMACSCMD:(test-face-1 ":= new" "Record_Type_3" 'font-lock-type-face)
   --EMACSCMD:(test-face "1234" 'font-lock-constant-face)
   --EMACSCMD:(test-face-1 "(1234)" "1" 'font-lock-constant-face)
   --EMACSCMD:(test-face "1.234" 'font-lock-constant-face)
   Object_3 : access Record_Type_3 := new Record_Type_3 (new Integer'(1234), 1, new Float'(1.234));
begin
   null;
end Ada_Mode.Nominal;
-- Local Variables:
-- ada-auto-case: t
-- ada-end-name-optional: t
-- End:
