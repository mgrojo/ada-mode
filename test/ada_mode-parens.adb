--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")

--EMACSCMD:(progn (wisi-parse-buffer 'face)(font-lock-ensure))

with Ada.Strings.Maps;
package body Ada_Mode.Parens is

   --  This used to cause exponential explosion of parallel parsers;
   --  now it's linear.
   No_Conditional_Set : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps."or"
       (Ada.Strings.Maps.To_Set (' '),
        Ada.Strings.Maps."or"
          (Ada.Strings.Maps.To_Set ('.'),
           Ada.Strings.Maps."or"
             (Ada.Strings.Maps.To_Set (','),
              Ada.Strings.Maps."or"
                (Ada.Strings.Maps.To_Set (':'),
                 Ada.Strings.Maps."or"
                   (Ada.Strings.Maps.To_Set (';'),
                    Ada.Strings.Maps."or"
                      (Ada.Strings.Maps.To_Set ('!'),
                       Ada.Strings.Maps."or"
                         (Ada.Strings.Maps.To_Set ('('),
                          Ada.Strings.Maps.To_Set (')'))))))));

   --EMACSCMD:(progn (forward-line 3)(forward-word 2)(newline)(ada-align))
   -- only one default. result is tested by .diff
   function Function_1
     (Param_1, Param_2, Param_3 : in     Ada.Text_IO.Count;
      Param_4                   : in out Integer;
      Param_5                   : in out Integer;
      Param_6                   : in     Float := 1.0)
     return Float
   is
      Local_1 : Integer := (1 + 2 + 3);
      Local_2 : Integer := (1 + 2 +
                              3);
      Local_3 : Integer := (1 + 2
                              + 3);
      Local_4 : Integer := (1 +
                              2 + 3);
      Local_5 : Integer :=
        (1 + 2 +
           3);

      --EMACSCMD:(progn (end-of-line 2)(forward-char -3)(ada-case-adjust)(let ((case-fold-search nil))(looking-back "aBc")))
      Local_6 : String := ("123" & "456" & "aBc");
      --EMACSRESULT:t
      Local_7 : String := ("123" & "456" &
                             "789");
      Local_8 : String := ("123" &
                             "456" & "789");

      -- no comment between ( and first association
      Local_9 : String := (
                           "123" &
                             "456" &
                             "789 [");
      --EMACSCMD:(progn (end-of-line 2)(ada-case-adjust)(let ((case-fold-search nil))(looking-back "comMENT")))
      -- A comment for testing no auto-case in comMENT
      --EMACSRESULT:t

      Local_10 : String :=
        (
         "123" &
           "456" &
           "789"
           -- There are conflicting requirements on indenting a hanging
           -- right paren; when entering new code, we want it aligned
           -- where the new code would be. When left hanging, we want it
           -- aligned with the matching left paren. We choose the
           -- latter, partly for backward compatibility.
        );

      --  function call (actually type conversion, but it's the same indentation) in aggregate
      type Local_11_Type is record
         A : Integer;
         B : Integer;
      end record;

      Local_11 : Local_11_Type := Local_11_Type'
        (A => Integer
           (1.0),
         B => 1 +
           Integer
             (2.0));

      Local_12 : Local_11_Type
        := Local_11_Type'(A => Integer
                            (1.0),
                          B => Integer (2.0));

      Local_13 : Local_11_Type
        := (Integer'(1),
            Integer'(2));

      type Local_14_Type is record
         A : String (1 .. 3);
         B : String (1 .. 6);
      end record;

      Local_14 : Local_14_Type :=
        ("123",
         "456" &
           ("789"));
   begin
      return Float (
                    Integer'Value
                      (Local_6));
   end Function_1;

   function Function_2
     (Left,
        Right : in Array_Type_1) -- ada-indent-broken to match 4.01
     return Array_Type_1
   is
      type Matrix_Type is array (1 .. 4) of Array_Type_1;
      A : Matrix_Type :=
        ((1, 2, 3),
         (4, 5, 6),
         (7, 8, 9),
         (10, 11, 12));

      --  Test highly nested aggregates
      type Tensor_Type is array (1 ..2) of Matrix_Type;
      B : Tensor_Type :=
        (((1,
           2, 3),
          (4,
           5, 6),
          (7, 8, 9),
          (10, 11, 12)),
         ((1, 2, 3),
          (4, 5, 6),
          (7, 8, 9),
          (10, 11, 12)));

      C : Tensor_Type := (((1,
                            2, 3),
                           (4,
                            5, 6),
                           (7, 8, 9),
                           (10, 11, 12)),
                          ((1, 2, 3),
                           (4, 5, 6),
                           (7, 8, 9),
                           (10, 11, 12)));

      function To_Array (First : in Integer) return Array_Type_1
      is begin
         return
           (First, 0, 0);
      end To_Array;

      procedure Check
        (Label    : in String;
         Computed : in Array_Type_1;
         Expected : in Array_Type_1)
      is begin
         null;
      end Check;

   begin
      A :=
        (1 |
           2 => (0, 0, 0),
         others => (1, 1, 1));

      A :=
        (1 |
           2 => (1, 1, 1),
         3 |
           4 => (2, 2, 2));

      -- Parsing inside nested parens
      A :=
        (
         -- a comment between paren and first association
         1 =>
           (1 => 12,
            2 => 13,
            3 => 14),
         2 => To_Array

           (22), -- blank line in function call in aggregate (phew!)

         3 => (others => 30),
         4 => (others => 40));

      Check
        ("foo bar",
         A
           (1),
         A (2));

      --EMACSCMD:(progn (forward-line 3)(forward-word 1)(insert "   ")(ada-align))
      -- result is tested in diff
      return
        (1      =>
           1,
         2      =>
           1 + 2 * 3,
         3      => 1 +
           3 * 4,
         others => 5);
   end;

   --EMACSCMD:(progn (forward-line 3)(forward-word 2)(newline)(ada-align))
   -- lines before ( and after ) not empty. result is tested by .diff
   function Function_3 (Param_1 : in     Ada.Text_IO.Count;
                        Param_2 : in out Integer) return Float
   is begin
      return
        1.0 +
        2.0;
   end Function_3;

   --EMACSCMD:(font-lock-ensure)

   --EMACSCMD:(test-face "Boolean" font-lock-type-face)
   --EMACSCMD:(progn (forward-line 4)(test-face "Boolean" font-lock-type-face))
   --EMACSCMD:(progn (forward-line 2)(forward-word 3)(insert "   ")(forward-line 2)(forward-word 2)(insert "   ")(ada-align))
   -- paren on same line as 'procedure' to test that case in ada-format-paramlist. result is tested by .diff
   procedure If_Statement (A : access          Boolean;
                           B : not null access Boolean;
                           C : in              Boolean;
                           D : in              Boolean;
                           E : in              Boolean;
                           G : in              Boolean)
   is
   begin

      if A.all
        or else B.all
        or else C
      then
         null;
      end if;

      if A.all
        or else (B.all
                   --EMACSCMD:(test-face "then" 'font-lock-keyword-face)
                   and then
                   (C)
                   and then D)
      then
         null;
      end if;

      if A.all
        or else (B.all
                   and then C
                   and then D)
        or else ((B.all
                    and then C)
                   or else
                   (D
                      and then E))
        or else G
      then
         null;
      end if;

      --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(forward-sexp)(looking-at "loop -- target 1"))
      --EMACSRESULT: t
      while A.all
        or else B.all
        --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(forward-sexp)(looking-at "; -- target 2"))
        --EMACSRESULT: t
      loop -- target 1
         if A = null then B.all := False; end if; -- cached keywords between 'loop' and 'end loop'
      end loop; -- target 2

      --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(forward-sexp)(looking-at "loop -- target 3"))
      --EMACSRESULT: t
      while A.all
        or else (B.all
                   and then C
                   and then D)
      loop -- target 3
         null;
      end loop;

      while A.all
        or else (B.all
                   and then C
                   and then D)
        or else ((B.all
                    and then C)
                   or else
                   (D
                      and then E))
        or else G
      loop
         null;
      end loop;

      loop
         exit when A.all
           or else B.all
           or else C;

         exit when A.all
           or else (B.all
                      and then C
                      and then D);

         exit when A.all
           or else (B.all
                      and then C
                      and then D)
           or else ((B.all
                       and then C)
                      or else
                      (D
                         and then E))
           or else G;
      end loop;

   end If_Statement;

   --EMACSCMD:(progn (forward-line 4)(forward-word 2)(insert "   ")(ada-align))
   -- multiple defaults requiring alignment
   procedure Param_Format_1
     (A, B : in     Float   := 1.0;
      C, D : in     Integer := 2;
      E    :    out Character;
      F    : in out Integer);

   -- body for previous spec
   --EMACSCMD:(progn (forward-line 3)(insert "  ")(forward-line 1)(ada-align))
   -- multiline, multi-identifier, followed on same line by "is"
   procedure Param_Format_1
     (A, B : in     Float   := 1.0;
      C, D : in     Integer := 2;
      E    :    out Character;
      F    : in out Integer) is
   begin
      E := 'A';
   end;

   --EMACSCMD:(progn (forward-line 4)(ada-align))
   -- multiline, followed on same line by "return"
   function Param_Format_3
     (A : in     Float;
      B :    out Integer) return Float
   is begin
      B := 1;
      return A;
   end;

   --EMACSCMD:(progn (forward-line 4)(ada-align))
   -- multiline, no modes
   function Param_Format_4
     (A : Float   := 2.0;
      B : Integer := 3)
     return Float
   is begin
      return A;
   end;

   --EMACSCMD:(font-lock-ensure)

   --EMACSCMD:(progn (forward-line 9)(test-face "protected" 'font-lock-keyword-face))
   --EMACSCMD:(progn (forward-line 8)(test-face "procedure" 'font-lock-keyword-face))
   --EMACSCMD:(progn (forward-line 8)(test-face "constant" 'font-lock-keyword-face))
   --EMACSCMD:(progn (forward-line 7)(test-face "Integer" 'font-lock-type-face))
   --EMACSCMD:(progn (forward-line 6)(forward-word 4) (test-face "Integer" 'font-lock-type-face))
   --EMACSCMD:(progn (forward-line 4)(forward-word 2)(insert "    ")(ada-align))
   -- multiline access [constant | protected]
   function Param_Format_6
     (A : access constant Float;
      B : access protected procedure := null;
      C : access constant Integer    := new Integer'(1 + 3 * 4);
      D : out    Character)
     return Float
   is begin
      D := 'Z';
      return A.all;
   end;

   --EMACSCMD:(progn (forward-line 4)(test-face "constant" 'font-lock-keyword-face))
   --EMACSCMD:(progn (forward-line 4)(forward-word 2)(insert "    ")(ada-align))
   -- multiline [not null] access [constant | protected]
   function Param_Format_7
     (A : not null access constant Float;
      B : access          protected procedure := null;
      C : access          constant Integer    := new Integer'(1 + 3 * 4);
      D : in              Character;
      E :    out          Character)
     return Float
   is begin
      E := 'z';
      return A.all;
   end;

   --EMACSCMD:(progn (forward-line 4)(forward-word 2)(insert "    ")(ada-align))
   -- default at end of list, only 'in'
   procedure Param_Format_8
     (Grammar           : in String;
      Analyzer          : in String;
      Trace             : in Boolean := False;
      Put_Grammar       : in Boolean := False;
      First_State_Index : in Integer := 1)
   is begin
      null;
   end;

   --EMACSCMD:(progn (forward-line 2)(forward-word 5)(insert "    ")(ada-align))
   -- single line no mode
   function Param_Format_S1 (A : Float; B : Integer := 3) return Float
   is begin
      return A;
   end;

   --EMACSCMD:(progn (forward-line 2)(forward-word 5)(insert "    ")(ada-align))
   -- single line access [constant | protected]
   procedure Param_Format_S2 (A : access constant Float; B : access protected procedure)
   is begin
      null;
   end;

   -- string nested in parens
   procedure Hello
     (Message_1 : in String := "from ada_mode-parens.adb";
      Message_2 : in String := "from ada_mode-parens.adb")
   is
      Hello    : constant String := "hello";
      There    : constant String := " there";
      Out_File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Put_Line ("Hello" & ' ' &
                              "World");

      Ada.Text_IO.Put_Line (Out_File,
                            Hello &
                              There);
      Ada.Text_IO.Put_Line ( -- comment after paren
                            Out_File,
                            Hello &
                              There
                              --  Comment before trailing paren
                           );
      Ada.Text_IO.Put_Line (Out_File,
                            Hello & There
                            --  Comment before trailing paren, token.First = true
                           );
      Ada.Text_IO.Put_Line
        (Hello & There
         --  Comment before trailing paren, token.First = False
        );

      Ada.Text_IO.Put_Line (Item =>
                              Hello & There
                              --  Comment before trailing paren, token.First = True
                           );
      Ada.Text_IO.Put_Line (Item =>
                              Hello &
                                There
                                --  Comment before trailing paren, token.First = True
                           );
      Ada.Text_IO.Put_Line
        (Item => Hello & There
         --  Comment before trailing paren, token.First = False
        );
      Ada.Text_IO.Put_Line (Item => Hello &
                              There
                              --  Comment before trailing paren, token.First = False
                           );
   end Hello;

   --  Slice in procedure call
   procedure Slice_1 (A : in Integer; B : in String)
   is begin
      null;
   end Slice_1;

   procedure Slice
   is
      C: constant String := "abcd";
   begin
      Slice_1
        (1,
         C
           (1 .. 2));
   end Slice;

   procedure Weird_List_Break is
   begin
      Slice_1 (1
               ,    --  used to get an error here; don't care about the actual indentation
               "string");
   end;

   procedure Quantified_Exression is

      type T is (V1,V2,V3);

      A : array (T) of T := (others => V1);

      -- ARM 4.5.8(4) allows removing the doubled parens
      -- around a quantified expression
      pragma Assert (for all X of A => X in V1);

      procedure F1 (Item : in Boolean) is begin null; end;
   begin
      F1 (for all X of A => X in V1);
   end;

   procedure If_Expr_As_Actual_Parameter is

      function ID (X : Boolean) return Boolean is
      begin
         return X;
      end ID;

      -- ARM 4.5.7(7) allows removing the doubled parens
      -- around a conditional_expression
      Tmp : Boolean := ID (if True then True else True);
   begin
      null;
   end If_Expr_As_Actual_Parameter;

end Ada_Mode.Parens;
--  Local Variables:
--  ada-indent-comment-gnat: t
--  ada-end-name-optional: t
--  End:
