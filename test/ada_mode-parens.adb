--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")
package body Ada_Mode.Parens is
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
                             "789");

      --EMACSCMD:(progn (end-of-line 2)(ada-case-adjust)(let ((case-fold-search nil))(looking-back "comMENT")))
      -- A comment for testing no auto-case in comMENT
      --EMACSRESULT:t

      Local_10 : String :=
        (
         "123" &
           "456" &
           "789"
        );

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
         2 =>
           (1 => 22,
            2 => 23,
            3 => 24),
         3 => (others => 30));

      --EMACSCMD:(progn (forward-line 3)(forward-word 1)(insert "   ")(ada-align))
      -- result is tested in diff
      return
        (1      => 1,
         2      =>
           1 + 2 * 3,
         3      => 1 +
           3 * 4,
         others => 5);
   end;

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
                   and then C
                   and then D)  --  requires ada-indent-validate-cache-paren
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

      --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(ada-next-statement-keyword)(looking-at "loop"))
      --EMACSRESULT: t
      while A.all
        or else B.all
        --EMACSCMD:(progn (forward-line 3)(back-to-indentation)(ada-next-statement-keyword)(looking-at "end loop"))
        --EMACSRESULT: t
        -- FIXME (later): conflicts with gnat style check
      loop
         if A = null then B.all := False; end if; -- cached keywords between 'loop' and 'end loop'
      end loop;

      --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(ada-next-statement-keyword)(looking-at "loop"))
      --EMACSRESULT: t
      while A.all
        or else (B.all
                   and then C
                   and then D)
      loop
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
   -- default at end of list
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
      Hello : constant String := "hello";
      There  : constant String := " there";
      Out_File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Put_Line ("Hello" & ' ' & -- test ada-indent-next keyword with string, character literal
                              "World");

      Ada.Text_IO.Put_Line (Out_File, -- smie-backward-sexp returns "(" here
                            Hello &
                              There);
      Ada.Text_IO.Put_Line ( -- comment after paren
                            Out_File,
                            Hello &
                              There);
   end Hello;

end Ada_Mode.Parens;
