-- More cases for indenting parens.
--
-- Also cases where ada-indent-comment-gnat matters a lot;
-- ada_mode-conditional_expressions sets ada-indent-comment-gnat.

procedure Ada_Mode.Conditional_Expressions.More_1
is
   K1 : Integer := (if J > 42 then -1
                    --  comment
                    else +1);

   K2 : Integer := (if J > 42
                      --  comment
                    then -1
                    else +1);

   K2a : Integer :=
     (if J > 42
        --  comment; same as next test case
      then -1
      else +1);

   K2b : Integer :=
     (if J >
        42
        --  comment; matches last line of preceding expression
      then -1
      else +1);
begin
   --  From real code; shows additional requirements for ada-indent-aggregate
   A :=
     (if B
      then Add_Nonterm
        ((+Token_Keyword_Non_Grammar_Id, 0),
         (1 => Add_Identifier (Keyword_Id)))
      -- '(1' was indented wrong
      else Add_Nonterm);
end Ada_Mode.Conditional_Expressions.More_1;
