--  From real code; shows additional requirements for ada-indent-aggregate
--
--
--
procedure Ada_Mode.Conditional_Expressions.More_1
is
begin
   A :=
     (if B
      then Add_Nonterm
        ((+Token_Keyword_Non_Grammar_Id, 0),
         (1 => Add_Identifier (Keyword_Id)))
      -- '(1' was indented wrong
      else Add_Nonterm);
end Ada_Mode.Conditional_Expressions.More_1;
