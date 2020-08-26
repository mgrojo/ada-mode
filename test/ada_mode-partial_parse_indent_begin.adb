--  Testing indent of 'begin' in partial parse.

--EMACSCMD:(progn (end-of-line 4)(delete-char 1)(wisi-indent-newline-indent)(current-column))
--EMACSRESULT: 3
procedure Ada_Mode.Partial_Parse_Indent_Begin
is

   --EMACSCMD:(progn (end-of-line 3)(delete-char 1)(wisi-indent-newline-indent)(current-column))
   --EMACSRESULT: 3
   A : Integer;

   procedure Foo
   is
      --EMACSCMD:(progn (end-of-line 3)(delete-char 1)(wisi-indent-newline-indent)(current-column))
      --EMACSRESULT: 6
   begin

      --EMACSCMD:(progn (end-of-line 3)(delete-char 1)(wisi-indent-newline-indent)(current-column))
      --EMACSRESULT: 6
      A := A + 1;

   end Foo;

   -- indent-newline-indent after 'begin' indents 'begin' incorrectly;
   -- don't do that. Insert newline at beginning of next line.
begin

   --EMACSCMD:(progn (beginning-of-line 1)(delete-char -1)(wisi-indent-newline-indent)(current-column))
   --EMACSRESULT: 3


   --EMACSCMD:(progn (end-of-line 3)(delete-char 1)(wisi-indent-newline-indent)(current-column))
   --EMACSRESULT: 3
   Foo;


end Ada_Mode.Partial_Parse_Indent_Begin;
-- Local Variables:
-- wisi-partial-parse-threshold: 0
-- End:
