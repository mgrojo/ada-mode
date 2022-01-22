-- In incremental parse, an error used to remain.
procedure Ada_Mode.Interactive_15
is
   P_Status : Parser_Status;

   --EMACSCMD:(progn (forward-line 3)(insert "C : N ")(indent-for-tab-command)(length (wisi-parser-parse-errors wisi--parser)))
   --EMACSRESULT:1
   --EMACSCMD:(progn (end-of-line 2)(insert "renames A;\n")(indent-for-tab-command)(length (wisi-parser-parse-errors wisi--parser)))

   --EMACSRESULT:0
   --EMACSCMD:(delete-region (line-beginning-position -1) (line-beginning-position -2))

begin
   null;
end Ada_Mode.Interactive_15;
