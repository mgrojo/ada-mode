function Ada_Mode.Interactive_06
  (Tree   : in Syntax_Trees.Tree;
   Item : in Recover_Token)
  return Boolean
is
   pragma Unreferenced (Tree);
begin
   return
     (if Item.Virtual
      then Item.Contains_Virtual_Terminal
      else
        (case Item.Element_Node.Label is
            when Source_Terminal => False,
            when Virtual_Terminal | Virtual_Identifier => True,

            -- First insert code missing final ',', check that gives a syntax
            -- error. Then insert ',', check the error goes away.

            --EMACSCMD:(progn (end-of-line 6)(forward-char -3)(insert "\n when Non_Syntax_Label => False")(indent-for-tab-command)(length (wisi-parser-parse-errors wisi--parser)))
            --EMACSRESULT:1
            --EMACSCMD:(progn (end-of-line 4)(insert ",")(indent-for-tab-command))
            --EMACSCMD:(length (wisi-parser-parse-errors wisi--parser))
            --EMACSRESULT:0
            when Nonterm => Item.Element_Node.Virtual));
   --EMACSCMD:(progn (end-of-line 0)(forward-char -3)(set-mark (point))(end-of-line 0)(forward-char -1)(delete-region (point)(mark)))
end Ada_Mode.Interactive_06;
