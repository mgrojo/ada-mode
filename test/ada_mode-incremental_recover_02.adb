--EMACS_SKIP_UNLESS: wisi-incremental-parse-enable
function Ada_Mode.Incremental_Recover_02
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

         --EMACSCMD:(progn (end-of-line 4)(forward-char -2)(insert "\n when Non_Syntax_Label => False")(indent-for-tab-command))
         --EMACSCMD:(progn (end-of-line 3)(insert ",")(length (wisi-parser-parse-errors wisi--parser)))
         --EMACSRESULT:0
         when Nonterm => Item.Element_Node.Virtual);
   --EMACSCMD:(progn (end-of-line 0)(forward-char -2)(set-mark (point))(end-of-line 0)(forward-char -1)(delete-region (point)(mark)))
end Ada_Mode.Incremental_Recover_02;
