-- Used to get "error during resume" with incremental parse.
--
-- This is extracted from ada_mode-interactive_2.adb, then expanded.
procedure Ada_Mode.Interactive_03
is

   --EMACSCMD:(progn (end-of-line 7)(delete-char -2)(newline-and-indent))
   --EMACSCMD:(progn (forward-line 5)(execute-kbd-macro "is begin\nnull;\nend Function_Access_1;\n")(current-indentation))
   --EMACSRESULT:3
   function Function_Access_1
     (A_Param : in Float)
     return Standard.Float;

   --EMACSCMD:(let ((i 1)) (while (< i 9) (setq i (1+ i)) (undo)(wisi-indent-statement)))

begin
   null;
end Ada_Mode.Interactive_03;
