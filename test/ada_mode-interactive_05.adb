-- Simplified from ada_mode-interactive_01.adb;
-- ada-make-subprogram-body got wrong subprogram name for Proc_1.
--
procedure Ada_Mode.Interactive_05
is
   --EMACSCMD:(progn (end-of-line 1)(newline-and-indent)(execute-kbd-macro "foo_bar_baz ")(let ((case-fold-search nil))(looking-back "Foo_Bar_Baz ")))
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line -2)(kill-line 1))

   --EMACSCMD:(progn (forward-line 1)(downcase-word 1)(execute-kbd-macro "\C-u\C-c\C-w")(let ((case-fold-search nil))(looking-back "Ada_Identifier")))
   -- Ada_Identifier in comment; force auto-case
   --EMACSRESULT:t

   --EMACSCMD:(progn (end-of-line 3)(kill-line 4)(insert ";")(ada-make-subprogram-body))
   -- result verified by diff.
   procedure Proc_1
   is begin

   end Proc_1;

   -- procedure, parameters on one line
   --EMACSCMD:(progn (end-of-line 3)(kill-line 4)(insert ";")(ada-make-subprogram-body))
   -- result verified by diff
   procedure Proc_2 (A : in Integer)
   is begin

   end Proc_2;

   function Func_1 return Integer
   is begin
      return 0;
   end Func_1;

begin
   null;
end Ada_Mode.Interactive_05;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
