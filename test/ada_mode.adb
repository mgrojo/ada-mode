package body Ada_Mode is

   protected body Separate_Protected_Body is separate;

   task Separate_Task_Body is
      entry Please_Abort;
   end;

   task body Separate_Task_Body is separate;

   procedure Separate_Procedure is separate;

   -- testing that ada-auto-case is buffer-local;  nil in separate_package_1
   function Separate_Function return Integer is separate;
   --EMACSCMD:ada-auto-case
   --EMACSRESULT:t
   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(downcase-word 2)(execute-kbd-macro " ")(delete-char -1)(let ((case-fold-search nil))(looking-back "Ada_Mode")))
   --EMACSRESULT:t
end Ada_Mode;
-- Local Variables:
-- ada-auto-case: t
-- End:
