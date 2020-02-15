--  Test ada-find-other-file and a few other things

--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-tool (gpr_query "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
package body Ada_Mode.Nominal.Child is -- target 0

   --EMACSCMD:(progn (end-of-line 3)(kill-line 4)(insert ";")(ada-make-subprogram-body)(insert "null;"))
   -- result verified by diff.
   overriding procedure Procedure_1a (Item  : in out Child_Type_1)
   is begin
null;
   end Procedure_1a;
   
   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(ada-find-other-file)(and (looking-at "overriding function Function_2a") (string= (file-name-nondirectory (buffer-file-name)) "ada_mode-nominal-child.ads")))
   --EMACSRESULT:t
   overriding function Function_2a (Param : in Child_Type_1) return Float
   is
      pragma Unreferenced (Param);
   begin
      --EMACSCMD:(progn (forward-line 2)(forward-word 1)(ada-find-other-file)(looking-at "overriding function Function_2a"))
      --EMACSRESULT:t
      return 0.0;
   end Function_2a;

   --EMACSCMD:(progn (end-of-line 5)(kill-line 4)(insert ";")(ada-make-subprogram-body)(insert "return 0.0;"))
   -- result verified by diff.
   overriding
   function Function_2b (Param : in Child_Type_1) return
     Float
   is begin
return 0.0;
   end Function_2b;
   
   function Static_Call_Function_2b return Float
   is begin
      return Function_2b (Child_Type_1'(1, 1.0, False, 2, 2.0, True));
   end Static_Call_Function_2b;

   function Dynamic_Call_Function_2b (Item : in Parent_Type_1'Class) return Float
   is begin
      return Function_2b (Item);
   end Dynamic_Call_Function_2b;

   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(forward-char 1)(ada-goto-declarative-region-start)(looking-at " -- target 0"))
   --EMACSRESULT:t
   overriding function Function_2c (Param : in Child_Type_1)
                                   return Float
   is -- target Function_2c
   begin
      return 0.0;
   end Function_2c;

   function Function_2d_Different_Name
     (Param : in Child_Type_1) return Float
   is begin
      return 0.0;
   end Function_2d_Different_Name;

   overriding function
     Function_2e (Param : in Child_Type_1) return Float
   is begin
      return 0.0;
   end Function_2e;

   function Function_2f_Different_Name
     (Param : in Child_Type_1)
     return Float
   is begin
      return 0.0;
   end Function_2f_Different_Name;

   function Child_Add (Left, Right : in Child_Type_1) return Child_Type_1
   is begin
      return (Parent_Type_1
              with 1, 0.0, False);
   end Child_Add;

   --  Homonym, for testing xref with no line/col info
   --EMACSCMD:(test-all-defs "function Child_Add" t)
   --EMACSRESULT_START:(cl-ecase ada-xref-tool (gpr_query '("ada_mode-nominal-child.ads" "Child_Add Ada_Mode.Nominal.Child.Child_Type_1;(Left, Right) function")) (gnat '("ada_mode-nominal-child.adb" "Child_Add spec")))
   --EMACSRESULT_ADD:  (cl-ecase ada-xref-tool (gpr_query '("ada_mode-nominal-child.adb" "Child_Add Ada_Mode.Nominal.Child.Child_Type_1;(Left, Right) body")) (gnat '("ada_mode-nominal-child.adb" "Child_Add body")))
   --EMACSRESULT_ADD:  (cl-ecase ada-xref-tool (gpr_query '("ada_mode-nominal-child.adb" "Child_Add (Left) function/body"))(gnat '("ada_mode-nominal-child.ads" "Child_Add spec")))
   --EMACSRESULT_ADD: (when (eq ada-xref-tool 'gnat) '("ada_mode-nominal-child.adb" "Child_Add body"))
   --EMACSRESULT_FINISH:
   function Child_Add (Left : in Child_Type_1) return Child_Type_1
   is begin
      return (Parent_Type_1 with 1, 0.0, False);
   end Child_Add;

   procedure Function_2h (Param_1 : in Child_Type_1; Param_2 : in Integer)
   is
      pragma Unreferenced (Param_1, Param_2);
   begin
      null;
   end Function_2h;

   overriding function Function_2i (Param : in Child_Type_2) return Child_Type_2
     is (1, 1.0, False, 2);

end Ada_Mode.Nominal.Child;
