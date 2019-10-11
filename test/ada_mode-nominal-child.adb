--  Test ada-find-other-file and a few other things

--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-tool (gpr_query "subdir/ada_mode.adp") (gnatxref "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
package body Ada_Mode.Nominal.Child is

   --EMACSCMD:(progn (end-of-line 3)(kill-line 4)(insert ";")(ada-make-subprogram-body)(insert "null;"))
   -- result verified by diff.
   overriding procedure Procedure_1a (Item  : in out Child_Type_1)
   is begin
      null;
   end Procedure_1a;

   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(ada-find-other-file)(looking-at "overriding function Function_2a"))
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

   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(forward-char 1)(ada-goto-declarative-region-start)(looking-at " -- target Function_2c"))
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

end Ada_Mode.Nominal.Child;
