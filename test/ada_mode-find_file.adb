--  other file for testing ada-find-other-file; requires search path
-- test .gpr as only project file
--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "ada_mode.gpr") (gnat "ada_mode-gnatxref.prj")) (ada-prj-default))

-- Select package name, goto its parent spec
--EMACSCMD:(progn (forward-line 1)(forward-word 2)(forward-char 1)(push-mark-command t t)(forward-word 2)(ada-find-other-file)(looking-at "package Ada_Mode is"))
package body Ada_Mode.Find_File is
   --EMACSRESULT:t
   --EMACSCMD:(progn (pop-mark)(deactivate-mark))

   -- point between package start and first decl; find package spec
   --EMACSCMD:(progn (ada-find-other-file)(looking-at "package Ada_Mode.Find_File is"))

   procedure P_Bug_One is
   begin
      null;
   end P_Bug_One;

   --EMACSCMD:(progn (forward-line 2)(ada-find-other-file)(looking-at (cl-ecase ada-xref-backend (eglot "--  Test ada-find-other-file") ((gpr_query gnat) "procedure P_Bug$"))))
   procedure P_Bug
     (Param_1 : in Integer)
   is
      --EMACSRESULT:t
   begin
      --EMACSCMD:(progn (ada-find-other-file)(looking-at (cl-ecase ada-xref-backend (eglot "--  Test ada-find-other-file") ((gpr_query gnat) "procedure P_Bug$"))))
      null;
      --EMACSRESULT:t
   end P_Bug;

end Ada_Mode.Find_File;
--Local Variables:
--ada-eglot-gpr-file: nil
--End:
