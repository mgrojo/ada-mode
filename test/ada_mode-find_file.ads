--  Test ada-find-other-file

-- Point on "package" line, goto its parent
--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "ada_mode.gpr") (gnat "ada_mode-gnatxref.prj")) (ada-prj-default))
--EMACSCMD:(progn (forward-line 1)(forward-word 1)(ada-find-other-file)(looking-at "package Ada_Mode is"))
package Ada_Mode.Find_File is
   --EMACSRESULT:t

   -- point between package start and first decl; find package body
   --EMACSCMD:(progn (ada-find-other-file)(looking-at "package body Ada_Mode.Find_File is"))

   procedure P_Bug_One;

   --EMACSCMD:(progn (forward-line 2)(ada-find-other-file)(looking-at (cl-ecase ada-xref-backend (eglot "--  other file for testing ada-find-other-file") ((gpr_query gnat) "procedure P_Bug$"))))
   procedure P_Bug
     (Param_1 : in Integer);
   --EMACSRESULT:t

end Ada_Mode.Find_File;
--Local Variables:
--ada-eglot-gpr-file: nil
--End:
