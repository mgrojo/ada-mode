--  Test ada-find-other-file

-- Point on "package" line, goto its parent
--EMACSCMD:(ada-parse-prj-file "ada_mode.gpr")
--EMACSCMD:(ada-select-prj-file "ada_mode.gpr")
--EMACSCMD:(progn (forward-line 1)(forward-word 1)(ada-find-other-file t)(looking-at "Ada_Mode is"))
package Ada_Mode.Find_File is
   --EMACSRESULT:t

   -- point between package start and first decl; find package body
   --EMACSCMD:(progn (ada-find-other-file t)(looking-at "package body Ada_Mode.Find_File is"))

   procedure P_Bug_One;

   --EMACSCMD:(progn (forward-line 1)(ada-find-other-file t)(looking-at "   procedure P_Bug$"))
   procedure P_Bug
     (Param_1 : in Integer);
   --EMACSRESULT:t

end Ada_Mode.Find_File;
