--  other file for testing ada-find-other-file; requires search path
-- test .gpr as only project file
--EMACSCMD:(ada-parse-prj-file "ada_mode.gpr")
--EMACSCMD:(ada-select-prj-file "ada_mode.gpr")
--EMACSCMD:ada-prj-current-file
--EMACSRESULT:(expand-file-name "ada_mode.gpr")

-- Select package name, goto its parent spec
--EMACSCMD:(progn (forward-line 1)(forward-word 2)(forward-char 1)(push-mark-command t t)(forward-word 2)(ada-find-other-file)(pop-mark)(looking-at "package Ada_Mode is"))
package body Ada_Mode.Find_File is
   --EMACSRESULT:t

   -- point between package start and first decl; find package spec
   --EMACSCMD:(progn (ada-find-other-file)(looking-at "package Ada_Mode.Find_File is"))

   procedure P_Bug_One is
   begin
      null;
   end P_Bug_One;

   --EMACSCMD:(progn (forward-line 2)(ada-find-other-file)(looking-at "procedure P_Bug$"))
   procedure P_Bug
     (Param_1 : in Integer)
   is
      --EMACSRESULT:t
   begin
      --EMACSCMD:(progn (ada-find-other-file)(looking-at "procedure P_Bug$"))
      null;
      --EMACSRESULT:t
   end P_Bug;

end Ada_Mode.Find_File;
