--  other file for testing ada-find-other-file

-- Select package name, goto its spec
--EMACSCMD:(progn (forward-line 1)(forward-word 2)(forward-char 1)(push-mark-command t t)(forward-word 2)(ada-find-other-file t)(pop-mark)(looking-at "Ada_Mode is"))
package body Ada_Mode.Find_File is
   --EMACSRESULT:t

   procedure P_Bug_One is
   begin
      null;
   end P_Bug_One;

   --EMACSCMD:(progn (forward-line 1)(ada-find-other-file t)(looking-at "   procedure P_Bug$"))
   procedure P_Bug
     (Param_1 : in Integer)
   is
      --EMACSRESULT:t
   begin
      --EMACSCMD:(progn (ada-find-other-file t)(looking-at "   procedure P_Bug$"))
      null;
      --EMACSRESULT:t
   end P_Bug;

end Ada_Mode.Find_File;
