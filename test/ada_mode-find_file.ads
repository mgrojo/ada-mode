--  Test ada-find-other-file

-- Point on "package" line, goto its parent
--EMACSCMD:(progn (forward-line 1)(forward-word 1)(ada-find-other-file t)(looking-at "Ada_Mode is"))
package Ada_Mode.Find_File is
   --EMACSRESULT:t

   procedure P_Bug_One;

   --EMACSCMD:(progn (forward-line 1)(ada-find-other-file t)(looking-at "   procedure P_Bug$"))
   procedure P_Bug
     (Param_1 : in Integer);
   --EMACSRESULT:t

   -- FIXME: package, function, protected type, task
   -- FIXME: at least one nested

end Ada_Mode.Find_File;
