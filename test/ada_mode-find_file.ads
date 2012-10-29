--  Test ada-find-other-file

-- Select package name, goto its spec
--EMACSCMD:(progn (forward-line 1)(forward-word 1)(forward-char 1)(push-mark-command t t)(forward-word 2)(ada-find-other-file t)(pop-mark)(looking-at "Ada_Mode is"))
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
