--  other file for testing ada-find-other-file
package body Ada_Mode.Find_File is

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
