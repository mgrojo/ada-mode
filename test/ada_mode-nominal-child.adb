--  Test ada-find-other-file and a few other things
package body Ada_Mode.Nominal.Child is

   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(ada-find-other-file nil)(looking-at "overriding function Function_2a"))
   --EMACSRESULT:t
   overriding function Function_2a (Param : in Child_Type_1) return Float
   is
      pragma Unreferenced (Param);
   begin
      --EMACSCMD:(progn (forward-line 2)(forward-word 1)(ada-find-other-file nil)(looking-at "overriding function Function_2a"))
      --EMACSRESULT:t
      return 0.0;
   end Function_2a;

end Ada_Mode.Nominal.Child;
