--  Test navigation when changing a subprogram name

--EMACSCMD:(setq wisi-indent-region-fallback nil)

package body Ada_Mode.Recover_Change_Name is

   --EMACSCMD:(progn (forward-line 2)(forward-sexp 3)(looking-at "begin -- New_Name_1"))
   --EMACSRESULT:t
   procedure New_Name_1
   is
      A : Integer;
      B : Integer;

   begin -- New_Name_1
      A := B;
      A := B;
   end Old_Name_1;

   --EMACSCMD:(progn (forward-line 2)(forward-sexp 3)(looking-at "begin -- Same_Name_2"))
   --EMACSRESULT:t
   procedure Same_Name_2
   is
      A : Integer;
      B : Integer;

   begin -- Same_Name_2
      A := B;
      A := B;
   end Same_Name_2;

   --EMACSCMD:(progn (forward-line 2)(forward-sexp 3)(looking-at "begin -- New_Name_3"))
   --EMACSRESULT:t
   procedure New_Name_3
   is
      A : Integer;
      B : Integer;
   begin -- New_Name_3
      A := B;
      A := B;
   end Old_Name_3;

end Ada_Mode.Recover_Change_Name;
