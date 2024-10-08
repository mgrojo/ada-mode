--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
package Ada_Mode.Expression_Functions is

   function Square (A : in Float) return Float
     is (A * A);

   --  Yes, this looks a little weird, but that's due to putting the
   --  argument list on the same line as the function name; see Fun2
   --  below, and/or setting ada-indent-return to 0.
   function Fun1 (Really_Really_Long_Argument_List : Boolean)
                 return Boolean -- a Really_Really_Long_Return_Type
     is
     (True) -- a Really_Really_Long_expression
   with Convention => Ada;

   --  Simple fix for the above
   function Fun2
     (Really_Really_Long_Argument_List : Boolean)
     return Integer -- a Really_Really_Long_Return_Type
     is (1) -- a Really_Really_Long_expression
   with Convention => Ada;

   --  comment after expression_function - was broken
   type Foo_Type is (A, B, C, D);

   --  Don't require extra parens for case expressions
   function Symbol (Foo : in Foo_Type) return String is
     (case Foo is
         when A => "Ok ",
         when B | C => "Err",
         when D => "Unk");

   --EMACSCMD:(test-all-defs "type Result")
   --EMACSRESULT:(unless (eq ada-xref-backend 'eglot) (list (list "ada_mode-expression_functions.ads" (concat "Result " (cl-ecase ada-xref-backend (gpr_query "record type")(gnat "spec"))))))
   type Result is record
      A : Integer;
      B : Integer;
   end record;

   --EMACSCMD:(test-all-defs "return Result")
   --EMACSRESULT:(list (list "ada_mode-expression_functions.ads" (cl-ecase ada-xref-backend (eglot "   type Result is record")(gpr_query "Result record type")(gnat "Result spec")))))
   --  Don't require extra parens for aggregate result in expression function
   function Key return Result is
     (A => 1,
      B => 2);

   Y : array (1 .. 42) of Integer := (others => 0);

   --  Indent after =>
   function F1 return Boolean is
     (for some X of Y =>
        X /= 0);

   function F2 return Boolean is (for some X of Y =>
                                    X /= 0);

   function F3 return Boolean is (for some X of Y
                                    =>
                                      X /= 0);

end Ada_Mode.Expression_Functions;
--Local Variables:
--ada-eglot-gpr-file: nil
--End:
