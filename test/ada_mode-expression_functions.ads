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

   Y : array (1 .. 42) of Integer := (others => 0);

   --  Indent after =>
   function F return Boolean is
     (for some X of Y =>
        X /= 0);

end Ada_Mode.Expression_Functions;
