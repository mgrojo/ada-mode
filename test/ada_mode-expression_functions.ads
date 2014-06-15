package Ada_Mode.Expression_Functions is

   function Square (A : in Float) return Float
     is (A * A);

   --  comment after expression_function - was broken
   type Foo_Type is (A, B, C, D);

   --  Don't require extra parens for case expressions
   function Symbol (Foo : in Foo_Type) return String is
     (case Foo is
         when A => "Ok ",
         when B | C => "Err",
         when D => "Unk");
end Ada_Mode.Expression_Functions;
