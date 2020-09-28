--  Explores many issues around indenting within parens

with Ada.Text_Io;
package Ada_Mode.Parens is

   -- non-pathological; compare to T3
   type T1 (A : Integer;
            B : Integer)
      is limited record
         V : Character;
         C : Integer;
      end record;

   --EMACSCMD:(progn (end-of-line 3)(ada-in-paramlist-p (syntax-ppss)))
   --EMACSRESULT:nil
   type T3 (A : Integer;
            B : Integer) is limited record
         V : Character;
         -- ada-mode 4.01 had this at ada-indent relative to
         -- type. But this is a pathological case; user should
         -- put 'is limited record' on next line!
         C : Integer;
      end record;

   --  A pathological subprogram declaration. We don't expect ada-format-paramlist to preserve these newlines.
   --EMACSCMD:(progn (end-of-line 4)(ada-in-paramlist-p (syntax-ppss)))
   --EMACSRESULT:t
   function Function_1
     (Param_1,
        Param_2,
        Param_3 : in Ada.Text_Io.
          Count; -- pretending this is wrapped because of line length limit
      Param_4,
        Param_5 : in
        out Integer; -- who would do this!?
      Param_6,
        Param_7
        : in Float
          := 1.0)
     return Float;

   type Array_Type_1 is array (1 .. 3) of Integer;

   function Function_2 (Left, Right : in Array_Type_1) return Array_Type_1;

   procedure Slice;

   --  Test ada-in-paramlist-p in expressions with parens that don't have wisi caches because of a failed parse.
   --EMACSCMD:(progn (end-of-line 3)(delete-char -1)(forward-word -1)(prog1 (ada-in-paramlist-p (syntax-ppss))(end-of-line 1)(insert ";")))
   --EMACSRESULT:nil
   A : Integer := 1 + (5 * 3);

   --  GNAT GPL 2014 accepts this without parens around the enitre
   --  expression, but that's a compiler bug.
   function Expression_Function_1 (V : Integer) return Boolean
     is ((V = V and then True)
           or else True);

end Ada_Mode.Parens;
