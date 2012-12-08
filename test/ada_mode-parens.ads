--  Explores all issues around indenting within parens
with Ada.Text_IO;
package Ada_Mode.Parens is

   -- non-pathological; compare to T3
   type T1 (A : Integer;
            B : Integer)
      is limited record
         V : Character;
         C : Integer;
      end record;

   --EMACSCMD:(progn (end-of-line 3)(ada-in-paramlist-p))
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
   function Function_1
     (Param_1,
        Param_2,
        Param_3 : in Ada.Text_IO.
        Count; -- pretending this is wrapped because of line length limit
      Param_4,
        Param_5 : in
        out Integer; -- who would do this!?
      Param_6
        : in Float
        := 1.0)
     return Float;

   type Array_Type_1 is array (1 .. 4) of Integer;

   function Function_2 (Left, Right : in Array_Type_1) return Array_Type_1;

end Ada_Mode.Parens;
