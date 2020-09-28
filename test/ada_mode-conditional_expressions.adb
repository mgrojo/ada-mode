--  This sets ada-indent-comment-gnat t in Local Variables below.

procedure Ada_Mode.Conditional_Expressions is
   subtype Bounded is Integer range -1 .. +1;
   J : Integer := 42;

   function Foo (I : in Integer) return Integer
   is begin
      return I;
   end Foo;

   function Foo_2 (I, J : in Integer) return Integer
   is begin
      return I;
   end Foo_2;

   K0a : Integer := Integer'(if J > 42 then -1 else +1);
   K0b : Integer := Integer'((if J > 42 then -1 else +1));

   K1 : Integer := (if J > 42 then -1
                    --  comment
                    else +1 -
                      2);
   K2 : Integer := (if J > 42
                    --  comment
                    then
                       1 +
                         2
                    else
                       1 +
                         2);
   K2a : Integer :=
     (if J > 42
      --  comment; compare to K3
      then -1
      else +1);
   K2b : Integer :=
     (if J >
        42
        --  comment; matches last line of preceding expression
      then -1
      else +1);

   K3 : Integer := (if
                      J > 42
                      --  comment; compare to K2a
                    then Foo
                      (-1)
                    else
                       Foo
                         (+1));

   K : Integer := K0a;
   L0 : Integer :=
     (case J is when 42 => -1, when Integer'First .. 41 => 0, when others => 1);
   L1 : Integer := (case J is
                       when 42 => -1,
                       -- comment aligned with 'when'
                       --EMACSCMD:(progn (forward-line 2)(forward-word 2)(downcase-word 1)(wisi-case-adjust)(let ((case-fold-search nil))(looking-back "'First")))
                       --EMACSCMD:(progn (forward-line 1)(forward-word 2)(upcase-word 1)(wisi-case-adjust)(let ((case-fold-search nil))(looking-back "'First")))
                       when Integer'First .. 41 => 0,
                       --EMACSRESULT:t
                       when others => +1);
   L2 : Integer := (case J is
                       when
                         42 => -1,
                       when
                         Integer'First .. 41 => 0,
                       when
                         others => +1);
   L3 : Integer := (case J is
                       when 42 =>
                          --  Comment aligned with "-1"
                          -1 +
                            2,
                       when Integer'First .. 41 =>
                          0 -
                            1,
                       when others =>
                          +1);
   L4 : Integer := (case J is
                       when
                         42
                         =>
                          -1 +
                            2,
                       when
                         Integer'First .. 41
                         =>
                          0,
                       when
                         others
                         =>
                          +1);

   type C_Type is (A, B, Z);
   C : C_Type := A;
   M : Boolean := True;
   function Fun (I : in Integer) return Integer is (I);

   L5 : Boolean :=
     (case C is
         when A =>
            J = 4
              or else M, --  test case from Piotr Trojanek
         when B =>
            Fun (J) = 0
              or else M,
         when others =>
           (1
              + 2) = 3);

   L : Integer := L0;
begin
   K := (if K < 0 then 42 elsif K = 0 then 43 else
           (if J > 42 then 44 else 45));
   K := (case Bounded (L) is when -1 => 42, when 0 => 41, when 1 => 43);
   --  embedded case
   --EMACSCMD: (progn (forward-line 4)(forward-word 2)(delete-char 2)(ada-align))
   K := (if K < 0 then 42
         elsif K = 0 then
           (case J is
               when 42                  => -1,
               when Integer'First .. 41 => 0,
               when others              => +1)
         else 44);
   --  embedded if with comment
   --EMACSCMD: (progn (forward-line 4)(forward-word 2)(delete-char 2)(ada-align))
   K :=
     (case Bounded (K) is
         when -1 => 42, -- '=>' aligned with next line
         when 0  => 41,
         when 1  =>
            -- comment indent matching GNAT style check
            -- second line of comment
           (if J > 42

            then 44     -- comment matching GNAT
                        -- second line

            else 45)); -- comment _not_ matching GNAT style check
                        -- comment matching GNAT

   K :=
     (if M
      then Foo_2
        (1,
         2)
      -- comment
      elsif M
      then
         1 +
           1
      else
         1
           + 2);

end Ada_Mode.Conditional_Expressions;
--  Local Variables:
--  ada-indent-comment-gnat: t
--  End:
