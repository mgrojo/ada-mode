--  Tests the variables ada-indent-return and ada-indent-renames
--  The default value for ada-indent-return (the one used in this file)
--  is 0, which means indent on the open parenthesis for the function, or
--  use ada-broken-indent if there is no parenthesis.

--EMACSCMD (setq ada-indent-return 0)
package Function1 is

   ------------------------------------------------------
   --  Use the default value of 0 for ada-indent-return
   ------------------------------------------------------

   function A return Integer;
   function B
     return Integer;   --  from ada-indent-broken

   function C (B : Integer) return Integer;
   function D (B : Integer)
              return Integer;   --  from ada-indent-return

   function E (B : Integer;
               C : Integer) return Integer;
   function F (B : Integer;
               C : Integer)
              return Integer;   --  from ada-indent-return

   type G is access function (C : Integer)
                             return Integer;

   procedure H (B : access function (C : Integer)
                  return Integer);   --  from ?????

   generic
      with function J (B : Integer)
                      return Integer;   --  from ada-indent-return
      with function K
             return Integer;   --  from ada-indent-broken
   package L is
   end L;

   ------------------------------------------------------
   --  Use the default value of 2 for ada-indent-renames
   ------------------------------------------------------

   function AR return Integer renames A;
   function BR
     return Integer   --  from ada-indent-broken
     renames B;  --  from ada-indent-renames

   function CR (B : Integer) return Integer renames C;
   function DR (B : Integer)
               return Integer   --  from ada-indent-return
     renames D; --  from ada-indent-renames

   function ER (B : Integer;
                C : Integer) return Integer renames E;
   function FR (B : Integer;
                C : Integer)
               return Integer   --  from ada-indent-return
     renames F;         --  from ada-indent-renames

end Function1;
