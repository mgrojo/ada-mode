--  Tests the variables ada-indent-return and ada-indent-renames.
--  The default value for ada-indent-return (the one used in this file)
--  is 0, which means indent on the open parenthesis for the function, or
--  use ada-indent-broken if there is no parenthesis.

--EMACSCMD: (setq ada-indent-broken 3)
--EMACSCMD: (setq ada-indent-return 0)
--  <= 0 indents relative to the open parenthesis
--EMACSCMD: (setq ada-indent-renames 2)
--  > 0 indents relative to the "function" or "procedure" keyword
package Ada_Mode.Options.Indent_Return_1 is

   function A return Integer;
   function B
      return Integer;   -- from ada-indent-broken (or ada-indent-return if > 0)

   function C (B : Integer) return Integer;
   function D (B : Integer)
              return Integer;   -- from ada-indent-return

   function E (B : Integer;
               C : Integer) return Integer;
   function F (B : Integer;
               C : Integer)
              return Integer;   -- from ada-indent-return

   type G is access function
                       return Integer; -- from ada-indent-broken/return

   type H is access function (C : Integer)
                             return Integer;  -- from ada-indent-return

   procedure I (B : access function (C : Integer)
                                    return Integer);  -- from ada-indent-return

   generic
      with function J (B : Integer)
                      return Integer;   --  from ada-indent-return
      with function K
              return Integer;   --  from ada-indent-broken/return
   package L is
   end L;

   --  Renaming

   function AR return Integer renames A;
   function BR
      return Integer   --  from ada-indent-broken/return
     renames B;  --  from ada-indent-broken (or ada-indent-renames if > 0)

   function CR (B : Integer) return Integer renames C;
   function DR (B : Integer)
               return Integer   --  from ada-indent-return
     renames D; --  from ada-indent-renames

   function ER (B : Integer;
                C : Integer) return Integer renames E;
   function FR (B : Integer;
                C : Integer)
               return Integer
     renames F;  --  from ada-indent-renames
   overriding function FO (B : Integer;
                           C : Integer)
                          return Integer   -- from ada-indent-return
                renames F;  -- from ada-indent-renames

   procedure P;
   procedure PR
     renames P;  -- from ada-indent-broken/renames

   procedure Q (X : Integer);
   procedure QR (X : Integer)
     renames Q;  -- from ada-indent-renames
   overriding procedure QO (X : Integer)
                renames Q;  -- from ada-indent-renames

end Ada_Mode.Options.Indent_Return_1;
