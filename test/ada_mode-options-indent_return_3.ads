--  Similar to ada_mode-options-indent_return_1.ads, except with a
--  different value of ada-indent-return and ada-indent-renames.

--EMACSCMD:(setq skip-recase-test t)

package Ada_Mode.Options.Indent_Return_3 is

   function A return Integer;
   function B
      return Integer;   -- from ada-indent-broken (or ada-indent-return if > 0)

   function C (B : Integer) return Integer;
   function D (B : Integer)
                return Integer;   --  from ada-indent-return

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

   -- see ada_mode-nominal-child.ads for 'overriding function ... renames'

   procedure P;
   procedure PR
      renames P;  -- from ada-indent-broken/renames

   procedure Q (X : Integer);
   procedure QR (X : Integer)
                renames Q;  -- from ada-indent-renames

   -- see ada_mode-nominal-child.ads for 'overriding procedure ... renames'

end Ada_Mode.Options.Indent_Return_3;
-- Local Variables:
-- ada-indent-broken: 3
-- ada-indent-return: -2
-- ada-indent-renames: 0
-- End:
