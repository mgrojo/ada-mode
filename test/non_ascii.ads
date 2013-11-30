--  Test non-ASCII source encoding, and ASCII equivalent of non-ASCII characters

with Ada.Numerics;
package Non_ASCII is

   --  Programmer wants this, but must use ASCII equivalents in source:
   X1_Non : Wide_String    := "in ‰"; -- per mille sign
   X2_Non : Wide_String    := "in €"; -- Euro sign

   --  FIXME: gives 'strings are delimited with double quote' with default GNAT options
   --  X3_Non : Wide_Character := 'θ';    -- theta

   --  ensure we have the right file character encoding here.
   --
   -- EMACSCMD:(progn (forward-line 2)(back-to-indentation)(char-after))
   -- EMACSRESULT:#x03c0
   --  FIXME: gives 'illegal char' with default GNAT options
   --   π  : constant       := Pi;     -- from LRM Ada.Numerics

   --  FIXME: lexer doesn't handle these correctly, so parse fails

   X1_ASCII : Wide_String := "in ["2030"]";  -- per mille sign
   X2_ASCII : Wide_String := "in ["20AC"]";  -- Euro sign
   X3_ASCII : Wide_Character := '["03B8"]';  -- theta
   ["03C0"] : constant := Ada.Numerics.Pi;   -- from GNAT Ada.Numerics

   --  End FIXME:

end Non_ASCII;
