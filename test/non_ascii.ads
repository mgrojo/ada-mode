--  Test non-ASCII source encoding, and ASCII equivalent of non-ASCII characters

package Non_ASIII is
   X1 : Wide_String    := "in ‰"; -- per mille sign
   X2 : Wide_String    := "in €"; -- Euro sign
   X3 : Wide_Character := 'θ';    -- theta

   --  ensure we have the right file character encoding here.
   --
   --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(char-after))
   --EMACSRESULT:#x03c0
   π  : constant       := Pi;     -- from LRM Ada.Numerics

   --  FIXME: lexer doesn't handle these correctly, so parse fails

   X1 : Wide_String := "in ["2030"]";  -- per mille sign
   X2 : Wide_String := "in ["20AC"]";  -- Euro sign
   X3 : Wide_Character := '["03B8"]';  -- theta
   ["03C0"] : constant := Pi;          -- from GNAT Ada.Numerics

   --  End FIXME:

end Non_ASIII;
