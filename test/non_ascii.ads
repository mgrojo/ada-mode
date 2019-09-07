--  Test non-ASCII source encoding, and ASCII equivalent of non-ASCII characters

--EMACSCMD:(wisi-prj-select-cached "ada_mode-no-gpr.adp" (ada-prj-default))
with Ada.Numerics;
package Non_ASCII is

   --  Programmer wants this, but must use ASCII equivalents in source:
   X1_Non : Wide_String    := "in ‰"; -- per mille sign
   X2_Non : Wide_String    := "in €"; -- Euro sign
   X3_Non : Wide_Character := 'θ';    -- theta

   --  ensure we have the right file character encoding here (UTF-8).
   --  pi is lowercase; case exception in case-exceptions-non_ascii
   --
   --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(char-after))
   --EMACSRESULT:#x03c0
   π_non : constant := 3.14159;

   --  Uppercase pi; auto-casing works for non-ASCII
   Π_Upper_Non : constant := 3.14159;


   --  The bracket source character coding used here is not defined in
   --  the Ada Reference Manual, but is supported by GNAT (and
   --  hopefully other compilers).

   X1_ASCII : Wide_String := "in ["2030"]";  -- per mille sign
   X2_ASCII : Wide_String := "in ["20AC"]";  -- Euro sign
   X3_ASCII : Wide_Character := '["03B8"]';  -- theta

   ["03c0"]_ASCII : constant := 3.14159;

end Non_ASCII;
