-- Test non-ASCII source encoding, and ASCII equivalent of non-ASCII
-- characters.

--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))

-- Selecting the project changes ada-syntax-propertize-hook, but does
-- not clear the syntax cache (see note in ada-prj-select-compiler for
-- why). So we do that here.
--EMACSCMD:(syntax-ppss-flush-cache (point-min))

--EMACSCMD:(setq skip-recase-test t)
with Ada.Numerics;
package Non_ASCII is

   X1_Non : Wide_String    := "in ‰"; -- per mille sign

   X2_Non : Wide_String    := "in €"; -- Euro sign

   X3_Non : Wide_Character := 'θ';    -- theta

   --  ensure we have the right file character encoding here (UTF-8).
   --  pi is lowercase; case exception in case-exceptions-non_ascii
   --
   --EMACSCMD:(test-face "3.14159" font-lock-constant-face)
   --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(char-after))
   --EMACSRESULT:#x03c0
   π_non : constant := 3.14159;
   --EMACSCMD:(progn (forward-line -1)(forward-word) (nth 2 (wisi-prj-identifier-at-point (project-current))))
   --EMACSRESULT: "π_non"

   -- Uppercase pi; test auto-casing for non-ASCII
   --EMACSCMD:(progn (forward-line 2)(back-to-indentation)(downcase-word 3)(wisi-case-adjust)(let ((case-fold-search nil))(looking-back "Π_Upper_Non")))
   --EMACSRESULT: t
   Π_Upper_Non : constant := 3.14159;
   --EMACSCMD:(progn (forward-line -1)(forward-word) (nth 2 (wisi-prj-identifier-at-point (project-current))))
   --EMACSRESULT: "Π_Upper_Non"


   --  The bracket source character coding used here is not defined in
   --  the Ada Reference Manual, but is supported by GNAT (and
   --  hopefully other compilers).

   X1_ASCII : Wide_String := "in ["2030"]";  -- per mille sign
   X2_ASCII : Wide_String := "in ["20AC"]";  -- Euro sign
   X3_ASCII : Wide_Character := '["03B8"]';  -- theta

   ["03c0"]_ASCII : constant := 3.14159;
   --EMACSCMD:(progn (forward-line -1)(forward-word) (nth 2 (wisi-prj-identifier-at-point (project-current))))
   --EMACSRESULT: "[\"03c0\"]_ASCII"

   --EMACSCMD:(progn (forward-line 3)(back-to-indentation)(insert "θ"))
   --EMACSCMD:(progn (end-of-line 3)(forward-word -1)(kill-word 1)(insert "0"))
   --EMACSCMD:(progn (forward-line 1)(forward-word) (nth 2 (wisi-prj-identifier-at-point (project-current))))
   π_non_1 : constant := 3.14159;
   --EMACSRESULT:"θπ_non_1"
   --EMACSCMD:(progn (end-of-line -1)(forward-word -2) (looking-at "3.0"))
   --EMACSRESULT:t

   -- Undo changes so diff test passes
   --EMACSCMD:(progn (forward-line -6)(kill-line 1)(insert "   π_non_1 : constant := 3.14159;\n"))

end Non_ASCII;
--Local Variables:
--ada-eglot-gpr-file: nil
--End:
