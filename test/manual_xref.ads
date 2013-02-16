--EMACSCMD:(setq skip-recase-test t)

--  Testing cross-references when the identifier names include a word
--  that can be a keyword or an operator ("mod", "and", ...)
--
--  Try doing cross-references on each of the identifiers below, you should
--  go from the private part to the public one, and back.

package MANUAL_XREF is
   MODAL : constant INTEGER;
   AND_ANOTHER : constant INTEGER;
   ANDANOTHER : constant INTEGER;
   PRIVATE_UNIT : constant INTEGER;
   PRIVATEUNIT : constant INTEGER;

private
   MODAL : constant INTEGER := 5;
   AND_ANOTHER : constant INTEGER := 5;
   ANDANOTHER : constant INTEGER := 5;
   PRIVATE_UNIT : constant INTEGER := 5;
   PRIVATEUNIT : constant INTEGER := 5;
end MANUAL_XREF;
