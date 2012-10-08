--  Check for color highlighting for with and use statements.
--  We also test the special case where the preceding comments
--  includes a 'with' keyword. This used to fail.
--
-- FIXME: automate the test; check for face text property

--  with luck
with Toto,
Tutu;
-- Ada mode 4.01 indented "Tutu" with hanging indent. But who would
-- use this style? don't care; it's only here to point out that
-- highlighting fails across newline.
use Toto;

with Ada; use Ada;
with A, B;


--  Now test highlighting of character constants in complex cases

String'('"' & Str & '"');
A'Class'('A');   --  Doesn't mean anything. Whatever.
