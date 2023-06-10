--  (first comment part of test) We provide Base_Tree and Tree in one package, because only Tree
--  needs an API; the only way Base_Tree is accessed is via Tree.

-- Recover encountered Bad_Config, because ada-wisi.el
-- wisi-expand-region did not ignore 'package' in the comment above;
-- there is no code begin point between point and bob.

--EMACSCMD:(wisi-fontify-region (progn (forward-line)(search-forward "pragma")(point)) (point-max))

pragma License (Modified_Gpl);

with Ada.Finalization;
with Sal.Gen_Unbounded_Definite_Vectors;
with Wisitoken.Lexer;
package Wisitoken.Syntax_Trees is

   type Base_Tree is new Ada.Finalization.Controlled with private;

   type Base_Tree_Access is access all Base_Tree;

   overriding procedure Finalize (Tree : in out Base_Tree);
   --  Free any allocated storage.

   overriding procedure Adjust (Tree : in out Base_Tree);
   --  Copy any allocated storage.

   type Tree is tagged private;
   -- Local Variables:
   -- wisi-partial-parse-threshold: 0
   -- End:
