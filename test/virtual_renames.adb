--  Test that ada_indent_renames_0 handles subp_tok with null region.
-- It no longer crashes, but it does produce random indentation
-- results; there are three solutions found with the same cost, they
-- are chosen randomly (race condition among multiple tasks in error
-- recovery), and they give different indentation results.

--EMACSCMD:(setq skip-reindent-test t)

procedure Put
is
   use Ada.Containers;
   --  In the middle of editing; deleted the name here.
   renames Shared_Parser.Semantic_State.Trace.all;

   use all type Ada.Strings.Unbounded.Unbounded_String;
begin
   null;
end Put;
