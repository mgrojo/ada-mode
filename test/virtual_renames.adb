--  Test that ada_indent_renames_0 handles
--  subp_tok with null region

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

procedure Put
is
   use Ada.Containers;
   --  In the middle of editing; deleted the name here.
   renames Shared_Parser.Semantic_State.Trace.all;

   use all type Ada.Strings.Unbounded.Unbounded_String;
begin
   null;
end Put;
