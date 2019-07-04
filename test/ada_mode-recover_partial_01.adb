-- From a real editing session. Recover deletes 'end ; else', so
-- indent was not computed for those lines.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

Term.Last_Trailing_Comment_Line  := Deleted_Term.Last_Trailing_Comment_Line;
   end; -- recover inserts 'if then begin' here, indents for 'end'
else
   if not Data.Terminals (Op.Token_Index + 1).First then
