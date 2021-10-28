-- From a real editing session. Recover deleted 'end ; else', so
-- indent was not computed for those lines.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

Term.Last_Trailing_Comment_Line  := Deleted_Term.Last_Trailing_Comment_Line;
end;
else
   if not Data.Terminals (Op.Token_Index + 1).First then

      -- Local Variables:
      -- wisi-mckenzie-task-count: 1
      -- End:
