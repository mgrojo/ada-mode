-- Recover fails with enqueue_limit; handle that cleanly.
--
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

begin
   New_Config.Check_Status := (Label => WisiToken.Semantic_Checks.Ok);
   Insert_Count            := Insert_Count + 1;

   Do_Shift
     (Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, Action.Id,
      Cost_Delta,
      Strategy   => Minimal_Complete);
end;

end if;
end case;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- wisi-debug: 0
-- End:
