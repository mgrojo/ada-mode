-- Recover failed with enqueue_limit. Now finds a solution quickly.
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
-- recover inserts 'case when => if then', indents 'case'.
end if;
end case;
