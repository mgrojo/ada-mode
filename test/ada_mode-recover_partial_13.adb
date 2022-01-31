-- LR1 recover used to fail on enqueue limit. Incremental parse
-- requires a higher enqueue_limit than full parse.
--
-- FIXME: add separate minimal_complete queue.
--EMACSCMD: (switch-to-lr1)
begin
   if Use_Minimal_Complete_Actions then
      if 0 < Insert_Minimal_Complete_Actions (Super, Shared, Parser_Index, Config, Local_Config_Heap) then
         if Matching_Begin_Token /= in;
         end if;
         --  Local Variables:
         --  wisi-mckenzie-enqueue-limit: 70000
         --  End:
