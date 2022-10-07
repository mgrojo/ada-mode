-- LR1 recover fails on enqueue limit with default enqueue limit, works with 100000.
--
-- FIXME: add separate minimal_complete queue.
--EMACS_SKIP_UNLESS: nil
--EMACSCMD: (switch-to-lr1)
begin
   if Use_Minimal_Complete_Actions then
      if 0 < Insert_Minimal_Complete_Actions (Super, Shared, Parser_Index, Config, Local_Config_Heap) then
         if Matching_Begin_Token /= in;
      end if;
