-- Test error recovery from an extra 'end loop'.

-- We get different indent results from partial and incremental parse;
--EMACSCMD:(setq skip-reindent-test (not wisi-incremental-parse-enable))
--EMACSCMD:(setq skip-recase-test t)

procedure Ada_Mode.Recover_Extra_End_Loop is
   procedure Find_Node
   is begin
      Iter.Current := Next_Sibling (Iter.Current);

   end loop;
     -- full parse recovery deletes 'end loop;', incremental restores
     -- 'end', deletes the next 'end'.
     end Find_Node;

begin
end Ada_Mode.Recover_Extra_End_Loop;
-- Local Variables:
-- ada-end-name-optional: nil
-- wisi-mckenzie-task-count: 1
-- End:
