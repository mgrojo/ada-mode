-- Test error recovery from an extra 'end loop'.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

procedure Ada_Mode.Recover_Extra_End_Loop is
   procedure Find_Node
   is begin
      Iter.Current := Next_Sibling (Iter.Current);

      end loop; --  recovery deletes 'loop'.
   end Find_Node;

begin
end Ada_Mode.Recover_Extra_End_Loop;
