
procedure Ada_Mode.Recover_Extra_End_Loop is
   procedure Find_Node
   is begin
      Iter.Current := Next_Sibling (Iter.Current);

      loop
      end loop;
   end Find_Node;

begin
end Ada_Mode.Recover_Extra_End_Loop;
