package body Ada_Mode.Recover_7 is
   procedure Follow
   is
   begin

      for B in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
      end loop;

      Prev_Result := Result;
      return Result;
   end Follow;

end Ada_Mode.Recover_7;
