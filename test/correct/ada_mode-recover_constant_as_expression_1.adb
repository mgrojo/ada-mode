procedure Ada_Mode.Recover_Constant_As_Expression_1
is begin
   loop

      if Next_Marker = Sof0 or 16#C5# then
      end if;

   end loop;
exception
   when  Sal.Invalid_Format =>
      raise;
end Ada_Mode.Recover_Constant_As_Expression_1;
