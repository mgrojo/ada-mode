procedure Ada_Mode.Recover_Constant_As_Statement_1
is begin
   loop

      if Next_Marker = Sof0 then
         Sof5 := 16#C5#;
      end if;

   end loop;
exception
   when  Sal.Invalid_Format =>
      raise;
end Ada_Mode.Recover_Constant_As_Statement_1;
