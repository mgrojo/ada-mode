--  From a real editing session.

procedure Ada_Mode.Recover_Constant_As_Expression_1
is begin
   loop

      if Next_Marker = Sof0 or
        Sof5  : constant Stream_Element := 16#C5#;
      --  Copied a list of constant declarations to construct a boolean
      -- expression. Desired solution: delete ': constant Stream_Element'.

   end loop;
exception
   when  Sal.Invalid_Format =>
      raise;
end Ada_Mode.Recover_Constant_As_Expression_1;
