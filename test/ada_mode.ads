package Ada_Mode is
   Global_Exception_1 : exception;

   protected Separate_Protected_Body
   with
     Priority => 5
   is
      entry E;
      procedure P;
      function F return Boolean;
   end;

end Ada_Mode;
