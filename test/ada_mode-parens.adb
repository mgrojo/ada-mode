package body Ada_Mode.Parens is
   function Function_1
     (Param_1, Param_2 : in Ada.Text_IO. Count;
      Param_3 : in out Integer;
      Param_4 : in Float)
     return Float
   is
      Local_1 : Integer := (1 + 2 + 3);
      Local_2 : Integer := (1 + 2 +
                              3);
      Local_3 : Integer := (1 + 2
                              + 3);
      Local_4 : Integer := (1 +
                              2 + 3);
      Local_5 : Integer :=
        (1 + 2 +
           3);

      Local_6 : String := ("123" & "456" & "789");
      Local_7 : String := ("123" & "456" &
                             "789");
      Local_8 : String := ("123" &
                             "456" & "789");

      Local_9 : String := (
                           "123" &
                             "456" &
                             "789"
                          );

      Local_10 : String :=
        (
         "123" &
           "456" &
           "789"
        );

   begin
      return Float (
                    Integer'Value
                      (Local_6));
   end Function_1;

   function Function_2 (Left, Right : in Array_Type_1) return Array_Type_1
   is begin
      return
        (1 => 1,
         2 =>
          1 + 2 * 3,
         3 => 1 +
           3 * 4,
         others => 5);
   end;

end Ada_Mode.Parens;
