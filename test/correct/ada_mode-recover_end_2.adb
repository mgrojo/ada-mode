package body Ada_Mode.Recover_End_2 is
   procedure Case_Example
   is begin
      case  is
         when A =>
            null;
      end case;
   end Case_Example;

   procedure If_Example
   is begin
      if A > 0 then
         null;
      end if;
   end If_Example;

   procedure Loop_Example
   is begin
      loop
         null;
      end loop;
   end Loop_Example;

   function Return_Example return Integer
   is begin
      return A : Integer := 0 do
         null;
      end return;
   end Return_Example;

end Ada_Mode.Recover_End_2;
