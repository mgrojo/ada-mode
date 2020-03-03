package body Ada_Mode.Recover_10 is
   procedure Check_Rhs_Order
     (Grammar          : in     Wisitoken.Productions.Prod_Arrays.Vector;
      Source_File_Name : in     String;
      Error_Count      : in out Natural)
   is begin
      for Prod of Grammar loop
         declare
         begin
            for I in Prod.Rhss.First_Index + 1 .. Prod.Rhss.Last_Index loop
               declare
                  Prev : Token_Id_Arrays.Vector renames Prod.Rhss (I - 1).Tokens;
                  Cur : Token_Id_Arrays.Vector renames Prod.Rhss (I).Tokens;
               begin

               end;
            end loop;
         end;
      end loop;
   end Check_Rhs_Order;
end Ada_Mode.Recover_10;
