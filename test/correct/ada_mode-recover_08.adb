package body Ada_Mode.Recover_8 is
   procedure Closure
   is
   begin
   For_Each_Production :
      for Prod of Grammar loop
      For_Each_Rhs :
         for B in Prod.Rhss.First_Index .. Prod.Rhss.Last_Index loop
            declare
            begin
               if Prod.Lhs = Rhs.Tokens (Item.Dot) then

                  Beta := Next (Item.Dot);
               end if;
            end;
         end loop For_Each_Rhs;
      end loop For_Each_Production;

   end Closure;

end Ada_Mode.Recover_8;
