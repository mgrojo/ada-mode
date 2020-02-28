package body Ada_Mode.Recover_16 is

   procedure Apply_Rule
   is
   begin
      case Memo.State is
         when Success =>
            if Parser.Head in B then
               return Memo;
            end if;
         when Failure =>
            return (State => Failure);

         when No_Result =>
            Parser;
      end case;

   end Apply_Rule;


end Ada_Mode.Recover_16;
