--  From a real editing session.

-- Recover used to fail on too many enqueues, now finds a good
-- solution in reasonable time.

--EMACSCMD:(setq wisi-indent-region-fallback nil)
package body Ada_Mode.Recover_16 is

   procedure Apply_Rule
   is
   begin
      case Memo.State is
         when Success =>
            if Parser.Head = in -- started typing 'if then end if' here
               return Memo;

            when Failure =>
            return (State => Failure);

         when No_Result =>
            Parser;
      end case;

   end Apply_Rule;


end Ada_Mode.Recover_16;
