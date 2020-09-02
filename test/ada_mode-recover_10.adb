--  From a real editing session.
--
-- Recover used to fail to find a solution; now it does.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
package body Ada_Mode.Recover_10 is
   procedure Check_Rhs_Order
     (Grammar          : in     Wisitoken.Productions.Prod_Arrays.Vector;
      Source_File_Name : in     String;
      Error_Count      : in out Natural)
   is begin
      for Prod of Grammar loop
         declare
         begin
            --  forgot to change 'of' to 'in'
            for I of Prod.Rhss.First_Index + 1 .. Prod.Rhss.Last_Index loop
               declare
                  Prev : Token_Id_Arrays.Vector renames Prod.Rhss (I - 1).Tokens;
                  Cur : Token_Id_Arrays.Vector renames Prod.Rhss (I).Tokens;
               begin

                  --  missing several 'end ...'
               end Check_Rhs_Order;
   end Ada_Mode.Recover_10;
