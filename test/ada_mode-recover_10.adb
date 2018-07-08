--  From a real editing session; mckenzie fails to find a solution.

-- Missing several "end ...;"
--
-- McKenzie reports many conflicts in Constrain_Terminals; state 42, 421
--
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
package Ada_Mode.Recover_10
procedure Check_RHS_Order
     (Grammar          : in     WisiToken.Productions.Prod_Arrays.Vector;
      Source_File_Name : in     String;
      Error_Count      : in out Natural)
   is begin
      for Prod of Grammar loop
         declare
         begin
         for I of Prod.RHSs.First_Index + 1 .. Prod.RHSs.Last_Index loop
         declare
         Prev : Token_ID_Arrays.Vector renames Prod.RHSs (I - 1).Tokens;
         Cur : Token_ID_Arrays.Vector renames Prod.RHSs (I).Tokens;
         begin
         end Check_RHS_Order;
   end Ada_Mode.Recover_10;
