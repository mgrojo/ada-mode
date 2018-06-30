--  From a real editing session; mckenzie fails to find a solution.

--  Changing "for .. of .. loop" to "for .. in .. loop"; forgot to
-- change the 'of'.
--
--  Also missing several "end ...;"
--
--  McKenzie reports many conflicts in Constrain_Terminals; state 42, 421
--
package debug
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
   end Debug;
