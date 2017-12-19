-- Error recovery fails on this, demonstrating the need for a second
-- pass using package and procedure names.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

pragma License (Modified_GPL);

with Ada.Text_IO;
with SAL.Gen_Unbounded_Definite_Queues;
with WisiToken.Token;
package body WisiToken.Parser.LR.McKenzie_Recover is

   procedure Check_One
     (Parser_Label : Natural;
      Config_Store : not null access McKenzie_Recover.Config_Store)
   is
      Data         : McKenzie_Data;
      Config       : Configuration;

      Current_Input : constant Token_ID := Get_Current_Input (Shared_Lookahead, Config);

      New_Config : Configuration;
   begin
      --  Started adding a procedure call
      Config_Store.Get
        (Parser_label

      -- Copied from above, intending to move the function call here.
      Current_Input : constant Token_ID := Get_Current_Input (Shared_Lookahead, Config);

      if Check (Data, Config, Current_Input) then

         return;
      end if;

   end Check_One;

   task type Check_Parser_Config (Config_Store : not null access McKenzie_Recover.Config_Store) is
      entry Start;
      --  Start getting parser/configs to check from Config_Store

      entry Finish;
      --  Terminate; no more configs to check.
   end Check_Parser_Config;

end WisiToken.Parser.LR.McKenzie_Recover;
-- Local variables:
-- wisi-disable-face: t
-- End:
