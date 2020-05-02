-- Real editing example. Error recovery used to fail, now it finds a
-- reasonable solution quickly.
--
-- Correct solution:
-- insert ');' after 'Parser_Label'
-- delete ': constant Token_ID' after 'Current_Input'
--
-- Error recovery finds a different reasonable solution.

--EMACSCMD:(setq skip-recase-test t)

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

pragma License (Modified_GPL);

with Ada.Text_IO;
with SAL.Gen_Unbounded_Definite_Queues;
with WisiToken.Token;
package body Ada_Mode.Recover_02 is

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
        (Parser_Label

   -- Copied from above, intending to move the function call here.
   -- recover finishes Check_One before this comment.
   Current_Input : constant Token_ID := Get_Current_Input (Shared_Lookahead, Config);

   -- recover inserts 'begin; here
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

end Ada_Mode.Recover_02;
-- Error recovery has a race condition; force it to return repeatable results
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
