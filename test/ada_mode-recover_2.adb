-- Real editing example. Error recovery used to fail, now it finds a
-- reasonable solution quickly.
--
-- Correct solution:
-- insert ');' after 'Parser_Label'
-- delete ': constant Token_ID' after 'Current_Input'
--
-- Error recovery finds (insert '); return') after 'Parser_Label',
-- turning the object declaration into an extended return statement.

--EMACSCMD:(setq skip-recase-test t)

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

pragma License (Modified_GPL);

with Ada.Text_IO;
with SAL.Gen_Unbounded_Definite_Queues;
with WisiToken.Token;
package body Ada_Mode.Recover_2 is

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

   end Ada_Mode.Recover_2;
