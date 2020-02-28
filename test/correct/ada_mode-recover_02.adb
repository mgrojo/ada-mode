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
      Config_Store.Get
        (Parser_Label);

      Current_Input := Get_Current_Input (Shared_Lookahead, Config);

      if Check (Data, Config, Current_Input) then

         return;
      end if;

   end Check_One;

   task type Check_Parser_Config (Config_Store : not null access McKenzie_Recover.Config_Store) is
      entry Start;

      entry Finish;
   end Check_Parser_Config;

end Ada_Mode.Recover_02;
