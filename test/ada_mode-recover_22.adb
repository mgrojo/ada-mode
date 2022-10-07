-- Example from real code.
--
--EMACSCMD:(setq skip-recase-test t)
package body Ada_Mode.Recover_22 is

   procedure Handle_Parse_Error
   is
   begin
      if Ada_Process_Actions.Token_Enum'(-Config.Error_Token.ID) in +ELSE_ID | +ELSIF_ID then
         declare
            Label         : constant String := "missing 'if then' ";
            New_Config    : Configuration;
            Matching_Index : SAL.Peek_Type := 1;
         begin
            Find_ID (Config, +IF_ID, Matching_Index);
            if Matching_Index = Config.Stack.Depth then

               New_Config := Config;
               New_Config.Error_Token.ID := Invalid_Token_ID;
               Push_Back_Check (New_Config, Fast_Forward_Seen, +sequence_of_statements_opt_ID);
               Insert (New_Config, (+IF_ID, +THEN_ID));
               if Config.Error_Token.ID = +ELSIF_ID then

                  --  Missing 'end if;' here

                  Local_Config_Heap.Add (New_Config);
                  if Trace_McKenzie > Detail then
                     Put ("Language_Fixes " & Label & Image (Config.Error_Token.ID, Descriptor), New_Config);
                  end if;
               end if;

               -- Error detected here; 'end if;' inserted before 'exception', so
               -- indent of blank line is correct for another statement.
            exception
            when Bad_Config =>
               null;
         end;
      end if;
   end Handle_Parse_Error;

end Ada_Mode.Recover_22;
