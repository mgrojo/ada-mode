package body WisiToken.LR.McKenzie_Recover.Ada is

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
               --  missing 'if .. then'
               --
               --  We don't handle missing 'if' alone; less likely. FIXME: add that
               --  if add 'then' to end_statement_keywords.

               New_Config := Config;
               New_Config.Error_Token.ID := Invalid_Token_ID;
               Push_Back_Check (New_Config, Fast_Forward_Seen, +sequence_of_statements_opt_ID);
               Insert (New_Config, (+IF_ID, +THEN_ID));
                  if Config.Error_Token.ID = +ELSIF_ID then

               Local_Config_Heap.Add (New_Config);
               if Trace_McKenzie > Detail then
                  Put ("Language_Fixes " & Label & Image (Config.Error_Token.ID, Descriptor), New_Config);
               end if;
            end if;
         exception
         when Bad_Config =>
            null;
         end;
      end if;
   end Handle_Parse_Error;

end WisiToken.LR.McKenzie_Recover.Ada;
