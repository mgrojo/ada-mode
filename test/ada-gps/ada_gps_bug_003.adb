--  Abstract :
--
--  ada_mode_gps_indent bug #3: comments inside expressions badly indented
--
--
procedure Bug_003 is
   --  Comment after 'procedure'; aligned with following 'function'

   function External_Message_Received (Action : Action_Information_T) return Boolean is
   begin
      return
        Action.Event in RPL_Load .. ATC_CPR
        or Action.Event in AO_Slot_Missed .. AO_Update_Readiness -- line 5: bug #2
        or Action.Event in CDM.DPI_Event_T
        or Action.Event in CDM.API_Event_T
        or Action.Event = Individual_Confirmation
        or Action.Event = Op_Cancel
        or Action.Event = Op_Individual_Confirmation
        or Action.Event = Op_Modify_Excluded_Regulation_List
        or Action.Event = Op_Modify_Exempt_Status
        or Action.Event = External_Create

        -- when Event = External_Create then ...
        or Action.Event = Change_EOBT
        -- when Event = Change_Eobt then we need ...
        or Action.Event = Op_Change_Manual_Suspension
        or Action.Event = Update_Measure_Related_CDM_Data;
   end External_Message_Received;
begin

   --  Comment after blank line; aligned with following 'A'

   A := 1;

   --  Comment after blank line; aligned with preceding 'A'
end Bug_003;
