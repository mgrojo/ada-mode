--  Abstract :
--
--  ada_mode_gps_indent bug #12: comment align with hanging code.
--
procedure Bug_012 is
   A : Integer :=
     --  Comment 1
     42;
   -- Comment 2
begin
   if A
     or B
     -- comment 3
   then
      null;
   end if;

   Put_Flight (Query,
               Slot_Change_Ctrl => (if OBT_Sufficiently_In_The_Future then
                                      All_Changes_Permitted_Respecting_RFI_And_Adpi
                                    else
                                      Slot_Change_Ctrl_Record.No_Changes_Permitted),
               -- Note1 : during several years CASA was not allowed to modify slots.
               -- But this caused too much surprize effect in the DPI processing, so the rule was changed to allow
               -- slot improvements.
               -- Note2 : during several years later, CASA was allowed to suspend a flight due to meteo
               -- (no longer the case) and was allowed to improve the delay (no longer the case except
               --     for RFI flights).
               With_Comment       => Comment,
               Significant_Update => Significant_Update);

end Bug_012;
