--  From a real editing session.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

package body Ada_Mode.Recover_Block_Name_Mismatch is
   procedure Find_First
   is begin -- extra 'begin'
      begin
         Match (Middle_Initial_Pat);

      end Find_First;
   -- In this specific case, we would prefer that 'end Find_First;' is
   -- indented the same as 'procedure Find_First'. However, wisi-ada.adb
   -- Insert_After doesn't have enough information, and it is biased
   -- towards typing new code, so we get this indent. Note that if this
   -- comment was before 'end Find_First', we would get the desired
   -- indent.
   procedure Swap_Names
   is
   begin
      null;
   end Swap_Names;

begin
   null;
end Ada_Mode.Recover_Block_Name_Mismatch;
-- Local Variables:
-- ada-end-name-optional: nil
-- End:
