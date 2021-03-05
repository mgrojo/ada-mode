--  From a real editing session.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

package body Ada_Mode.Recover_Block_Name_Mismatch is
   procedure Find_First
   is begin -- extra 'begin'
   begin
      Match (Middle_Initial_Pat);

   end Find_First;
   procedure Swap_Names
   is
   begin
   end Swap_Names;

begin
end Ada_Mode.Recover_Block_Name_Mismatch;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- ada-end-name-optional: nil
-- End:
