--  From a real editing session.
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
procedure Modify_Schema
is
   Server_Config : Sal.Config_Files.Configuration_Type;

      -- Deleted "procedure Get_Root is"
      --
      -- recover either inserts that before 'Server_Config', or ignores the
      -- error, giving different indent results.

      use Sal.Config_Files;
   begin
      raise Sal.Not_Found with "no root key found";
   end Get_Root;
begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count /= 2 then
         Usage;
      end if;
   end;
end Modify_Schema;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
