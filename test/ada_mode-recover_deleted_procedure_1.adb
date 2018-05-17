--  From a real editing session. Fails recover.
procedure Modify_Schema
is
   Server_Config : SAL.Config_Files.Configuration_Type;

   --  Deleted "procedure Get_Root is"
      use SAL.Config_Files;
   begin
      raise SAL.Not_Found with "no root key found";
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
