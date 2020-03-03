procedure Modify_Schema
is
   procedure Get_Root
   is
      Server_Config : Sal.Config_Files.Configuration_Type;

      --  Deleted "procedure Get_Root is"
      --  restore inserts that before 'Server_Config'.
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
