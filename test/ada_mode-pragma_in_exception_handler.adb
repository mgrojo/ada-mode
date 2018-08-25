package body Pp.Actions is

   procedure Per_File_Action
   is

      procedure Write_File_Name_File is
      begin
      exception
         pragma Warnings (Off);
         when Lock_Error =>
            raise;
         pragma Warnings (On);
      end Write_File_Name_File;

   begin
      Write_File_Name_File;
   end Per_File_Action;

end Pp.Actions;
