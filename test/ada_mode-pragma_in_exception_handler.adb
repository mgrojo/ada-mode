-- From some libadalang-tools code; the pragmas in the exception
-- handler used to confuse ada-mode navigation.

procedure Ada_Mode.Pragma_In_Exception_Handler is

   procedure Per_File_Action
   is

      procedure Write_File_Name_File is
      begin
         null;
      exception
         pragma Warnings (Off);
         when Constraint_Error =>
            raise;

            --  This pragma is either part of the sequence_of_statements following
            -- 'when Constraint_Error', or part of the exception_handler list. The
            -- parser chooses one interpretation randomly, so the indent can
            -- change when the grammar or parser does.
         pragma Warnings (On);
      end Write_File_Name_File;

   begin
      Write_File_Name_File;
   end Per_File_Action;
begin
   null;
end Ada_Mode.Pragma_In_Exception_Handler;
