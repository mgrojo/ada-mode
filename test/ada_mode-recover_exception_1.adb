--  From a real editing session.

--EMACSCMD:(setq skip-recase-test t)
package body Ada_Mode.Recover_Exception_1 is

   function All_Frames (File : in SMM.ID3.File) return Frame_Lists.List
   is
   begin
      File_Header'Read (Stream, File_Head);

   exception
      when E : SAL.Not_Implemented | SAL.Invalid_Format =>
         declare
            use Ada.Exceptions;
            -- Missing 'begin'
            Raise_Exception (Ada.Exceptions.Exception_ID (E), Name (File.Stream) & ": " &
                               Ada.Exceptions.Exception_Message (E));
         -- Missing 'end;'
   end All_Frames;

end Ada_Mode.Recover_Exception_1;
-- Local Variables:
-- ada-end-name-optional: nil
-- End:
