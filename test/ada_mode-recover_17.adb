-- From a real editing session; used to be noticeably slow
--EMACS_SKIP_UNLESS: (eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
function Slow_Recover_4
  (Header : in Frame_Header;
   Data   : in Ada.Streams.Stream_Element_Array)
  return Frame
is begin
   if Header.Flags_0 /= 0 or Header.Flags_1 /= 0 then
      raise SAL.Not_Implemented with "frame flags not 0";
   end if;

   if Header.ID (1) = 'T' then
      --  [3] section 4.2
      case Data (1) is
         when Text_Encoding_ISO_8859_1 | Text_Encoding_UTF_8 =>
            declare
               Content : String (1 .. Integer (Size (Frame_Head.Size)));
            begin
               String'Read (Stream, Content);
               if (for some C of Content => C = Ascii.Nul) then
                  raise SAL.Not_Supported with "multiple strings";

                  -- error on 'when'; missing stuff. Error recover finds the desired
                  -- solution:
                  --  (insert 'case is'), then later (insert ')); end if; end;')
                  return
                    (Header, To_String_List (
         when Text_Encoding_UTF_16 | Text_Encoding_UTF_16be =>
            raise SAL.Not_Implemented with "UTF-16 string";

         when others =>
            raise SAL.Invalid_Format;
      end case;
   --  Another error here; missing 'end if;'
end Slow_Recover_4;
-- Local Variables:
-- ada-end-name-optional: nil
-- wisi-mckenzie-task-count: 1
-- End:
