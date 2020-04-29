-- Example from real code.
--
--EMACS_SKIP_UNLESS: (eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
package body Ada_Mode.Recover_24 is

   function Fast_Forward return Non_Success_Status
   is
   begin
      for Item of Parse_Items loop
         declare
            Parsed_Config : Configuration renames Item.Config;
         begin
            if Parsed_Config.Current_Insert_Delete = No_Insert_Delete then
               raise Programmer_Error;

            else
               if Parsed_Config.Insert_Delete (Parsed_Config.Current_Insert_Delete).Token_Index =
                 Parsed_Config.Current_Shared_Token
               then

      end loop; -- error; should be 'end if;' - recover finishes 'if', 'declare'.

      Parsed_Config.Ops.Append ((Fast_Forward, Config.Current_Shared_Token));
      Local_Config_Heap.Add (Parsed_Config);
      end if;
      -- now this is an extra 'end if;', expecting 'end loop;'
      -- recover deletes 'if', matching this with 'fuction fast_forward'
   end;
end loop;
return Abandon;
end Fast_Forward;

end Ada_Mode.Recover_24;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
