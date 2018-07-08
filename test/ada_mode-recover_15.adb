--  From a real editing session; recover used to fail, now finds good solution quickly.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
package body Wisitoken.Lr.Parser is

   overriding procedure Parse (Shared_Parser : aliased in out Lr.Parser.Parser)
   is

   begin

   Main_Loop :
      loop
         loop
            if Current_Parser.Verb = Error then
               Current_Parser.Next;

            elsif Current_Parser.Verb = Current_Verb then
               if Action.Next /= null then

                  if Shared_Parser.Parsers.Count = Shared_Parser.Max_Parallel then
                     declare
                        Max_Parser       : Parser_Lists.Cursor;
                        Cur              : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
                     begin

                        if Max_Recover_Cost > 0 then
                           if Max_Parser = Current_Parser then
                              Parsers.Terminate_Parser (Current_Parser, "too many parsers; max cost");
                           else
                              Parsers.Terminate_Parser (Max_Parser, "too many parsers; max cost");
                           end if;
                        end if;
                     end;

                  end if; -- Inserted "end if;", intending to add "if ... then"

               else
                  Check_Error (Temp);
               end if;
            end if;

         end if;
      end loop; --  recover deletes this 'end loop;'
      end loop Main_Loop;

   end Parse;

end Wisitoken.Lr.Parser;
