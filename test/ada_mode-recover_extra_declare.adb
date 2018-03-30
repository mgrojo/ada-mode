-- Test error recovery from an extra 'declare'.
-- Does not compile.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

procedure Ada_Mode.Recover_Extra_Declare is
begin

   loop
   declare

      Next_Token (Lexer, Token);
      case Name (Token) is
         when End_Of_File =>
            return;

         when Bad_Token =>
            if not Ignore_HTML_Syntax then
               Syntax_Error ("HTML syntax error " & Lexeme (Token), Token);
            end if;

         when Start_Tag_Opener =>
            Parse_Tag (Lexer);

         when others =>
            null;

      end case;
   end loop;

end Ada_Mode.Recover_Extra_Declare;
