-- From a real editing session. Found a bug in lexer error handling.

--EMACS_SKIP_UNLESS:wisi-incremental-parse-enable
--EMACSCMD:(switch-to-lr1)
--EMACSCMD:(setq skip-recase-test t)
procedure WisiToken.BNF.Generate_Packrat
is
   use WisiToken.Generate;


   procedure Generate_Parser_Body
   is
   begin
      Indent := Indent + 3;

      for RHS_Index in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
         declare
            RHS : Productions.Right_Hand_Side renames Prod.RHSs (RHS_Index);
         begin
            Indent_Wrap_Comment (Productions.Image (Prod.LHS, RHS_Index, RHS.Tokens, Descriptor), Ada_Comment);
            Indent_Line ("Pos := Last_Pos;");
            Indent_Line ("Next_Pos := Tree.Stream_Next (Tree.Shared_Stream, Pos);");

            if RHS.Tokens.Length = 0 then
               Finish;
            else
               for Token_Index in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                  declare
                     ID      : constant String := Trimmed_Image (RHS.Tokens (Token_Index));
                     Var_Suf : constant String := Var_Suffix (RHS_Index, Token_Index);
                  begin
                     Indent_Line
                       ("Memo_" & Var_Suf & " := Parse_" & Image (RHS.Tokens (Token_Index), Descriptor) &
                          " (Parser, Pos);");

                     --EMACSCMD:(progn (forward-line 2)(forward-char 54)(wisi-replay-kbd-macro "\" & ")(forward-char 2)(wisi-replay-kbd-macro " & \"")(length (wisi-parser-local-parse-errors wisi-parser-local)))
                     --EMACSRESULT: 0
                     Indent_Line ("Update (Parser, Id, Memo_" & Var_Suf & ".Max_Examined_Pos, Max_Examined_Pos);");
                     --EMACSCMD:(progn (forward-line -1)(forward-char 54)(delete-char 4)(forward-char 2)(delete-char 4)(wisi-parse-incremental-none))
                  end;
               end loop;
            end if;

            New_Line;
            Indent_Line ("<<RHS_" & Trimmed_Image (RHS_Index) & "_Fail>>");
         end;
      end loop;

   end Generate_Parser_Body;

begin
   null;
end WisiToken.BNF.Generate_Packrat;
-- Local Variables:
-- ada-end-name-optional: nil
-- End:
