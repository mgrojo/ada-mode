-- Editing an aggregate used to cause incorrect faces.
--EMACSCMD:(setq skip-recase-test t skip-reindent-test t)
package body Ada_Mode.Incremental_Recover_04 is

   function Factory return WisiToken.Parse.Parser_Access
   is
      --EMACSCMD:(progn (end-of-line 0)(insert " begin"))
      --EMACSCMD:(progn (delete-region (line-beginning-position 3)(line-beginning-position 4)))
      --EMACSCMD:(test-face "Ada.Finalization.Limited_Controlled" nil)
      Result : access WisiToken.Parse.LR.Parser.Parser :=
        new WisiToken.Parse.LR.Parser.Parser'
          (Ada.Finalization.Limited_Controlled with
           User_Data                      => new Parse_Data_Type,
           Table                          => Parse_Table,
           Productions                    => Productions,
           Language_Fixes                 => Language_Fixes,
           Language_Matching_Begin_Tokens => Language_Matching_Begin_Tokens,
           Language_String_ID_Set         => Language_String_ID_Set,
           Partial_Parse_Active           => Partial_Parse_Active,
           Partial_Parse_Byte_Goal        => Partial_Parse_Byte_Goal,
           others                         => <>);
   begin
      Result.Parser.Tree.Lexer := Lexer;
      return WisiToken.Parse.Parser_Access (Result);
   end Factory;
end Ada_Mode.Incremental_Recover_04;
