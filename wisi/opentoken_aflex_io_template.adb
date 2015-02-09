--  aflex inserts 'with <root>_dfa; use <root>_dfa;
with Ada.Text_IO;
with OpenToken.Text_Feeder;
--%%1 aflex inserts package spec start
package Dummy is  -- for indentation

   NULL_IN_INPUT         : exception;
   AFLEX_INTERNAL_ERROR  : exception;
   UNEXPECTED_LAST_MATCH : exception;
   PUSHBACK_OVERFLOW     : exception;
   AFLEX_SCANNER_JAMMED  : exception;

   User_Output_File : Ada.Text_IO.File_Type; -- required by ECHO in yylex, which we don't use

   YY_END_OF_BUFFER_CHAR : constant Character := ASCII.NUL;
   yy_n_chars            : Integer; -- number of characters read into yy_ch_buf
   yy_eof_has_been_seen  : Boolean;

--  UMASS CODES :
   type String_Ptr is access String;
   Saved_Tok_Line1                : String_Ptr := null;
   Line_Number_Of_Saved_Tok_Line1 : Integer    := 0;
   Saved_Tok_Line2                : String_Ptr := null;
   Line_Number_Of_Saved_Tok_Line2 : Integer    := 0;
   Tok_Begin_Line                 : Integer    := 1;
   Tok_End_Line                   : Integer    := 1;
   Tok_Begin_Col                  : Integer    := 0;
   Tok_End_Col                    : Integer    := 0;
   Token_At_End_Of_Line           : Boolean    := False;
--  END OF UMASS CODES.

   Feeder : OpenToken.Text_Feeder.Text_Feeder_Ptr;

   type eob_action_type is
     (EOB_ACT_RESTART_SCAN, -- new text in buffer; scan it
      EOB_ACT_END_OF_FILE, -- hit end of file with no unscanned text
      EOB_ACT_LAST_MATCH); -- hit end of file with unscanned text; accept it

   procedure YY_Input (Buf : out unbounded_character_array; Result : out Integer; Max_Size : in Integer);
   --  Read up to Max_Size characters from text feeder, store in Buf.
   --  Update Result with count characters read.
   --
   --  If text feeder reports end of file, Result is 0.

   function yywrap return Boolean;

   procedure Open_Input (fname : in String);
   --  Called from yyrestart; raises AFLEX_INTERNAL_ERROR

--%%2 aflex inserts package spec end, body start
end Dummy;
package Dummy is -- for indentation

   procedure YY_Input (Buf : out unbounded_character_array; Result : out Integer; Max_Size : in Integer)
   is
      --  FIXME: change Buf to String to avoid this copy
      Temp      : String (Buf'First .. Buf'First + Max_Size - 1);
      Temp_Last : Integer;
   begin
      if Feeder.End_Of_Text then
         Result := 0;
      else
         Feeder.Get (Temp, Temp_Last);

         if Temp (Temp_Last) = OpenToken.EOF_Character then
            Temp_Last := Temp_Last - 1;
         end if;

         for I in Temp'First .. Temp_Last loop
            Buf (I) := Temp (I);
         end loop;

         Result := Temp_Last - Buf'First + 1;
      end if;
   end YY_Input;

--%%1 placeholder used with aflex -I option

   --  default yywrap function - always treat EOF as an EOF
   function yywrap return Boolean
   is begin
      return True;
   end yywrap;

   procedure Open_Input (fname : in String)
   is begin
      raise AFLEX_INTERNAL_ERROR;
   end Open_Input;

--%%1 aflex inserts package body end
end Dummy;

-- Local Variables:
-- ada-indent-comment-col-0: t
-- end:
