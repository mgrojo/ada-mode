--  aflex inserts 'with <root>_dfa; use <root>_dfa;
with Ada.Text_IO;
with OpenToken.Text_Feeder;
--%%1 aflex inserts package spec start
package Dummy is  -- for indentation

   NULL_IN_INPUT         : exception;
   AFLEX_INTERNAL_ERROR  : exception;
   UNEXPECTED_LAST_MATCH : exception;
   PUSHBACK_OVERFLOW     : exception;
   Aflex_Scanner_Jammed  : exception;

   User_Output_File : Ada.Text_IO.File_Type; -- required by ECHO in yylex, which we don't use

   YY_END_OF_BUFFER_CHAR : constant character := ASCII.NUL;
   yy_n_chars            : integer; -- number of characters read into yy_ch_buf
   yy_eof_has_been_seen  : boolean;

-- UMASS CODES :
   type String_Ptr is access string;
   Saved_Tok_Line1                : String_Ptr := Null;
   Line_Number_Of_Saved_Tok_Line1 : integer    := 0;
   Saved_Tok_Line2                : String_Ptr := Null;
   Line_Number_Of_Saved_Tok_Line2 : integer    := 0;
   Tok_Begin_Line                 : integer    := 1;
   Tok_End_Line                   : integer    := 1;
   Tok_Begin_Col                  : integer    := 0;
   Tok_End_Col                    : integer    := 0;
   Token_At_End_Of_Line           : Boolean    := False;
-- END OF UMASS CODES.

   Feeder : OpenToken.Text_Feeder.Text_Feeder_Ptr;

   type eob_action_type is
     (EOB_ACT_RESTART_SCAN, -- new text in buffer; scan it
      EOB_ACT_END_OF_FILE, -- hit end of file with no unscanned text
      EOB_ACT_LAST_MATCH); -- hit end of file with unscanned text; accept it

   function yy_get_next_buffer return eob_action_type;
   -- try to refill buffer

   function yyWrap return Boolean;

   procedure Open_Input(fname : in String);
   --  Called from yyrestart; raises AFLEX_INTERNAL_ERROR

--%%2 aflex inserts package spec end, body start
end Dummy;
package Dummy is -- for indentation

   procedure YY_INPUT(Buf: out Unbounded_Character_Array; Result: out Integer; Max_Size: in Integer)
   is
      --  Read up to Max_Size characters from text feeder, store in Buf.
      --  Update Result with count characters read.
      --
      --  If text feeder reports end of file, Result is 0.

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
   end YY_INPUT;

--%%1 placeholder used with aflex -I option

   function yy_get_next_buffer return eob_action_type is
      dest           : integer := 0;
      source         : integer := yytext_ptr - 1; -- copy prev. char, too
      number_to_move : integer;
      ret_val        : eob_action_type;
      num_to_read    : integer;
   begin
      if yy_c_buf_p > yy_n_chars + 1 then
         raise NULL_IN_INPUT;
      end if;

      -- try to read more data

      -- first move last chars to start of buffer
      number_to_move := yy_c_buf_p - yytext_ptr;

      for i in 0..number_to_move - 1 loop
         yy_ch_buf(dest) := yy_ch_buf(source);
         dest := dest + 1;
         source := source + 1;
      end loop;

      if yy_eof_has_been_seen then
         -- don't do the read, it's not guaranteed to return an EOF,
         -- just force an EOF

         yy_n_chars := 0;
      else
         --  Leave space for YY_END_OF_BUFFER_CHAR
         num_to_read := YY_BUF_SIZE - number_to_move - 1;

         if num_to_read > YY_READ_BUF_SIZE then
            num_to_read := YY_READ_BUF_SIZE;
         end if;

         YY_Input (YY_Ch_Buf (Number_To_Move .. YY_Ch_Buf'Last), YY_N_Chars, Num_To_Read);
      end if;
      if yy_n_chars = 0 then
         if number_to_move = 1 then
            ret_val := EOB_ACT_END_OF_FILE;
         else
            ret_val := EOB_ACT_LAST_MATCH;
         end if;

         yy_eof_has_been_seen := true;
      else
         ret_val := EOB_ACT_RESTART_SCAN;
      end if;

      yy_n_chars := yy_n_chars + number_to_move;
      yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
      yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

      -- yytext begins at the second character in
      -- yy_ch_buf; the first character is the one which
      -- preceded it before reading in the latest buffer;
      -- it needs to be kept around in case it's a
      -- newline, so yy_get_previous_state() will have
      -- with '^' rules active

      yytext_ptr := 1;

      return ret_val;
   end yy_get_next_buffer;

   -- default yywrap function - always treat EOF as an EOF
   function yywrap return boolean
   is begin
      return true;
   end yywrap;

   procedure Open_Input(fname : in String)
   is begin
      raise Aflex_Internal_Error;
   end Open_Input;

--%%1 aflex inserts package body end
end Dummy;

-- Local Variables:
-- ada-indent-comment-col-0: t
-- end:
