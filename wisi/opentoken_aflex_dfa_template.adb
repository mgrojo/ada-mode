--%%1 aflex inserts package spec start, aflex_debug
package Dummy is  -- for indentation

   yytext_ptr : Integer; -- points to start of yytext in buffer


   YY_READ_BUF_SIZE : Integer;
   --  It's not clear why we have yy_read_buf_size; aflex sets it to
   --  0.5 * yy_buf_size

   YY_BUF_SIZE : Integer;
   --  size of input buffer; must be large enough to hold largest
   --  token (typically comments)

   type unbounded_character_array is array (Integer range <>) of Character;
   type Unbounded_Character_Array_Access is access Unbounded_Character_Array;

   yy_ch_buf : Unbounded_Character_Array_Access;

   procedure Set_Buffer_Size (Size : in Integer);

   yy_cp, yy_bp : Integer;

   --  yy_hold_char holds the character lost when yytext is formed
   yy_hold_char : Character;
   yy_c_buf_p : Integer;   -- points to current character in buffer

   function YYText return String;
   function YYLength return Integer;
   procedure YY_DO_BEFORE_ACTION;
   --  These variables are needed between calls to YYLex.
   yy_init : Boolean := True; -- do we need to initialize YYLex?
   yy_start : Integer := 0; -- current start state number
   subtype yy_state_type is Integer;
   yy_last_accepting_state : yy_state_type;
   yy_last_accepting_cpos : Integer;

   procedure yyless (n : Integer);
   --  return all but the first 'n' matched characters back to the input stream

--%%1 aflex inserts package spec end
end Dummy;

with Ada.Unchecked_Deallocation;

--%%1 aflex inserts package body start
package Dummy is -- for indentation

   procedure Set_Buffer_Size (Size : in Integer)
   is
      procedure Free is new Ada.Unchecked_Deallocation (Unbounded_Character_Array, Unbounded_Character_Array_Access);
   begin
      --  We want to read the entire input file on the first call to yy_get_next_buffer.
      YY_READ_BUF_SIZE := Size;
      YY_BUF_SIZE      := Size;
      Free (yy_ch_buf);

      --  yy_ch_buf is 2 characters longer than YY_BUF_SIZE for 2 end-of-buffer characters.
      YY_Ch_Buf := new unbounded_character_array (0 .. Size + 1);
   end Set_Buffer_Size;

   function YYText return String is
      J : Integer := yytext_ptr;
   begin
      while J <= yy_ch_buf'Last and then yy_ch_buf (J) /= ASCII.NUL loop
         J := J + 1;
      end loop;

      declare
         subtype Sliding_Type is String (1 .. J - yytext_ptr);
      begin
         return Sliding_Type (yy_ch_buf (yytext_ptr .. J - 1));
      end;
   end YYText;

   --  Returns the length of the matched text

   function YYLength return Integer is
   begin
      return yy_cp - yy_bp;
   end YYLength;

   --  Done after the current pattern has been matched and before the
   --  corresponding action - sets up yytext

   procedure YY_DO_BEFORE_ACTION is
   begin
      yytext_ptr := yy_bp;
      yy_hold_char := yy_ch_buf (yy_cp);
      yy_ch_buf (yy_cp) := ASCII.NUL;
      yy_c_buf_p := yy_cp;
   end YY_DO_BEFORE_ACTION;

   procedure yyless (n : Integer) is
   begin
      yy_ch_buf (yy_cp) := yy_hold_char; -- undo effects of setting up yytext
      yy_cp := yy_bp + n;
      yy_c_buf_p := yy_cp;
      YY_DO_BEFORE_ACTION; -- set up yytext again
   end yyless;

--%%1 aflex inserts package body end
end Dummy;

-- Local Variables:
-- ada-indent-comment-col-0: t
-- end:
