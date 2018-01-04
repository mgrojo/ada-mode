--  generated by WisiToken Wisi from wisi_grammar.wy
--  with command line: wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer re2c --interface process --enum
--  wisi_grammar.wy
--

with Interfaces.C;
with WisiToken;
with System;
package wisi_grammar_re2c_c is

   function New_Lexer
     (Buffer    : in System.Address;
      Length    : in Interfaces.C.size_t;
      Verbosity : in Interfaces.C.int)
     return System.Address
   with Import        => True,
        Convention    => C,
        External_Name => "wisi_grammar_new_lexer";
   --  Create the lexer object, passing it the full text to process.

   procedure Free_Lexer (Lexer : in out System.Address)
   with Import        => True,
        Convention    => C,
        External_Name => "wisi_grammar_free_lexer";
   --  Free the lexer object

   procedure Reset_Lexer (Lexer : in System.Address)
   with Import        => True,
        Convention    => C,
        External_Name => "wisi_grammar_reset_lexer";

   function Next_Token
     (Lexer         : in     System.Address;
      ID            :    out WisiToken.Token_ID;
      Byte_Position :    out Interfaces.C.size_t;
      Byte_Length   :    out Interfaces.C.size_t;
      Char_Position :    out Interfaces.C.size_t;
      Char_Length   :    out Interfaces.C.size_t;
      Line_Start    :    out Interfaces.C.int)
     return Interfaces.C.int
   with Import        => True,
        Convention    => C,
        External_Name => "wisi_grammar_next_token";

end wisi_grammar_re2c_c;
