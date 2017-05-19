with YYLex;
with ada_syntax_io;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada_Tokens;
procedure Run_Ada_Lexer
is
   Token : Ada_Tokens.Token;
   pragma Unreferenced (Token);
begin
   Open (ada_syntax_io.user_input_file, In_File, Argument (1));

   loop
      exit when End_Of_File (ada_syntax_io.user_input_file);
      Token := YYLex;
   end loop;
exception
when E : others =>
   if Is_Open (ada_syntax_io.user_input_file) then
      Put_Line (Count'Image (Line (ada_syntax_io.user_input_file)) & ": " & Ada.Exceptions.Exception_Message (E));
   else
      Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   end if;

end Run_Ada_Lexer;
