with OpenToken.Token.Analyzer;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Separator;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.List_Mixin;
package Token_List_Test is

   type Token_IDs is (Int, Comma, EOF, Whitespace);

   package Master_Token is new OpenToken.Token
     (Token_IDs, Token_IDs'First, Token_IDs'Last, Token_IDs'Image);
   package Tokenizer is new Master_Token.Analyzer;
   package Token_List is new Master_Token.List_Mixin (Master_Token.Instance, Master_Token.Instance);

   Syntax : constant Tokenizer.Syntax :=
     (Int        => Tokenizer.Get (OpenToken.Recognizer.Integer.Get),
      Comma      => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (",")),
      EOF        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),
      Whitespace => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get
           (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   Analyzer : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax);

   String_Feeder : aliased OpenToken.Text_Feeder.String.Instance;

end Token_List_Test;
