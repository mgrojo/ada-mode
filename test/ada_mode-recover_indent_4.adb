-- Real editing that encountered an error recovery failure. Fixed by
-- adjusting cost of insert left-paren.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
package body WisiToken.Wisi_Runtime is

   function Current_Indent_Offset
     (Data         : in Parse_Data_Type;
      Anchor_Token : in Semantic_State.Augmented_Token;
      Offset       : in Integer)
     return Integer
   is
      --  [2] compute delta in wisi-elisp-parse--anchored-1.

      Line_Begin_Pos : constant Buffer_Pos :=
        (if Anchor_Token.First and
           Anchor_Token.First_Indent_Line = Anchor_Token.Line
         then Anchor_Token.Char_Region.First
         else Data.Semantic_State.All_Tokens
           (Data.Semantic_State.Find_Line_Begin
              --  Error here; missing left paren; converting from package.function
              -- (object, args) to objec.function (args)
              Anchor_Token.Line, Anchor_Token)).Char_Region.First);
   begin
      return Offset + Integer (Anchor_Token.Char_Region.First - Line_Begin_Pos);
   end Current_Indent_Offset;


end WisiToken.Wisi_Runtime;
--  Local Variables:
--  wisi-disable-face: t
--  End:
