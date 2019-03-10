--  Raised Constraint_Error. Now fixed.
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
end if;
end if;
if Deleted_Token.First and Token_Index = Data.Terminals.Last_Index then
   --  Deleted_Token.Line is now blank; add to previous token non grammar.
   if Prev_Token.First_Trailing_Comment_Line = Invalid_Line_Number then
      Prev_Token.First_Trailing_Comment_Line := Deleted_Token.Line;
