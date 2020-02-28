procedure Ada_Mode.Recover_20
is
begin
   if (Indenting_Token.ID = -Expression_Opt_ID and
       (Prev_1 = -With_ID and
          (Prev_3 = Invalid_Token_ID or
             Prev_3 /= Left_Paren_ID)))
     or
     ()
   then
   end if;

end Ada_Mode.Recover_20;
