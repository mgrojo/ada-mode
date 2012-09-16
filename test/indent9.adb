
--  7318-006: indentation problems after select..then abort


procedure Indent9 is
begin

   select
      Toto;
   then abort
      Titi;  --  Should be indented with ada-indent, instead of
             --  ada-broken-indent
   end select;


   select
      Toto;
   then
     abort    -- indented with ada-broken-indent, since this is
              --  part of a "then abort" statement after a select
       Titi;
   end select;


   if A
   then
      Shout  --  indented with ada-indent, since not part of "then"
             --  statement.
   end if;
end Indent9;
