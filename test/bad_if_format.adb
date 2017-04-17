--  Abstract :
--
--  Document that ada-mode properly indents this if statement,
--  even though it's badly formatted.
procedure Bad_If_Format
is
   procedure A is begin null; end;
   procedure B is begin null; end;
begin

   -- bad format:
   if 1 > 2 then
      null;
   else if 2 > 3 then
         null;
      else if 3 > 4 then
            null;
         else
            null;
         end if;
         A;
      end if;
      B;
   end if;

   --  preferred format:
   if 1 > 2 then
      null;
   else
      if 2 > 3 then
         null;
      else
         if 3 > 4 then
            null;
         else
            null;
         end if;
         A;
      end if;
      B;
   end if;

end Bad_If_Format;
