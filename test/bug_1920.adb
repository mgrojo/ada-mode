--  -*- ada-label-indent: -4 -*-

--  http://debbugs.gnu.org/cgi/bugreport.cgi?bug=1920

procedure Bug_1920 is
begin
   if True then
      null;
  Label1:
      begin
         null;
     <<Label2>>   -- ada-label-indent was ignored here
         if False then
            null;    -- then-part was not correctly indented
         else
            null;
         end if;
      end Label1;
   end if;
end Bug_1920;

