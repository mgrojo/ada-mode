--  Test various issues with placeholders

--EMACSCMD:(setq skip-recase-test t)

--  Missing '}'
{hea
procedure Ada_Mode.Placeholders
is begin
   --  placeholder with valid text following
   if {expression} then

      --  missing '}'; 'then' not seen
   elsif {expression then

   end if;
end Ada_Mode.Placeholders;
