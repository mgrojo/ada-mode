--  Test various issues with placeholders

--EMACSCMD:(setq skip-recase-test t)

--  Missing '}'; lexer reports an error for '{', parse error recover inserts ';'.
{hea
procedure Ada_Mode.Placeholders
is begin
   --  placeholder with valid text following
   if {expression} then
      null;

      --  missing '}'; lexer error, but no parse error
   elsif {expression then
      null;
   end if;
end Ada_Mode.Placeholders;
