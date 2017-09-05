--  Test indent after recover from syntax error; missing ';' (and/or rest of extended return statement).
--  Does not compile.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

function Find_Path  return Path
is
begin
   return Result : Path (1 .. Result_Length)
end Find_Path;
