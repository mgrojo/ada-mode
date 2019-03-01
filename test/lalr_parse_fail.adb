-- The LALR process parser throws a constraint error due to a bad state after
-- reduce on this; the lr1 parser succeeds.

-- FIXME: fix the parser generator
--EMACS_SKIP_UNLESS:nil

Start := Ada.Real_Time.Clock;

for I in 1 .. Cl_Params.Repeat_Count loop
end loop;
-- end of file.
