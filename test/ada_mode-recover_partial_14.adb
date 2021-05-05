-- Raised Container_Empty in error recovery; fixed now.
--
-- This file has DOS line endings, to test that case.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

--  wisi-debug 1 is required to see the failure
--EMACSCMD:(setq wisi-debug: 1)

begin
   if Argument_Count = 3 then
      Input_File_Name := Argument (1);
      Gen_Alg         := Wisitoken.Bnf.Generate_Algorithm'Value (Argument (2));
      Trace_Generate  := Integer'Value (Argument (3));
   else
      Put_Usage;
      return;
   end I
exception
   when Constraint_Error =>
      Put_Usage;
