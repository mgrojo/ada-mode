--  Raised Container_Empty in error recovery; fixed now.

--  wisi-debug 1 is required to see the failure

   begin
      if Argument_Count = 3 then
      Input_File_Name := Argument (1);
      Gen_Alg         := WisiToken.BNF.Generate_Algorithm'Value (Argument (2));
         Trace_Generate  := Integer'Value (Argument (3));
      else
         Put_Usage;
         return;
end i
   exception
   when Constraint_Error =>
      Put_Usage;
-- Local Variables:
-- wisi-debug: 1
-- End:
