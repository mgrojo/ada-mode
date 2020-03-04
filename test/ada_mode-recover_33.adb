-- Parser used to hang in error recovery when mckenzie-task-count = 1; now fixed.

--EMACSCMD:(switch-to-lr1)
procedure Sum_Diff_Lengths
is

begin
   Search
     (Directory => ".",
      Pattern => "*.diff",
      Filter => (Ordinary_File =>
                   Process =>
end Sum_Diff_Lengths;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
