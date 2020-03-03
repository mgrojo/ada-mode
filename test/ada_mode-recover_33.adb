-- Parser used to hang in error recovery; now fixed.

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
