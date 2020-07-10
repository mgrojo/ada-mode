procedure Sum_Diff_Lengths
is
begin
   Search
     (Directory => ".",
      Pattern => "*.diff",
      Filter => (Ordinary_File => True),
      Process => Process_File'Access);
end Sum_Diff_Lengths;
