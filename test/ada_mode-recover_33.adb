-- Parser used to hang in error recovery when mckenzie-task-count = 1; now fixed.

--EMACSCMD:(switch-to-lr1)
procedure Ada_Mode.Recover_33
is

begin
   Search
     (Directory => ".",
      Pattern => "*.diff",
      Filter => (Ordinary_File =>
                   Process =>
end Ada_Mode.Recover_33;
