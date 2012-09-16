--EMACSCMD: (setq ada-stmt-end-indent 3)
with Ada.Text_Io;
package Adacore_9717_001 is
   subtype A_Long_Name
      is Ada.Text_Io.Count; --  ada-stmt-end-indent
   use type A_Long_Name;
   subtype Another_Long_Name is
     Ada.Text_Io.Number_Base; --  ada-broken-indent
   use type Another_Long_Name;
   subtype X is Integer;
end Adacore_9717_001;
