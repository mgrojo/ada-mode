--EMACSCMD:(setq skip-recase-test t)
with Ada.Text_IO;
package Adacore_9717_001 is
   subtype A_Long_Name
     is Ada.Text_Io.Count;
   use type A_Long_Name;
   subtype Another_Long_Name is
     Ada.Text_Io.Number_Base;
   use type Another_Long_Name;
   subtype X is Integer;
end Adacore_9717_001;
