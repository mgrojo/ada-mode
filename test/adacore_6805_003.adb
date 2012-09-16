---------------------------------------------------------
--  6805-003: problem for indentation after a task type declaration
--  The problem was caused by the task type declaration with no
--  block attached
---------------------------------------------------------

package body Adacore_6805_003 is
   task type Cyclic_Task;
   task body Cyclic_Task is
   begin
      null;
   end Cyclic_Task;
end Adacore_6805_003; -- line incorrectly indented in adamode 3.4a

