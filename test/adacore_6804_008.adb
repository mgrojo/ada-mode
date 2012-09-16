--------------------------------------------------------
--  6804-008: problem for indentation after a task declaration
--  The problem was caused by the task declaration with no
--  block attached
--------------------------------------------------------

package body Adacore_6804_008 is
   task Executive;
   task body Executive is
   begin
      null;
   end Executive;
end Adacore_6804_008; -- line incorrectly indented in adamode 3.4a
