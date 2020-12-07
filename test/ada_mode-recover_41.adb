-- From a real editing session. Was completing the function spec
-- before the newline; now the newline is inside the parameter list.
package debug is
   --EMACSCMD:(progn (end-of-line 5) (execute-kbd-macro "\n")(current-column))
   --EMACSRESULT: 6
   function Sled_Rental_Valid
     (Line : in String;
      Low, High : in Integer;

      --EMACSCMD:(progn (forward-line -2)(kill-line 1))
end debug;
