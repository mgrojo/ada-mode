-- From a real editing session. Completes the function spec
-- before the newline; we want the newline is inside the parameter list.
--
-- FIXME: with both partial and incremental parse, recover solution is:
-- ((PUSH_BACK, SEMICOLON, 0), (INSERT, RIGHT_PAREN, 0), (INSERT, RETURN, 0), (INSERT, IDENTIFIER, 0))
-- and the newline is after 0:';', so outside the parameter list.
--
package Ada_Mode.Recover_41 is
   --EMACSCMD:(progn (end-of-line 5) (execute-kbd-macro "\r")(current-column))
   --EMACSRESULT: 3
   function Sled_Rental_Valid
     (Line : in String;
      Low, High : in Integer;

   --EMACSCMD:(progn (forward-line -2)(kill-line 1))
end Ada_Mode.Recover_41;
