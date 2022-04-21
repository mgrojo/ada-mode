-- From a real editing session. With incremental parse, error did not go away.
--EMACSCMD:(setq skip_recase_test t)
package Ada_Mode.Interactive_11 is
   --EMACSCMD:(progn (end-of-line 4)(kill-region (point) (progn (forward-symbol 3)(point)))(delete-char 1))
   --EMACSCMD:(progn (end-of-line 3)(forward-char -1)(wisi-replay-kbd-macro "\ris (Super.Min_Sequential_Indices)")(length (wisi-parser-local-parse-errors wisi-parser-local)))
   --EMACSRESULT: 0
   function Min_Sequential_Index (Super : in Supervisor) return Syntax_Trees.Stream_Node_Parents_Array
     is (Super.Min_Sequential_Indices);
end Ada_Mode.Interactive_11;
