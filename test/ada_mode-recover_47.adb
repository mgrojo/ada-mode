--  Used to get CONSTRAINT_ERROR in recover.
--EMACSCMD:(setq skip-reindent-test t skip-recase-test t)
--EMACSCMD:(switch-to-lr1)
--EMACSCMD:(progn (forward-line 1)(forward-symbol 2)(delete-char -1)(indent-for-tab-command))
procedure Debug is

   pragma Assert
     (if Saved_Prev_Terminal = Invalid_Stream_Node_Ref
    then Saved_Prev_Terminal.Element /= Check_Deleted.Element
    else Terminal.Element /= Check_Deleted.Element);

end Debug;
