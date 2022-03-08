-- Used to get CONSTRAINT_ERROR in recover.
--EMACSCMD:(switch-to-lr1)
--EMACSCMD:(progn (forward-line 1)(forward-symbol 2)(delete-char -1)(indent-for-tab-command))
procedure Ada_Mode.Recover_Incremental_02 is
   --EMACSCMD:(progn (forward-line -1)(forward-symbol 2)(insert "e")(indent-for-tab-command))

   pragma Assert
     (if Saved_Prev_Terminal = Invalid_Stream_Node_Ref
      then Saved_Prev_Terminal.Element /= Check_Deleted.Element
      else Terminal.Element /= Check_Deleted.Element);

end Ada_Mode.Recover_Incremental_02;
