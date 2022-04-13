--  Expression used to be indented wrong.
procedure Ada_Mode.Indent_02
is begin
   pragma Assert
     (if Saved_Prev_Terminal = Invalid_Stream_Node_Ref
      then Saved_Prev_Terminal.Element /= Check_Deleted.Element
      else Terminal.Element /= Check_Deleted.Element);
end Ada_Mode.Indent_02;
