separate (Ada_Mode) protected body Separate_Protected_Body
  -- no comment before "separate", no newline after )
  --EMACSCMD:(progn (forward-line -3)(ada-find-other-file t)(looking-at "protected body Separate_Protected_Body is"))
is
   entry E when True is
   begin
      null;
   end E;
   procedure P is
   begin
      null;
   end P;
   function F return Boolean is
   begin
      return False;
   end F;
end Separate_Protected_Body;
