procedure Ada_Mode.Recover_Align_1
  (Pattern      : in     Recover_Pattern_1;
   Parser       : in     LR.Instance'Class;
   Parser_State : in out Parser_Lists.Parser_State;
   Error_ID     : in     Token_ID;
   Config       :    out Configuration)
is begin
   if A then B; end if;
end Ada_Mode.Recover_Align_1;
