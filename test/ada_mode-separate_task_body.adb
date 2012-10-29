separate (Ada_Mode)
task body Separate_Task_Body is
   -- no comment before "separate"
   --EMACSCMD:(progn (forward-line -3)(ada-find-other-file t)(looking-at "task Separate_Task_Body is"))

   procedure Titi is begin null; end;
   procedure Shout is begin null; end;
   A : Boolean := False;
begin
   begin  -- nothing between this 'begin' and the previous; should be indented ada-indent.
      accept Please_Abort;
      null;
   end;

   --  test indenting asynchronous_select
   select
      Please_Abort;
   then abort
      Titi;
      --  'Titi' indented with ada-indent
   end select;

   select
      Please_Abort;
   then
     abort
      -- 'abort' indented with ada-broken-indent, since this is part
      --  of a "select then abort" statement. Comment indented with
      --  ada-indent.

      Titi;
      -- 'Titi' indented with ada-indent, as is this comment.
   end select;


   if A
   then
      Shout;
      --  indented with ada-indent, since not part of "then"
      --  statement.

   end if;

   if A
   then
      abort Separate_Task_Body;
      --  indented with ada-indent, since not part of "select then abort"
      --  statement.
   elsif A then
      abort Separate_Task_Body;
   end if;

end Separate_Task_Body;
