procedure Ada_Mode.Selected_Component
is begin

   Ada.
     Text_Io.Put_Line;

   Ada.Text_Io
     .Put_Line;

   Recover_Op_Arrays.Variable_Ref
     (Parser_State.Recover_Insert_Delete,
      Recover_Op_Arrays.Last_Index (Parser_State.Recover_Insert_Delete))
     .Ins_Tree_Node := Parser_State.Current_Token;

   Foo (A).
     B;
end Ada_Mode.Selected_Component;
