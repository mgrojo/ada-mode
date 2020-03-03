
procedure Ada_Mode.Recover_3 is
   function Process return Boolean
   is
      Node_Id : Token_Id renames B;

   begin
      return Node.Id = Id;
   end Process;

   function Process_1 return Boolean
   is
      Node_Id : Token_Id renames B;
   begin
      return Node.Id = Id;
   end Process_1;
begin
end Ada_Mode.Recover_3;
