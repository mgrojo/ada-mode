--  From a real editing session. Now finds a reasonable solution quickly.
--
procedure Ada_Mode.Recover_03 is
   function Process return Boolean
   is
      Node_Id : Token_Id renames
   begin
      return Node.Id = Id;
   end Process;

   function Process_1 return Boolean
   is
      Node_Id : Token_Id
        -- missing '<identifier> ;' after renames. There are two solutions
        -- here; 'delete renames, insert ;' and 'insert <identifier> ;'. They
        -- give different indents; one is chosen at random, which makes the
        -- indent change with small changes in the error recover code.

        renames begin
      return Node.Id = Id;
   end Process_1;

   --  missing 'begin null;'
end Ada_Mode.Recover_03;
-- Local Variables:
-- ada-end-name-optional: nil
-- End:
