--  Used to encounter a BAD_CONFIG in ada-mode language_fixes.

procedure Ada_Mode.Recover_46
is
   use Syntax_Trees;

   Tree         : Syntax_Trees.Tree renames Shared_Parser.Tree;
   Parse_Table  : in Wisitoken.Parse.Lr.Parse_Table;
   Parser_Label : constant Syntax_Trees.Stream_Id := Super.Stream (Parser_Index);

   procedure Put (Message : in String; Config : in Configuration)
   is begin
      Put (Message, Super.Trace.all, Tree, Parser_Label, Config);
   end Put;

   End_Name_Token   : constant Recover_Token := Recover_Stacks.Peek
     (Config.Stack, Config.In_Parse_Action_Token_Count - Config.In_Parse_Action_Status.End_Name + 1).Token;
begin
   null;
end Ada_Mode.Recover_46;
