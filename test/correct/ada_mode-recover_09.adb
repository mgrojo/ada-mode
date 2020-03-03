procedure Ada_Mode.Recover_9 is
   procedure Generate_Parser_Body (Prod : in Productions.Instance)
   is
   begin
      Indent_Line (Parser_Spec (Parser_Name (Prod.Lhs)));
      Indent_Line ("is");
      Indent := Indent + 3;

      Indent_Line ("Pos : Token_Index := Start_Pos;");

      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;

      Indent_Line ("if Pos > Parser.Terminals.Last_Index then");
      Indent_Line ("   return (State => Failure);");
      Indent_Line ("end if;");
      Indent_Line ("case Parser.Derivs (" & Result_Id & ")(Pos).State is");
      Indent_Line ("when Success =>");
      Indent_Line ("   if WisiToken.Trace_Parse > Detail then");

      Indent_Line ("Parser.Trace.Put_Line"
                     ("accept: Memo " & Parser.Tree.Image
                        (Parser.Derivs (Accept_Id)(Start_Pos).Result, Parser.Trace.Descriptor.all,
                         Include_Children => True)));

      Indent_Line ("end if;");
      Indent_Line ("   return Parser.Derivs (" & Result_Id & ")(Pos);");

   end Generate_Parser_Body;

begin
   New_Line;
end Ada_Mode.Recover_9;
