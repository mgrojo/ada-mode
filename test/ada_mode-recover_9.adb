-- From a real editing session; converting code into generated code.
--
-- throws Constraint_Error in McKenzie_Recover; too many ops in a config.
procedure Wisi.Generate_Packrat_Parser
   procedure Generate_Parser_Body (Prod : in Productions.Instance)
   is
   begin
      Indent_Line (Parser_Spec (Parser_Name (Prod.LHS)));
      Indent_Line ("is");
      Indent := Indent + 3;

      Indent_Line ("Pos : Token_Index := Start_Pos;");

      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;

      Indent_Line ("if Pos > Parser.Terminals.Last_Index then");
      Indent_Line ("   return (State => Failure);");
      Indent_Line ("end if;");
      Indent_Line ("case Parser.Derivs (" & Result_ID & ")(Pos).State is");
      Indent_Line ("when Success =>");
      Indent_Line ("   if WisiToken.Trace_Parse > Detail then");
            Indent_Line ("Parser.Trace.Put_Line
              ("Accept: memo " & Parser.Tree.Image
                 (Parser.Derivs (Accept_ID)(Start_Pos).Result, Parser.Trace.Descriptor.all, Include_Children => True));
         end if;
      Indent_Line ("   return Parser.Derivs (" & Result_ID & ")(Pos);");

   end Generate_Parser_Body;

begin
   New_Line;
end Wisi.Generate_Packrat_Parser;
