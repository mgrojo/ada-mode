-- Used to get CONSTRAINT_ERROR in error recover, due to bug in Ada language fixes.

procedure Gen_Run_Wisi_LR_Text_Rep_Parse
is
   Parse_Data_Template : aliased Parse_Data_Type;
begin
   WisiToken.Trace_Memory            := 1;
   WisiToken.Trace_Incremental_Parse := 1;
   GNATCOLL.Memory.Configure
     (Activate_Monitor      => True,
      Stack_Trace_Depth     => 0,
      Reset_Content_On_Free => False);

   declare
      Trace : WisiToken.Text_IO_Trace.Trace;
      Lexer : constant WisiToken.Lexer.Handle := Create_Lexer;
      --  No point in reporting lexer memory; it's very small
      Parse_Table : constant WisiToken.Parse.LR.Parse_Table_Ptr := Create_Parse_Table;
   begin
      Trace.Put_Line ("parse table created");
      WisiToken.Report_Memory (Trace);

   Run_Wisi_Common_Parse.Parse_File
     ((Descriptor, Create_Lexer, Create_Parse_Table
         (Ada.Directories.Containing_Directory (Ada.Command_Line.Command_Name) & "/" & Text_Rep_File_Name),
       Partial_Parse_Active, Partial_Parse_Byte_Goal, Language_Fixes, Language_Matching_Begin_Tokens,
       Language_String_ID_Set, Parse_Data_Template'Unchecked_Access));
end Gen_Run_Wisi_LR_Text_Rep_Parse;
