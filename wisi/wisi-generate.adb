--  Abstract :
--
--  Parser for Wisi grammar files, producing Ada or Elisp source
--  files for a parser.
--
--  Copyright (C) 2012 - 2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Wisi.Generate_Utils;
with Wisi.Output_Ada;
with Wisi.Output_Ada_Common;
with Wisi.Output_Ada_Emacs;
with Wisi.Output_Elisp;
with Wisi.Output_Elisp_Common;
with WisiToken.Generate.Packrat;
with WisiToken.LR.LALR_Generate;
with WisiToken.LR.LR1_Generate;
with WisiToken.LR.Parser_No_Recover;
with WisiToken.Productions;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Grammar_Runtime;
with Wisi_Grammar_Actions;
with Wisi_Grammar_Main;
procedure Wisi.Generate
is
   use all type Standard.Ada.Containers.Count_Type;

   procedure Put_Usage
   is
      use Standard.Ada.Text_IO;
   begin
      --  verbosity meaning is actually determined by output choice;
      --  they should be consistent with this description.
      Put_Line (Standard_Error, "wisi-generate [options] {wisi grammar file}");
      Put_Line (Standard_Error, "version 0.00 - experimental");
      Put_Line (Standard_Error, "Generate source code implementing a parser for the grammar.");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "The following grammar file directives control parser generation:");
      Put_Line (Standard_Error,
                "%generate <algorithm> <output language> [<lexer>] [<interface>]");
      Put_Line (Standard_Error, "   specify one of each generate parameter. May be repeated.");
      Put_Line (Standard_Error, "   algorithm: LALR | LR1 | Packrat_Gen | Packrat_Proc");
      Put_Line (Standard_Error, "   output language: Ada | Ada_Emacs | Elisp");
      Put_Line (Standard_Error, "   interface: interface Process | Module");
      Put_Line (Standard_Error, "      only valid with Ada_Emacs:");
      Put_Line (Standard_Error, "      Process is for an external subprocess communicating with Emacs.");
      Put_Line (Standard_Error, "      Module  is for a dynamically loaded Emacs module.");
      Put_Line (Standard_Error, "   lexer: re2c | Elisp");
      New_Line (Standard_Error);
      Put_Line (Standard_Error, "options are:");
      Put_Line (Standard_Error, "  -v level: sets verbosity (default 0):");
      Put_Line (Standard_Error, "     0 - only error messages to standard error");
      Put_Line (Standard_Error, "     1 - add diagnostics to standard out");
      Put_Line (Standard_Error, "     2 - more diagnostics to standard out, ignore unused tokens, unknown conflicts");
      Put_Line (Standard_Error, "  --suffix <string>; appended to grammar file name");
      Put_Line (Standard_Error,
                "  --test_main; generate standalone main program for running the generated parser, modify file names");
      Put_Line (Standard_Error, "  --time; output execution time of various stages");

   end Put_Usage;

   Language_Name         : Standard.Ada.Strings.Unbounded.Unbounded_String; -- The language the grammar defines
   Output_File_Name_Root : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Suffix                : Standard.Ada.Strings.Unbounded.Unbounded_String;
   Test_Main             : Boolean := False;

   Command_Generate_Set : Generate_Set_Access; -- override grammar file declarations

   Trace          : aliased WisiToken.Text_IO_Trace.Trace (Wisi_Grammar_Actions.Descriptor'Access);
   Input_Data     : aliased WisiToken.Wisi_Grammar_Runtime.User_Data_Type;
   Grammar_Parser : WisiToken.LR.Parser_No_Recover.Parser;

   Do_Time : Boolean := False;

   procedure Use_Input_File (File_Name : in String)
   is
      use Standard.Ada.Strings.Unbounded;
      use Standard.Ada.Text_IO;
   begin
      Output_File_Name_Root := +Standard.Ada.Directories.Base_Name (File_Name) & Suffix;

      Wisi_Grammar_Main.Create_Parser
        (Parser    => Grammar_Parser,
         Trace     => Trace'Unchecked_Access,
         User_Data => Input_Data'Unchecked_Access);

      Grammar_Parser.Lexer.Reset_With_File (File_Name);

      declare
         Language_Name_Dir   : constant Integer := Standard.Ada.Strings.Fixed.Index
           (File_Name, Standard.Ada.Strings.Maps.To_Set ("/\"), Going => Standard.Ada.Strings.Backward);
         Language_Name_Ext   : constant Integer := Standard.Ada.Strings.Fixed.Index (File_Name, ".wy");
      begin
         Language_Name := +Wisi.Output_Elisp_Common.Elisp_Name_To_Ada
           (File_Name
              ((if Language_Name_Dir = 0
                then File_Name'First
                else Language_Name_Dir + 1) ..
                 Language_Name_Ext - 1),
            Append_ID => False,
            Trim      => 0);
      end;
   exception
   when Name_Error | Use_Error =>
      raise Name_Error with "input file '" & File_Name & "' could not be opened.";
   end Use_Input_File;

begin
   declare
      use Standard.Ada.Command_Line;
      Arg_Next : Integer := 1;
   begin
      loop
         exit when Argument (Arg_Next)(1) /= '-';

         --   -v first, then alphabetical

         if Argument (Arg_Next) = "-v" then
            Arg_Next  := Arg_Next + 1;
            WisiToken.Trace_Generate := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--generate" then
            Arg_Next  := Arg_Next + 1;
            declare
               Quad : Generate_Quad;
            begin
               begin
                  Quad.Gen_Alg := Generate_Algorithm'Value (Argument (Arg_Next));
                  Arg_Next     := Arg_Next + 1;
               exception
               when Constraint_Error =>
                  raise User_Error with "invalid value for generator_algorithm: '" & Argument (Arg_Next) & ";";
               end;
               begin
                  Quad.Out_Lang := Output_Language'Value (Argument (Arg_Next));
                  Arg_Next  := Arg_Next + 1;
               exception
               when Constraint_Error =>
                  raise User_Error with "invalid value for output_language: '" & Argument (Arg_Next) & ";";
               end;

               begin
                  Quad.Lexer := To_Lexer (Argument (Arg_Next));
                  Arg_Next   := Arg_Next + 1;
                  begin
                     Quad.Interface_Kind := Interface_Type'Value (Argument (Arg_Next));
                     Arg_Next := Arg_Next + 1;
                  exception
                  when Constraint_Error =>
                     --  interface not specified
                     null;
                  end;
               exception
               when User_Error =>
                  --  lexer not specified
                  null;
               end;

               Add (Command_Generate_Set, Quad);
            end;

         elsif Argument (Arg_Next) = "--suffix" then
            Arg_Next := Arg_Next + 1;
            Suffix   := +Argument (Arg_Next);
            Arg_Next := Arg_Next + 1;

         elsif Argument (Arg_Next) = "--test_main" then
            Arg_Next  := Arg_Next + 1;
            Test_Main := True;

         elsif Argument (Arg_Next) = "--time" then
            Arg_Next := Arg_Next + 1;
            Do_Time  := True;

         else
            raise User_Error with "invalid argument '" & Argument (Arg_Next) & "'";
         end if;
      end loop;

      Use_Input_File (Argument (Arg_Next));

      if Arg_Next /= Argument_Count then
         raise User_Error with "arg count" & Integer'Image (Argument_Count) &
           " different from expected count" & Integer'Image (Arg_Next);
      end if;
   end;

   begin
      Grammar_Parser.Parse;
   exception
   when WisiToken.Syntax_Error =>
      Grammar_Parser.Put_Errors (Input_Data.Grammar_Lexer.File_Name);
      raise;
   end;

   declare
      use all type WisiToken.Unknown_State_Index;
      use all type Standard.Ada.Strings.Unbounded.Unbounded_String;
      use Standard.Ada.Text_IO;

      --  Create a .parse_table file unless verbosity > 0
      Parse_Table_File : File_Type;

      Generate_Done : Lexer_Generate_Algorithm_Set := (others => (others => False));
      Generate_Set  : Generate_Set_Access;

      Parse_Done : Lexer_Set := (others => False);
      Lexer_Done : Lexer_Set := (others => False);

      procedure Parse_Check (Lexer : in Lexer_Type)
      is begin
         Input_Data.User_Lexer := Lexer;
         --  Specifying the lexer can change the parsed grammar, due to %if lexer.

         Input_Data.Reset;
         Grammar_Parser.Execute_Actions;

         if not Input_Data.If_Lexer_Present then
            --  Parse cannot depend on lexer - mark them all done.
            Parse_Done := (others => True);
         else
            Parse_Done (Input_Data.User_Lexer) := True;
         end if;

         if Input_Data.Rule_Count = 0 or Input_Data.Tokens.Rules.Length = 0 then
            raise WisiToken.Grammar_Error with "no rules";
         end if;

      end Parse_Check;

   begin
      if Command_Generate_Set = null then
         --  get quad from input file
         Parse_Check (None);

         if Input_Data.Generate_Set = null then
            raise User_Error with
              WisiToken.Generate.Error_Message
                (Input_Data.Grammar_Lexer.File_Name, 1,
                 "generate algorithm, output_language, lexer, interface not specified");
         end if;
         Generate_Set := Input_Data.Generate_Set;
      else
         Generate_Set := Command_Generate_Set;
      end if;

      for Quad of Generate_Set.all loop

         if Quad.Lexer = None then
            case Quad.Out_Lang is
            when Ada | Ada_Emacs =>
               Input_Data.User_Lexer := re2c_Lexer;
            when Elisp =>
               Input_Data.User_Lexer := Elisp_Lexer;
            end case;
         else
            Input_Data.User_Lexer := Quad.Lexer;
         end if;

         if not Parse_Done (Input_Data.User_Lexer) then
            Parse_Check (Input_Data.User_Lexer);
         end if;

         declare
            use Standard.Ada.Real_Time;

            Time_Start : Time;
            Time_End   : Time;

            Generate_Data : aliased Wisi.Generate_Utils.Generate_Data := Wisi.Generate_Utils.Initialize
              (Input_Data.Grammar_Lexer.File_Name, Input_Data.Tokens, -Input_Data.Generate_Params.Start_Token);

            Packrat_Data : constant WisiToken.Generate.Packrat.Data := WisiToken.Generate.Packrat.Initialize
              (Input_Data.Grammar_Lexer.File_Name, Generate_Data.Grammar, Generate_Data.Source_Line_Map,
               Generate_Data.LR1_Descriptor.First_Terminal);
         begin
            if not Lexer_Done (Input_Data.User_Lexer) then
               Lexer_Done (Input_Data.User_Lexer) := True;
               if Input_Data.User_Lexer = re2c_Lexer then
                  Wisi.Output_Ada_Common.Create_re2c
                    (Input_Data, Quad, Generate_Data, -Output_File_Name_Root, Input_Data.Elisp_Names.Regexps);
                  --  FIXME: "Elisp_Names" is a misnomer if output lang /= elisp; "lexer_names" would be better?
               end if;
            end if;

            if not Generate_Done (Input_Data.User_Lexer)(Quad.Gen_Alg) then
               Generate_Done (Input_Data.User_Lexer)(Quad.Gen_Alg) := True;

               if WisiToken.Trace_Generate = 0 then
                  Create
                    (Parse_Table_File, Out_File,
                     -Output_File_Name_Root & "_" & To_Lower (Generate_Algorithm'Image (Quad.Gen_Alg)) &
                       (if Input_Data.If_Lexer_Present
                        then "_" & Lexer_Image (Input_Data.User_Lexer).all
                        else "") &
                       ".parse_table");
                  Set_Output (Parse_Table_File);
               end if;

               case Quad.Gen_Alg is
               when LALR =>
                  Time_Start := Clock;

                  Generate_Data.LR_Parsers (LALR) := WisiToken.LR.LALR_Generate.Generate
                    (Generate_Data.Grammar,
                     Generate_Data.LALR_Descriptor.all,
                     Generate_Utils.To_Conflicts
                       (Generate_Data, Input_Data.Conflicts, Input_Data.Grammar_Lexer.File_Name),
                     Generate_Utils.To_McKenzie_Param (Generate_Data, Input_Data.McKenzie_Recover),
                     Put_Parse_Table => True);

                  if Do_Time then
                     Time_End := Clock;

                     Put_Line
                       (Standard_Error,
                        "generate time:" & Duration'Image (To_Duration (Time_End - Time_Start)));
                  end if;

                  Generate_Data.Parser_State_Count :=
                    Generate_Data.LR_Parsers (LALR).State_Last - Generate_Data.LR_Parsers (LALR).State_First + 1;
                  Wisi.Generate_Utils.Count_Actions (Generate_Data, LALR);
                  Wisi.Generate_Utils.Put_Stats (Input_Data, Generate_Data);

               when LR1 =>
                  Time_Start := Clock;

                  Generate_Data.LR_Parsers (LR1) := WisiToken.LR.LR1_Generate.Generate
                    (Generate_Data.Grammar,
                     Generate_Data.LR1_Descriptor.all,
                     Generate_Utils.To_Conflicts
                       (Generate_Data, Input_Data.Conflicts, Input_Data.Grammar_Lexer.File_Name),
                     Generate_Utils.To_McKenzie_Param (Generate_Data, Input_Data.McKenzie_Recover),
                     Put_Parse_Table => True);

                  if Do_Time then
                     Time_End := Clock;

                     Put_Line
                       (Standard_Error,
                        "generate time:" & Duration'Image (To_Duration (Time_End - Time_Start)));
                  end if;

                  Generate_Data.Parser_State_Count :=
                    Generate_Data.LR_Parsers (LR1).State_Last - Generate_Data.LR_Parsers (LR1).State_First + 1;
                  Wisi.Generate_Utils.Count_Actions (Generate_Data, LR1);
                  Wisi.Generate_Utils.Put_Stats (Input_Data, Generate_Data);

               when Packrat_Generate_Algorithm =>
                  if Do_Time then
                     --  The only significant computation done for Packrat is First, done
                     --  in Initialize; not worth timing.
                     Put_Line (Standard_Error, " --time not supported for Packrat");
                  end if;

                  Put_Line ("Tokens:");
                  WisiToken.Put_Tokens (Generate_Data.LR1_Descriptor.all);
                  New_Line;
                  Put_Line ("Productions:");
                  WisiToken.Productions.Put (Generate_Data.Grammar, Generate_Data.LR1_Descriptor.all);

                  Packrat_Data.Check_All (Generate_Data.LR1_Descriptor.all);
               end case;

               if WisiToken.Trace_Generate = 0 then
                  Set_Output (Standard_Output);
                  Close (Parse_Table_File);
               end if;

               if WisiToken.Generate.Error then
                  raise WisiToken.Grammar_Error with "errors: aborting";
               end if;
            end if;

            case Quad.Out_Lang is
            when Ada =>
               Wisi.Output_Ada (Input_Data, -Output_File_Name_Root, Generate_Data, Packrat_Data, Quad, Test_Main);

            when Ada_Emacs =>
               Wisi.Output_Ada_Emacs
                 (Input_Data, -Output_File_Name_Root, Generate_Data, Packrat_Data, Quad, Test_Main, -Language_Name);

            when Elisp =>
               Wisi.Output_Elisp (Input_Data, -Output_File_Name_Root, Generate_Data, Packrat_Data, Quad);

            end case;
         end;
      end loop;
   end;
exception
when WisiToken.Syntax_Error =>
   --  error message already output
   Standard.Ada.Command_Line.Set_Exit_Status (Standard.Ada.Command_Line.Failure);

when E : User_Error =>
   declare
      use Standard.Ada.Command_Line;
      use Standard.Ada.Exceptions;
      use Standard.Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Exception_Message (E));
      Put_Command_Line (Ada_Comment);
      Set_Exit_Status (Failure);
      Put_Usage;
   end;

when E : WisiToken.Grammar_Error =>
   --  error message not already output
   declare
      use Standard.Ada.Command_Line;
      use Standard.Ada.Exceptions;
      use Standard.Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Exception_Message (E));
      Set_Exit_Status (Failure);
   end;

when E :  others =>
   --  IMPROVEME: for some exceptions, Error message already output via wisi.utils.Put_Error
   declare
      use Standard.Ada.Text_IO;
      use Standard.Ada.Exceptions;
      use Standard.Ada.Command_Line;
   begin
      Put_Line (Standard_Error, Exception_Name (E) & ": " & Exception_Message (E));
      Put_Line (Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Set_Exit_Status (Failure);
   end;

end Wisi.Generate;
