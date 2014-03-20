--  Abstract :
--
--  Support Emacs Ada mode and gnat-query minor mode queries about
--  GNAT projects and cross reference data
--
--  Copyright (C) 2014 Free Software Foundation All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Command_Line;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.Arg_Lists;
with GNATCOLL.Paragraph_Filling;
with GNATCOLL.Projects;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces;
with GNATCOLL.Utils;
with GNATCOLL.VFS;
with GNATCOLL.Xref;
procedure GNATQuery is

   function "+" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   function "+" (Item : in GNATCOLL.VFS.Filesystem_String) return String
   is begin
      return String (Item);
   end "+";


   Me : constant GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create ("Query");

   Invalid_Command : exception;
   procedure Process_Line (Line : String);
   --  Process a full line of commands.
   --  Raise Invalid_Command when the command is invalid.

   procedure On_Ctrl_C;
   pragma Convention (C, On_Ctrl_C);

   function Get_Entity (Arg : String) return GNATCOLL.Xref.Entity_Information;
   --  Return the entity matching the "name:file:line:column" argument

   type My_Xref_Database is new GNATCOLL.Xref.Xref_Database with null record;
   --  Derived so we can override Image to output full paths

   overriding function Image (Self : My_Xref_Database; File : GNATCOLL.VFS.Virtual_File) return String;
   function Image (Self : GNATCOLL.Xref.Entity_Information) return String;
   --  Return a display version of the argument

   Xref : aliased My_Xref_Database;
   Env     : GNATCOLL.Projects.Project_Environment_Access;
   Tree    : GNATCOLL.Projects.Project_Tree;
   Previous_Progress : Natural := 0;
   Progress_Reporter : access procedure (Current, Total : Integer) := null;

   procedure Dump (Refs : in out GNATCOLL.Xref.References_Cursor'Class);
   --  Display the results of a query

   generic
      with function Compute
        (Self   : in GNATCOLL.Xref.Xref_Database'Class;
         Entity : in GNATCOLL.Xref.Entity_Information)
        return GNATCOLL.Xref.Entity_Information;
   procedure Process_Command_Single (Args : GNATCOLL.Arg_Lists.Arg_List);
   --  Get the entity identified by Args, which must contain a single
   --  argument. Then call Compute, and output the result.
   --
   --  Appropriate for queries that return a single entity result.

   procedure Process_Command_Single (Args : GNATCOLL.Arg_Lists.Arg_List)
   is
      use GNATCOLL.Arg_Lists;
      use GNATCOLL.Xref;

      Entity : Entity_Information;
      Comp   : Entity_Information;
   begin
      if Args_Length (Args) /= 1 then
         Ada.Text_IO.Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));
      Comp := Compute (Xref, Entity);
      if Comp /= No_Entity then
         Ada.Text_IO.Put_Line (Image (Comp));
      end if;
   end Process_Command_Single;

   --  Command procedures; Args is the command line.
   --
   --  Infrastructre commands
   procedure Process_Help (Args : GNATCOLL.Arg_Lists.Arg_List);
   procedure Process_Refresh (Args : GNATCOLL.Arg_Lists.Arg_List);

   --  Queries; alphabetical
   procedure Process_Overrides is new Process_Command_Single (GNATCOLL.Xref.Overrides);
   procedure Process_Refs (Args : GNATCOLL.Arg_Lists.Arg_List);
   procedure Process_Source_Dirs (Args : GNATCOLL.Arg_Lists.Arg_List);

   procedure Load_Project (Path : GNATCOLL.VFS.Virtual_File);
   --  Load the given project

   type Command_Descr is record
      Name    : GNAT.Strings.String_Access;
      Args    : GNAT.Strings.String_Access;
      Help    : GNAT.Strings.String_Access;
      Handler : access procedure (Args : GNATCOLL.Arg_Lists.Arg_List);
   end record;

   Commands : constant array (Natural range <>) of Command_Descr :=
     ((new String'("help"),
       new String'("[command or variable name]"),
       new String'("Display the list of commands and their syntax."),
       Process_Help'Access),

      (new String'("refresh"),
       null,
       new String'("Refresh the contents of the xref database."),
       Process_Refresh'Access),

      (new String'("overrides"),
       new String'("name:file:line:column"),
       new String'("The entity that is overridden by the parameter"),
       Process_Overrides'Access),

      (new String'("refs"),
       new String'("name:file:line:column"),
       new String'("Display all known references to the entity."),
       Process_Refs'Access),

      (new String'("source_dirs"),
       null,
       new String'("Return the project source directories, recursively."),
       Process_Source_Dirs'Access));

   --  Parsed command line info
   Cmdline              : GNAT.Command_Line.Command_Line_Configuration;
   Commands_From_Switch : aliased GNAT.Strings.String_Access;
   DB_Name              : aliased GNAT.Strings.String_Access := new String'("gnatquery.db");
   Nightly_DB_Name      : aliased GNAT.Strings.String_Access;
   Show_Progress        : aliased Boolean;
   Project_Name         : aliased GNAT.Strings.String_Access;
   Config_File          : aliased GNAT.Strings.String_Access;
   ALI_Encoding         : aliased GNAT.Strings.String_Access := new String'("");

   ----------
   --  Procedure bodies, alphabetical

   procedure Display_Progress (Current, Total : Integer) is
      Now : constant Integer := Integer (Float'Floor
        (Float (Current) / Float (Total) * 100.0));
   begin
      if Now /= Previous_Progress then
         Ada.Text_IO.Put_Line
           ("completed" & Current'Img
              & " out of" & Total'Img
              & " (" & GNATCOLL.Utils.Image (Now, Min_Width => 0) & "%)...");
         Previous_Progress := Now;
      end if;
   end Display_Progress;

   procedure Dump (Refs : in out GNATCOLL.Xref.References_Cursor'Class)
   is
      use GNATCOLL.Xref;

      Ref : Entity_Reference;
   begin
      while Has_Element (Refs) loop
         Ref := Refs.Element;

         declare
            Name : constant String := +Xref.Declaration (Ref.Entity).Name;
         begin
            Ada.Text_IO.Put_Line (Name & ':' & Xref.Image (Ref) & " (" & (+Ref.Kind) & ")");
         end;

         Next (Refs);
      end loop;
   end Dump;

   function Get_Entity (Arg : String) return GNATCOLL.Xref.Entity_Information
   is
      use GNAT.Directory_Operations;
      use GNATCOLL.Xref;

      Words  : GNAT.Strings.String_List_Access := GNATCOLL.Utils.Split (Arg, On => ':');
      Ref    : GNATCOLL.Xref.Entity_Reference;
   begin
      case Words'Length is
      when 4 =>
         Ref := Xref.Get_Entity
           (Name     => Words (Words'First).all,
            File     => Format_Pathname
              (Style => UNIX,
               Path  => Words (Words'First + 1).all),
            Line     => Integer'Value (Words (Words'First + 2).all),
            Column   => Visible_Column
              (Integer'Value (Words (Words'First + 3).all)));

      when 3 =>
         Ref := Xref.Get_Entity
           (Name     => Words (Words'First).all,
            File     => Format_Pathname
              (Style => UNIX,
               Path  => Words (Words'First + 1).all),
            Line     => Integer'Value (Words (Words'First + 2).all));

      when 2 =>
         Ref := Xref.Get_Entity
           (Name     => Words (Words'First).all,
            File     => Format_Pathname
              (Style => UNIX,
               Path  => Words (Words'First + 1).all));

      when others =>
         Ada.Text_IO.Put_Line
           ("Invalid parameter, expecting name:file:line:column => '" & Arg & "'");
         GNAT.Strings.Free (Words);
         return No_Entity;
      end case;

      GNAT.Strings.Free (Words);

      if Ref.Entity = GNATCOLL.Xref.No_Entity then
         Ada.Text_IO.Put_Line ("Error: entity not found '" & Arg & "'");

      elsif GNATCOLL.Xref.Is_Fuzzy_Match (Ref.Entity) then
         Ada.Text_IO.Put_Line ("fuzzy match for the entity");
         --  FIXME: gnat-query.el look for this, prompt for reparse?
      end if;

      return Ref.Entity;
   end Get_Entity;

   overriding function Image (Self : My_Xref_Database; File : GNATCOLL.VFS.Virtual_File) return String
   is
      pragma Unreferenced (Self);
   begin
      return File.Display_Full_Name;
   end Image;

   function Image (Self : GNATCOLL.Xref.Entity_Information) return String
   is
      use GNATCOLL.Xref;

      Decl : Entity_Declaration;
   begin
      if Self = No_Entity then
         return "Unknown entity";
      else
         Decl := Xref.Declaration (Self);

         if Is_Predefined_Entity (Decl) then
            return "predefined entity: " & (+Decl.Name);
         else
            return +Decl.Name & ":" & Xref.Image (Decl.Location);
         end if;
      end if;
   end Image;

   procedure Load_Project (Path : GNATCOLL.VFS.Virtual_File)
   is
      GNAT_Version : GNAT.Strings.String_Access;
   begin
      --  FIXME: this dies for aggregate projects in GNAT 7.2
      Env.Set_Path_From_Gnatls
        (Gnatls       => "gnatls",
         GNAT_Version => GNAT_Version,
         Errors       => Ada.Text_IO.Put_Line'Access);
      GNAT.Strings.Free (GNAT_Version);

      --  The default extensions must match those defined in the gprconfig
      --  knowledge base. FIXME: read them from there! Where is this used?
      --  Env.Register_Default_Language_Extension
      --    (Language_Name       => "C",
      --     Default_Spec_Suffix => ".h",
      --     Default_Body_Suffix => ".c");
      --  Env.Register_Default_Language_Extension
      --    (Language_Name       => "C++",
      --     Default_Spec_Suffix => ".hh",
      --     Default_Body_Suffix => ".cpp");

      GNATCOLL.Traces.Trace (Me, "processing 'PROJECT' '" & Path.Display_Full_Name & "'");
      Tree.Load
        (Root_Project_Path => Path,
         Env               => Env,
         Errors            => Ada.Text_IO.Put_Line'Access);

   exception
   when GNATCOLL.Projects.Invalid_Project =>
      Ada.Text_IO.Put_Line (Path.Display_Full_Name & ": error: invalid project file");
   end Load_Project;

   procedure On_Ctrl_C is
   begin
      Free (Xref);
      GNAT.OS_Lib.OS_Exit (0);
   end On_Ctrl_C;

   procedure Process_Help (Args : GNATCOLL.Arg_Lists.Arg_List)
   is
      use Ada.Text_IO;
      use GNATCOLL.Arg_Lists;
      use type GNAT.Strings.String_Access;
   begin
      for C in Commands'Range loop
         if Args_Length (Args) = 0
           or else Nth_Arg (Args, 1) = Commands (C).Name.all
         then
            Put ("  " & Commands (C).Name.all);
            if Commands (C).Args = null then
               New_Line;
            else
               Put_Line (" " & Commands (C).Args.all);
            end if;

            Put
              (Ada.Strings.Unbounded.To_String
                 (GNATCOLL.Paragraph_Filling.Knuth_Fill
                    (Commands (C).Help.all,
                     Max_Line_Length => 70,
                     Line_Prefix     => "      ")));
         end if;
      end loop;
   end Process_Help;

   procedure Process_Line (Line : String)
   is
      Expr : GNAT.Strings.String_List_Access;
   begin
      if Ada.Strings.Fixed.Trim (Line, Ada.Strings.Both) = "" then
         return;
      end if;

      Expr := GNATCOLL.Utils.Split (Line, On => ';');

      for C in Expr'Range loop
         if Ada.Strings.Fixed.Trim (Expr (C).all, Ada.Strings.Both) = "" then
            null;

         else
            declare
               use GNATCOLL.Arg_Lists;
               List  : constant Arg_List := Parse_String (Expr (C).all, Mode => Separate_Args);
               Cmd   : constant String   := Ada.Characters.Handling.To_Lower (Get_Command (List));
               Found : Boolean           := False;
            begin
                  for Co in Commands'Range loop
                     if Commands (Co).Name.all = Cmd then
                        Commands (Co).Handler (List);
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Ada.Text_IO.Put_Line ("Invalid command: '" & Cmd & "'");
                     raise Invalid_Command;
                  end if;
            end;

         end if;
      end loop;

      GNAT.Strings.Free (Expr);
   end Process_Line;

   procedure Process_Refresh (Args : GNATCOLL.Arg_Lists.Arg_List)
   is
      use type GNATCOLL.Projects.Project_Environment_Access;
      pragma Unreferenced (Args);
   begin
      if Env /= null then
         Xref.Parse_All_LI_Files
           (Tree                => Tree,
            Project             => Tree.Root_Project,
            Parse_Runtime_Files => True,
            Show_Progress       => Progress_Reporter,
            ALI_Encoding        => ALI_Encoding.all,
            From_DB_Name        => Nightly_DB_Name.all,
            To_DB_Name          => DB_Name.all);
      end if;
   end Process_Refresh;

   procedure Process_Refs (Args : GNATCOLL.Arg_Lists.Arg_List)
   is
      use GNATCOLL.Arg_Lists;
      use GNATCOLL.Xref;
      Entity  : Entity_Information;
      Refs    : References_Cursor;
      Renamed : Entity_Information;
   begin
      if Args_Length (Args) /= 1 then
         Ada.Text_IO.Put_Line ("Invalid number of arguments");
         return;
      end if;

      Entity := Get_Entity (Nth_Arg (Args, 1));

      Renamed := Xref.Renaming_Of (Entity);
      if Renamed /= No_Entity then
         Ada.Text_IO.Put_Line ("Renaming of " & Image (Renamed));
      end if;

      Xref.References (Entity, Cursor => Refs);
      Dump (Refs);
   end Process_Refs;

   procedure Process_Source_Dirs (Args : GNATCOLL.Arg_Lists.Arg_List)
   is
      pragma Unreferenced (Args);
      Dirs : constant GNATCOLL.VFS.File_Array := GNATCOLL.Projects.Source_Dirs
        (Project   => Tree.Root_Project,
         Recursive => True);
      --  FIXME: add predefined_source_dirs:
      --  /usr/gnat-7.2.1-x86_64/lib/gcc/x86_64-pc-linux-gnu/4.7.4/rts-native/adainclude/g-string.ads

   begin
      for I in Dirs'Range loop
         Ada.Text_IO.Put_Line (+GNATCOLL.VFS.Full_Name (Dirs (I)));
      end loop;
   end Process_Source_Dirs;

begin
   declare
      use GNAT.Command_Line;
   begin
      Set_Usage
        (Cmdline,
         Help => "Query project info and cross-references on source code");
      Define_Switch
        (Cmdline,
         Output      => DB_Name'Access,
         Long_Switch => "--db=",
         Help        => "Specifies the name of the database (or ':memory:')");
      Define_Switch
        (Cmdline,
         Output      => Nightly_DB_Name'Access,
         Long_Switch => "--nightlydb=",
         Help        => "Specifies the name of a prebuilt database");
      Define_Switch
        (Cmdline,
         Output      => Commands_From_Switch'Access,
         Switch      => "-c:",
         Long_Switch => "--command=",
         Help        => "Execute the commands from ARG, and exit");
      Define_Switch
        (Cmdline,
         Output      => Project_Name'Access,
         Switch      => "-P:",
         Long_Switch => "--project=",
         Help        => "Load the given project (mandatory)");
      Define_Switch
        (Cmdline,
         Output      => Show_Progress'Access,
         Long_Switch      => "--display_progress",
         Switch      => "-d",
         Help        => "Show progress as LI files are parsed");
      Define_Switch
        (Cmdline,
         Output      => ALI_Encoding'Access,
         Long_Switch => "--encoding=",
         Switch      => "-e=",
         Help        => "The character encoding used for source and ALI files");
      Define_Switch
        (Cmdline,
         Output      => Config_File'Access,
         Long_Switch => "--config=",
         Help        => "Specify the configuration file (.cgpr) to load before"
           & " loading the project");

      GNATCOLL.Projects.Initialize (Env);

      Getopt (Cmdline, null);
   end;

   if Project_Name.all = "" then
      Ada.Text_IO.Put_Line ("No project file specified");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   declare
      use GNATCOLL.VFS;
      use type GNAT.Strings.String_Access;
      Path : constant Virtual_File := Create (+Project_Name.all);
   begin
      if not Path.Is_Regular_File then
         Ada.Text_IO.Put_Line (Project_Name.all & ": not found");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      --  FIXME: this requires gnatcoll 1.6, which requires gnat 7.2.
      --  but not really? just patch gnatcoll 1.6, like we did 1.6w
      --  In any case, needs to compile with current public release 2013

      --  if Config_File /= null and then Config_File.all /= "" then
      --     Env.Set_Config_File (Create_From_Base (+Config_File.all, Get_Current_Dir.Full_Name.all));
      --  end if;

      --  if Config_File /= null and then Config_File.all /= "" then
      --     Env.Set_Automatic_Config_File (True);
      --     Env.Set_Config_File (Create_From_Base ("auto.cgpr", Get_Current_Dir.Full_Name.all));
      --  end if;

      if Show_Progress then
         Progress_Reporter := Display_Progress'Unrestricted_Access;
      end if;

      Load_Project (Path);
   end;

   if DB_Name.all /= ":memory:" then
      declare
         use GNATCOLL.VFS;

         N    : constant String := DB_Name.all;
         Temp : Virtual_File    := Tree.Root_Project.Object_Dir;
         Dir2 : Virtual_File;
      begin
         GNAT.Strings.Free (DB_Name);

         --  If the project does not have an object directory, create
         --  the database in the directory containing the project file.
         if Temp = No_File then
            Temp := Tree.Root_Project.Project_Path.Dir;
            GNATCOLL.Traces.Trace
              (Me, "Root project does not have an object dir:" & ASCII.LF
                 & "creating database in " & (+Temp.Full_Name.all));
         end if;

         Temp := Create_From_Base (Base_Dir => Temp.Full_Name.all, Base_Name => +N);
         Dir2 := Create (Temp.Dir_Name);

         if not Dir2.Is_Directory then
            Dir2.Make_Dir (Recursive => True);
         end if;

         DB_Name := new String'(Temp.Display_Full_Name);
      end;
   end if;

   Xref.Setup_DB (GNATCOLL.SQL.Sqlite.Setup (Database => DB_Name.all));

   Process_Refresh (GNATCOLL.Arg_Lists.Empty_Command_Line);

   if Commands_From_Switch.all /= "" then
      Process_Line (Commands_From_Switch.all);
      On_Ctrl_C;
      return;
   end if;

   loop
      Ada.Text_IO.Put (">>> ");
      declare
         Input : constant String := Ada.Text_IO.Get_Line;
      begin
         exit when Input = "exit";
         Process_Line (Input);
      exception
         when Invalid_Command =>
            null;
      end;
   end loop;

   On_Ctrl_C;

exception
   when GNAT.Command_Line.Exit_From_Command_Line
      | Ada.Text_IO.End_Error =>
      On_Ctrl_C;
   when Invalid_Command =>
      On_Ctrl_C;
   when E : others =>
      On_Ctrl_C;
      Ada.Text_IO.Put_Line ("Unexpected exception");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end GNATQuery;
