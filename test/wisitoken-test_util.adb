--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2021, 2022 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);
with AUnit.Assertions;
with AUnit.Checks;
package body WisiToken.Test_Util is

   procedure Spawn
     (Program     : in String;
      Args        : in GNAT.OS_Lib.String_List;
      Output_File : in String := "")
   is
      use Ada.Text_IO;
      use AUnit.Checks;
      use GNAT.OS_Lib;
      Exe         : constant String_Access := Locate_Exec_On_Path (Program);
      Success     : Boolean;
      Return_Code : Integer;
      pragma Unreferenced (Return_Code);
   begin
      if Exe = null then
         AUnit.Assertions.Assert (False, "'" & Program & "' not found on path");
      end if;

      if WisiToken.Trace_Tests > WisiToken.Detail then
         Put (Standard_Error, Program);
         for Str_Acc of Args loop
            Put (Standard_Error, " ");
            Put (Standard_Error, Str_Acc.all);
         end loop;
         if Output_File /= "" then
            Put (Standard_Error, " > " & Output_File);
         end if;

         New_Line (Standard_Error);
      end if;

      if Output_File = "" then
         Spawn
           (Program_Name => Exe.all,
            Args         => Args,
            Success      => Success);
      else
         Spawn
           (Program_Name => Exe.all,
            Args         => Args,
            Output_File  => Output_File,
            Err_To_Out   => True,
            Return_Code  => Return_Code,
            Success      => Success);
      end if;

      Check (Program, Success, True);
   end Spawn;

   procedure Dos2unix (File_Name : in String)
   is
      use GNAT.OS_Lib;
   begin
      if GNAT.OS_Lib.Directory_Separator = '\' then
         declare
            Exe : constant String_Access := Locate_Exec_On_Path ("dos2unix.exe");
            Success : Boolean;
            pragma Unreferenced (Success);
         begin
            Spawn
              (Program_Name => Exe.all,
               Args         =>
                 (1         => new String'("-q"),
                  2         => new String'(File_Name)),
               Success      => Success);
         end;
      end if;
   end Dos2unix;

end WisiToken.Test_Util;
