-- This file tests the indentation of the 'begin' keyword, especially
-- when in subblocks.
--
-- Also other stuff

with Ada.Directories;
with Ada.Text_IO;
with Ada_Mode.Nominal;
package body Ada_Mode.Nested_Packages is

   function "*" (Left, Right : in Ada_Mode.Nominal.Floating_Point) return Ada_Mode.Nominal.Floating_Point
     renames Ada_Mode.Nominal."*";

   --  begin block in a loop statement. [6618-008]
   package body Sequencer is

      function Create (Model   : in Integer;
                       Context : in String) return String is
         -- Anonymous array
         Cache : array (1 .. 10) of Boolean := (True, False, others => False);
         Strlist : String (1 .. 2);
      begin
         if Cache (Model) then
            begin
               null;
            end;
            loop
               begin  --  was indented in the first column (adamode-4.0)
                  Strlist := ('(', '#');
               end;
            end loop;
         end if;
         return Strlist;
      end Create;
   end Sequencer;

   --  two begin blocks following each other [6726-028]
   package body Wem is
      procedure GetVariableValue is
      begin
         if True then
            begin
               null;
            end;
            begin   --  uncorrectly indented (based on 'package')
               null;
            end;
         end if;
      end GetVariableValue;
   end Wem;


   --  a declare block just within a nested subprogram declaration
   --  [6920-004]
   procedure F is
   begin
      declare
         procedure P is
            procedure Q is
            begin
               null;
            end;
         begin
            null;
         end;
      begin  -- uncorrectly indented (based on procedure P)
         null;
      end;
   end;


   --  identifiers that include 'end', 'begin', ...
   --  as part of their name
   --  [7029-002]
   procedure Server_Begin is
      procedure Wait_For_End is
      begin
         null;
      end Wait_For_End;

   begin             --  uncorrectly indented (based on end...)
      null;
   end Server_Begin;

   --  'end if' interaction...
   --  [7103-008]
   package body Test_Format is
      procedure Test_Proc is
         Local_File : Ada.Text_IO.File_Type;
         package Io renames Ada.Text_IO;
         A_File : Io.File_Type renames Local_File;
      begin
         --  Indentation is broken by this if statement
         if Ada.Text_Io.Is_Open (Local_File) then
            Io.Close(Local_File);
         end if;

         begin
            Ada.Directories.Create_Directory(Ada.Directories.Full_Name("foo"));
         exception
            when Io.Name_Error =>
               null;
         end;

         begin   --  uncorrectly indented (based on package body)
            null;
         end;
      end Test_Proc;
   end Test_Format;

   --  "for" not associated with "while" or "loop"
   package body TestForWhile is
      procedure Test is
         Foo : Integer;
         The_Bits : Integer;
         for The_Bits'Address use Foo'Address;
      begin
         if The_Bits > 0 then
            loop
               null;
            end loop;   --  uncorrectly indented (because of the for_clause)
         end if;
      end Test;
   end TestForWhile;

   -- "for" loop with nested begin
   procedure Test_For_1 is
   begin
      for j in 1 .. 10 loop
         begin             -- was not indented
            null;
         end;
      end loop;
   end Test_For_1;

end Ada_Mode.Nested_Packages;
