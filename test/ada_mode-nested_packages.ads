--EMACSCMD:(setq skip-recase-test t) we have identifiers that don't follow the standard naming convention

-- test align of 'use'. need a blank line to avoid indenting this comment (sigh). result is tested with diff.
--EMACSCMD:(progn (forward-line 2)(forward-word 3)(forward-char 1)(insert "   ")(ada-align))

with Ada.Directories;  use Ada.Directories;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada_Mode.Nominal; use Ada_Mode.Nominal;
package Ada_Mode.Nested_Packages is

   Local_Exception_1 : exception renames
     Global_Exception_1;

   package Sequencer is
      function Create (Model   : in Integer;
                       Context : in String) return String;
   end Sequencer;

   package Wem is
      procedure GetVariableValue;
   end Wem;

   procedure F;

   procedure Server_Begin;

   package Test_Format is
      procedure Test_Proc;
   end Test_Format;

   package TestForWhile is
      procedure Test;
   end TestForWhile;

private -- part of Ada_Mode.Nested_Packages, not Private_Only

   package Private_Only is
   private
      package Private_Package is
      end Private_Package;
      A : Integer;
   end Private_Only;

   procedure Test_For_1;

end Ada_Mode.Nested_Packages;
