-- Bugs reported by Mats Weber <Mats.Weber@elca-matrix.ch>
-- ada-mode version: 2.23

--EMACSCMD:(jit-lock-fontify-now)

with Ada.Text_Io; use Ada.Text_Io;
package body Mats_Weber_Bugs is
   -- Fontification is messed up with string delimiter inside
   -- character constant
   -- starting with the '"' character, everything is fontified as a string
   -- litteral until the end of the buffer.
   -- Indentation is also incorrect for the following statment
   -- Fixed in: ada-mode 3.2, by using text properties on '
   procedure Test_1 is
      A : Integer;
   begin
      if False then
         --EMACSCMD:(test-face "'" 'font-lock-string-face)
         Put('"');
         --EMACSCMD:(test-face "\"" 'font-lock-string-face)
         Put('"');
         A := 1;
      end if;
   end Test_1;

   procedure Test_2 is
   begin
      --  Fontification problem for a raise without an exception name
      --  Fixed in: ada-mode 2.28

      --EMACSCMD:(test-face "raise" 'font-lock-keyword-face)
      raise Error;
   exception
      --EMACSCMD:(test-face "raise" 'font-lock-keyword-face)
      when others => raise;
   end Test_2;

   --  Incorrect fontification of 'body'

   --EMACSCMD:(test-face "body" 'font-lock-keyword-face)
   package body Test_3 is
      --EMACSCMD:(test-face "body" 'font-lock-keyword-face)
      task body Yy is
      begin
         null;
      end Yy;

      -- Bad indentation of a begin .. end block inside a loop
      procedure Test_4 is
      begin
         begin
            null;
         exception
            when others =>
               null;
         end;

         loop
            begin
               null;
            end;
         end loop;
      end Test_4;

      -- the following piece of code is not indented correctly
      package body P is

         procedure Q is
         begin
            declare
            begin
               null;
            end;

            if True then
               begin
                  null;
               exception
                  when Constraint_Error => null;
               end;
            end if;
         end Q;

      end P;

      -- Wrong indentation of continuation lines in record declaration
      -- and parameter lists

      procedure Test_5 is
         type Tt is
            record
               Min,
                 Max : Integer;
            end record;
         File : File_Type;
         A : String := "hi";
         B : String := "World";
      begin
         -- indent of
         Put(File,
             Item => A &
               B);
      end Test_5;

   end Test_3;
end Mats_Weber_Bugs;
