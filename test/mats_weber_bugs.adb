-- Bugs reported by Mats Weber <Mats.Weber@elca-matrix.ch>
-- ada-mode version: 2.23

-- Fontification is messed up with string delimiter inside
-- character constant
-- starting with the '"' character, everything is fontified as a string
-- litteral until the end of the buffer.
-- Indentation is also incorrect for the following statment
-- Fixed in: ada-mode 3.2, by using text properties on '
procedure Test is
begin
   if False then
      Put('"');
      A := 1;
   end if;
end Test;

--  Fontification problem for a raise without an exception name
--  Fixed in: ada-mode 2.28

procedure Test is
begin
   raise Error; -- is fontified correctly, but in
   raise;       -- the raise keyword does not get the keyword font.
end Test;


--  Incorrect fontification of 'body'

package body Test is -- fontified correctly, but in
   task body Yy is -- "body" appears red.
   end Test;


   -- Bad indentation of a begin .. end block inside a loop --
   -- Workaround: The loop gets correct indentation if you remove the
   --  exception block

   procedure Test is
   begin
      begin
         null;
      exception
         when others =>
            null;
      end;

      loop
         begin  --  Begin is not correctly indented (under loop)
            null;
         end;
      end loop;
   end Test;


   -- the following piece of code is not indented correctly:
   -- The indentation gets correct if you remove the first declare block, select
   -- everything and hit C-c C-l.

   package body P is

      procedure Q is
      begin
         declare
         begin
            null;
         end;

         if True then
            begin
               exception -- missing 'statement;' causes bad indentation, but not crash
              when Constraint_Error => null;
            end;
         end if;
      end Q;

   end P;

   -- Wrong indentation of continuation lines in record declaration
   -- and parameter lists

   procedure Test is
      type Tt is
         record
            Min,
            Max : Int;
         end record;
      -- max gets indented with respect to min, which is wrong. I realise that
      -- this requires more analysis to get right, but I think it would be
      -- worth it if not too hard to implement.
   begin
      -- indent of
      Put(File,
          Item => A &
            B);
      -- this silimar case also gets the wrong indentation: "b" gets indented
      -- under "item" instead of "a" (same remark as in the previous one).

   end Test;

   --  Incorrect indentation when the previous line starts
   --  with a declaration (even if inside a paramlist....)
   --  reported by: Mats Weber <Mats.Weber@elca-matrix.ch>
   --  ada-mode version: 3.1

   package Test is

      procedure Toto (Un : in Integer;
                      Deux : in Integer) is private;

      procedure Tata;

   private

      A : Integer;
   end Test;

   -- no 'end package'.
