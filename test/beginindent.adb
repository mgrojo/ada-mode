-- This file tests the indentation of the 'begin' keyword, especially
-- when in subblocks


--  First case: begin block in a loop statement. [6618-008]


package body Sequencer is
   function Create (Model   : in String;
                    Context : in String) return SequencerRef is
   begin
      if CacheLAT.IsInLAT (Cache, Model) then
         begin
            null;
         end;
         loop
            begin  --  was indented in the first column (adamode-4.0)
               Strlist := TokenizerOnBlank.GetTokenList (ConfigFile, '#');
            end;
         end loop;
      end if;
   end Create;
end Sequencer;


--  Second case : two begin blocks following each other [6726-028]

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


--  Third case : a declare bug just within a nested subprogram declaration
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
   end;
end;


--  Fourth case : identifiers that include 'end', 'begin', ...
--  as part of their name
--  [7029-002]

procedure Server_Main is
   procedure Wait_For_End is
   begin
      null;
   end Wait_For_End;

begin             --  uncorrectly indented (based on end...)
   null;
end Server_Main;


--  Fifth case: 'end if' interaction...
--  [7103-008]
package body Test_Format is
   procedure Test_Proc is
   begin
      --  Indentation is broken by this if statement
      if Io.Is_Open(Checkpoint_File) then
         Io.Close(Checkpoint_File);
      end if;

      begin
         Posix_Files.Unlink(Posix.To_Posix_String(Name));
      exception
         when File_Io.File_Not_Found =>
            null;
      end;

      begin   --  uncorrectly indented (based on package body)
         null;
      end;
   end Test_Proc;
end Test_Format;


--  Sixth Case: for loop not associated with a while or for
--  ada-mode was looking for a possible 'for' or 'while' as far as needed,
--  instead of stopping at the first ';' encountered.
--  Fixed 12/02/1999

package body TestForWhile is
   procedure Test is
      The_Bits : Integer;
      for The_Bits'Address use Foo'Address;
   begin
      if A then
         loop
            null;
         end loop;   --  uncorrectly indented (because of the for_clause)
      end if;
   end Test;
end TestForWhile;

