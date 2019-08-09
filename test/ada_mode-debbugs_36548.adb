--  Indent used to fail with Programmer_Error.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

with Ada.Text_IO; use Ada.Text_IO;
procedure Field_parent is

   type Root_Disc_T is (Rdisc1, Rdisc2, Rdisc3);

   type Root_T (Root_Disc : Root_Disc_T := Root_Disc_T'First) is tagged
      limited record
         Root_Field : Integer;
         case Root_Disc is
            when Rdisc1 => Root_Field1 : Integer;
            when Rdisc2 => Root_Field2 : Float;
            when Rdisc3 => Root_Field3 : Character;
         end case;
      end record;

   type Child_Disc_T is (Cdisc1, Cdisc2, Cdisc3);

   type Child_T (Child_Disc : Child_Disc_T := Child_Disc_T'First) is new
     Root_T (Rdisc2) with record --  Error here; extra 'is record'
     is new Root_T  with record
     Child_Field : Integer;
     case Child_Disc is
        when Cdisc1 => Child_Field1 : Integer;
        when Cdisc2 => Child_Field2 : Float;
        when Cdisc3 => Child_Field4 : Character;
     end case;record; --  Error: missing 'end' for 'record'

   type D1_T is (D1, D2, D3);
   type D2_T is (D4, D5, D6);

   type Var_T (Disc1 : D1_T, Disc2 : D2_T) is record T --  Error: ',' should be ';' ; extra 'T'
      Field1 : Integer;
   end record;


   Child : Child_T := (Cdisc1, Child_Field  => 1, Child_Field1 => 2,
                       Root_Field => 3, Root_Field2 => 1.42);
begin
   Put_Line ("Hi:" & Child.Child_Field'Image);
   end;
   -- Local Variables:
   -- wisi-mckenzie-task-count: 1
   -- End:
