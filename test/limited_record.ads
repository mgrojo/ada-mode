--  This package tests the indentation of limited record declarations.
--  Tracking number: 8031-015

package Limited_Record is

   --------------
   --  Indentation was different for records and limited record
   --------------

   type T1 (A : Integer;
            B : Integer) is
      limited record  --  Indentation was (ada-broken-indent)
         V : Character;
      end record;


   type T2 is
      record         --  But here it was ada-indent-record-rel-type
         V : Character;
      end record;

   --------------
   --  Indentation was incorrect for discriminated records (and limited)
   --------------

   type T3 (A : Integer;
            B : Integer) is limited record
      V : Character;
      C : Integer;
   end record;

   type T6 (A : Integer;
            B : Integer) is record
      V : Character;
   end record;

   --------------
   --  Simple declarations are fine
   --------------

   type T4 is record
      V : Character;
   end record;

   type T5 is limited record
      V : Character;
   end record;

end Limited_Record;
