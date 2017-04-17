--  Bugs reported by Simon Wright Sep 2013. Does not compile
with Ada.Finalization;
with Ada.Streams;
package Access_In_Record is

   --  Indenting (A'Access) gives "wisi-indent-line: Wrong type argument: number-or-marker-p, nil"
   type A
      is new Ada.Streams.Root_Stream_Type with record
         Finalizer : A_Finalizer
           (A'Access);
         Length    : Natural := 0;
      end record;

   type Wrong_Layout_With_Private (R : access Integer)
     is new A with private;

private

   type Wrong_Layout_With_Private (R : access Integer)
     is new A with null record;

end Access_In_Record;
