--  Testing Ada 2020 features

procedure Ada_Mode.Ada2020 is

   subtype Worker is Natural range 1 .. 20;

   function Creator (Index : Worker) return Worker;
   function Creator (Index : Worker) return Worker is
   begin
      return Index;
   end Creator;

   type Task_Array is array (1 .. 10) of Worker;

   --  http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/ai12-0061-1.txt
   Worker_Tasks : Task_Array :=
     (for Index in Worker_Indexes => Creator (Index)); -- hangs gprbuild; AdaCore ticket T107-052

   A : Integer := 1;
begin
   --  http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/ai12-0125-3.txt
   A := @ + 1;

   --  http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/ai12-0127-1.txt
   Worker_Tasks := (@ with delta 4 => 5);
end Ada_Mode.Ada2020;
