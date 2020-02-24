--  Testing Ada 2020 features currently supported by GNAT Community
--
--  [1] http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/
--  [2] http://www.ada-auth.org/standards/ada2x.html

package Ada_Mode.Ada2020 is

   subtype Worker is Integer range -10 .. 10;

   type Worker_Indexes is range 1 .. 10;

   function Creator_1 (Index : Worker_Indexes) return Worker;

   function Creator_2 (Index : Worker_Indexes) return Worker;

end Ada_Mode.Ada2020;
