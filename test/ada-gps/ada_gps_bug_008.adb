--  Abstract :
--
--  ada_mode_gps_indent bug #8: double ada-indent-broken in rare cases
--
procedure Bug_008 is
begin

   --  Coding standard violation; '..' should be on beginning of next line
   Normal_Flight.Regulations_Confirmed_In (FTFX.N + 1 ..
                                             FTFX.N + N)
     := Regulations_Confirmed_In (1 .. N);

   --  No coding standard violation
   Normal_Flight.Regulations_Confirmed_In (FTFX.N + 1
                                           .. FTFX.N + N)
     := Regulations_Confirmed_In (1 .. N);

end Bug_008;
