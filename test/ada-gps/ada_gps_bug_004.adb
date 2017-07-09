--  Abstract :
--
--  ada_mode_gps_indent bug #4: multiline comments starting after code
--
procedure Bug_004
is
begin

   Format := Normal_Flight.Fixed_Info.ICAO_Content;  -- If no new format is given in the current incoming message
                                                     -- we keep the current format. If this happens the first time
                                                     -- (i.e for a new flight) this attribute has a default value
                                                     -- ( = icao_mode.old_format)

   A ((1,
       2), -- trailing comment
           -- comment aligned with trailing comment
      3);
end Bug_004;
