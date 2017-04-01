--  Abstract :
--
--  ada_mode_gps_indent bug #6: indentation inside aggregate which is a parameter
--
procedure Bug_006 is
begin
   The_TV := E (Query, (Def_Restriction.Traffic_Volume,
                        Def_Traffic_Volume.Convert
                          (CASA_Main.TV_Identifier)));
end Bug_006;
