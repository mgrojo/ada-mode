--  Abstract :
--
--  ada_mode_gps_indent bug #2: does not honor ada-broken-indent
--
--
procedure Bug_002
is
begin
   Error_Report.Report_Error (Severity     =>
                                Error_Report.Severe,
                              Description  => "In theory the counterp_process..., "
                                & "as they should not differ but they do... "
                                & "the indexes, so that we do not crash....",
                              Title        => "Curtain indexes should not differ but they do.",
                              Caller_Scope => "Curtain_Indexes_May_Differ");
end Bug_002;
