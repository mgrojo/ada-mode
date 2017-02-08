--  Abstract :
--
--  bug reported by Ludovic Brenta. does not compile
--

procedure Bug_2016_11_21_01
is begin
   --  The two leading '&' were not aligned.
   Error_Report.Report_Error (Severity    => Error_Report.Warning,
                              Title       => "ROBOT_ERROR",
                              Description =>
                                "DLA/CHG Message received after TACT"
                                  & Flight_Identifier.Safe_Image ("toto")
                                  & " => message is discarded !!");

   --  These were; make sure they still are.
   Error_Report.Report_Error (Severity    => Error_Report.Warning,
                              Title       => "ROBOT_ERROR",
                              Description =>
                                "DLA/CHG Message received after TACT"
                                  & "toto"
                                  & " => message is discarded !!");

end Bug_2016_11_21_01;
