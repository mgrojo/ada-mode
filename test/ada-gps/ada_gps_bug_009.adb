--  Abstract :
--
--  ada_mode_gps_indent bug #9: hanging indent in expressions
--
function Bug_009 return String is
begin
   A :=
     " Tactid:"
       -- comment 1
       & " Lobt:"
       & " Iobt:";

   return
     " Tactid:"
       & " Lobt:"
       & " Iobt:";

   --  The following assume ada-indent-hanging-rel-exp t
   B := " Tactid:"
          -- comment 2
          & " Lobt:"
          & " Iobt:";

   return " Tactid:"
            & " Lobt:"
            & A
            & " Iobt:" & B;

   C :=
     Fim.Kind (Action_Info.Message) /= Fim.DLA
       and Fim.Kind (Action_Info.Message) /= Fim.APL
       and Fim.Kind (Action_Info.Message) /= Fim.ACH
       and Fim.Kind (Action_Info.Message) /= Fim.ARR
       and Fim.Kind (Action_Info.Message) /= Fim.DEP;

   Call_1 (Call_2
             (A,
              B));
end Bug_009;
-- Local Variables:
-- ada-indent-hanging-rel-exp: t
-- End:
