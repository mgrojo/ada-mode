--  Abstract :
--
--  ada_mode_gps_indent bug #11: hanging indent in named association
--
function Bug_011 return String is
begin
   A (Query =>
        Query,
      Object
        => 1);
end Bug_011;
