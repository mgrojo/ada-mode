--  Demonstrate that gps indentation engine respects ada-mode indent params

procedure Gps_Indent_Options is

  type A_Type is
    record
      Item : Integer;
    end record;

  A : Integer := 2;
  B : Integer;

begin
  case A is
  when 2 =>
    B := 3 +
     42;

  when 3 =>
    B := 4;
  end case;
end;
-- Local Variables:
-- ada-gps-size-threshold:      0
-- ada-indent:                  2
-- ada-indent-broken:           1
-- ada-indent-record-rel-type : 2
-- ada-indent-when:             0
-- End:
