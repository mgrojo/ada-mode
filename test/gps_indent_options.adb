--  Demonstrate that gps indentation engine respects ada-indent,
--  ada-broken-indent, ada-indent-when

procedure Gps_Indent_Options is

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
-- eval: (ada-gps-setup)
-- ada-indent:        2
-- ada-indent-broken: 1
-- ada-indent-when:   0
-- End:
