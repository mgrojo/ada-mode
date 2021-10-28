-- From Eurocontrol; gets "too many parallel parsers required" with
-- default 20 limit
--
-- FIXME: the number of parsers required grows geometrically with the
-- nesting level, not the number of elements in the aggregate. So
-- increasing wisi-parse-max-parallel is not a long-term solution; we
-- need to optimize the grammar in this area. ada-mode 7.1.6 did not
-- have this problem.
procedure Ada_Mode.Nested_Parens
is
   Headers : aliased constant Headers_T
     := ((new String'("ARCID"), ((Field_T'Pos (Aircraft),      Text_Kind), others => None_Column), True),
         (new String'("Aty"),   ((Field_T'Pos (Aircraft_Type), Text_Kind), others => None_Column), True),
         (new String'("Aty"),   ((Field_T'Pos (Aircraft_Type), Text_Kind), others => None_Column), True));
begin
   null;
end Ada_Mode.Nested_Parens;
-- Local Variables:
-- wisi-parse-max-parallel: 40
-- End:
