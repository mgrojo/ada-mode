-- Example statements that get "too many parallel parsers required" with
-- default 20 limit
--
-- FIXME: the number of parsers required grows geometrically with the
-- nesting level, not the number of elements in the aggregate. So
-- increasing wisi-parse-max-parallel is not a long-term solution; we
-- need to optimize the grammar in this area. ada-mode 7.1.6 did not
-- have this problem.
procedure Ada_Mode.Nested_Parens
is
   -- From Eurocontrol
   Headers : aliased constant Headers_T
     := ((new String'("ARCID"), ((Field_T'Pos (Aircraft),      Text_Kind), others => None_Column), True),
         (new String'("Aty"),   ((Field_T'Pos (Aircraft_Type), Text_Kind), others => None_Column), True),
         (new String'("Aty"),   ((Field_T'Pos (Aircraft_Type), Text_Kind), others => None_Column), True));
begin
   --  From WisiToken generated subprograms actions.
   Indent_Action_0
     (Parse_Data, Tree, Nonterm,
      (T1 => (False, (Simple, (Label => None))),
       T2 => (False, (Simple, (Label => None))),
       T3 => (False, (Simple, (Label => None))),
       T4 => (False, (Simple, (Int, Subp_Indent))),
       T5 => (False, (Simple, (Label => None))),
       T6 => (False, (Simple, (Int, Subp_Indent))),
       T7 => (False, (Simple, (Label => None))),
       T8 => (False, (Simple, (Label => None))),
       T9 => (False, (Simple, (Label => None)))));
end Ada_Mode.Nested_Parens;
