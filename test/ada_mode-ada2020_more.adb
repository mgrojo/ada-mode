-- Testing ada2020 features not yet supported by GNAT Community
--
-- This file will be merged with ada_mode-ada2020 when GNAT Community
-- supports all features.

procedure Ada_Mode.Ada2020_More
is
   --  [1] ai12-0061-1.txt
   --  [2 Language Changes] array aggregate index parameter
   --  [2] 4.3.3 5.1 iterated_component_association with discrete_choice_list
   --  [2] 5.5 iterator_filter
   Worker_Tasks : Task_Array :=
     (for Index in when Index <= 5 => Creator_1 (Index),
      for Index in when Index >  5 => Creator_2 (Index));
begin
   --  [1] ai12-0127-1.txt
   --  [2] 4.3.4 array_delta_aggregate
   Worker_Tasks :=
     [@ with delta
      4 =>  Creator_2 (4),
      7 => Creator_1 (7)];
end Ada_Mode.Ada2020_More;
