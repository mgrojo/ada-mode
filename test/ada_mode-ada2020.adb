package body Ada_Mode.Ada2020 is

   function Creator_1 (Index : Worker_Indexes) return Worker is
   begin
      return Worker (Index);
   end Creator_1;

   function Creator_2 (Index : Worker_Indexes) return Worker is
   begin
      return -Worker (Index);
   end Creator_2;

   type Task_Array is array (Worker_Indexes) of Worker;

   --  [1] ai12-0061-1.txt
   --  [2] 0.2 Language Changes array aggregate index parameter
   --  [2] 4.3.3 5.1 iterated_component_association with discrete_choice_list
   --  [2] 5.5 parallel in iteration_scheme
   Worker_Tasks : Task_Array :=
     (for Index in 1 ..  5 => Creator_1 (Index),
      for Index in 6 .. 10 => Creator_2 (Index));

   A : Integer := 1;
begin
   --  [1] ai12-0125-3.txt
   --  [2 Language changes '@']
   --  [2] 5.2.1 target_name
   A := @ + 1;

   --  [1] ai12-0127-1.txt
   --  [2] 4.3.4 array_delta_aggregate
   Worker_Tasks :=
     (@ with delta
      4 => Creator_2 (4),
      7 => Creator_1 (7));

   --  [2] 0.2 Language Changes reduction expressions
   --  [2] 4.5.10 reduction expression
   --
   --  Count items in A
   A := [for Item in Iterator => 1]'Reduce ("+", 0);
end Ada_Mode.Ada2020;
