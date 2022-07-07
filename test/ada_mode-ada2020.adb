package body Ada_Mode.Ada2020 is

   function Creator_1 (Index : Worker_Indexes) return Worker is
   begin
      return Worker (Index);
   end Creator_1;

   function Creator_2 (Index : Worker_Indexes) return Worker is
   begin
      return -Worker (Index);
   end Creator_2;

   function Func_Declare_1 (A : in Integer) return Integer
     is (declare
            B : Integer renames A;
         begin
            B * 2 +
              --  Comment in expression
              4);

   --  Declare expression with constant in precondition.
   procedure Absolute_Difference
     (X, Y :     Integer;
      Diff : out Integer)
   with Post =>
     (declare
         Min : constant Integer := Integer'Min (X, Y);
         Max : constant Integer := Integer'Max (X, Y);
      begin Diff = Max - Min)
    is begin
       Diff := abs X - Y;
    end Absolute_Difference;

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
   A := [for Item in Worker'Range => 1]'Reduce ("+", 0);

   --  [2] 4.4 declare_expresssion
   A :=
     (declare B : constant Integer :=
        3;
      begin
         B + 1);

   A := Func_Declare_1
     (declare C : Integer renames
           A;
      begin C + 2);

end Ada_Mode.Ada2020;
