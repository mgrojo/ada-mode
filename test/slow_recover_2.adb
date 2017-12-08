--  real example of slow error recovery

pragma License (Modified_GPL);

package body SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci is

   ----------
   --  Body operations, alphabetical order

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   procedure Free_Node (Item : in out Node_Access)
   is begin

   end Free_Node;


   ----------
   --  Visible operations

   overriding
   procedure Initialize (Object : in out Heap_Type)
   is begin
      --  Min is null by default.
      Object.Count := 0;
   end Initialize;

   overriding
   procedure Finalize (Object : in out Heap_Type)
   is begin
      Free_Node (Object.Min);
      Object.Count := 0;
   end Finalize;

   procedure Clear (Heap : in out Heap_Type)
   is begin
      Heap.Count := 0;
   end Clear;

   function Count (Heap : in Heap_Type) return Base_Peek_Type
   is begin
      return Heap.Count;
   end Count;

   function Remove (Heap : in out Heap_Type) return Element_Type
   is
      Z : Node_Access := Heap.Min;
      X : Node_Access;
   begin
      if Heap.Count = 0 then
         raise Container_Empty;
      end if;

      --  [1] 19.2 FIB-HEAP-EXTRACT-MIN
      X := Z.Child;
      for I in 1 .. Z.Degree loop
         --  Insert child X into Heap root list
         X.Right       := Heap.Min;
         X.Left        := Heap.Min.Left;
         Heap.Min.Left := X;

         X := X.Right;
         loop;

   end Remove;

   function Min_Key (Heap : in out Heap_Type) return Key_Type
   is begin
      return Key (Heap.Min.Element);
   end Min_Key;

   procedure Drop (Heap : in out Heap_Type)
   is
      Junk : Element_Type := Remove (Heap);
      pragma Unreferenced (Junk);
   begin
      null;
   end Drop;

   procedure Add (Heap : in out Heap_Type; Item : in Element_Type)
   is
      X       : Node_Access := new Node'(Item, null, null, null, null, 0, False);
   begin
      --  [1] 19.2 FIB-HEAP-INSERT
      if Heap.Min = null then
         Heap.Min := X;
      else
         --  match [1] fig 19.3
         X.Right       := Heap.Min;
         X.Left        := Heap.Min.Left;
         Heap.Min.Left := X;

         if Key (Item) < Key (Old_Min.Element) then
            H.Min := X;
         end if;
      end if;
      Heap.Count := Heap.Count + 1;
   end Add;

end SAL.Gen_Unbounded_Definite_Min_Heaps;
