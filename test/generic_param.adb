--  Tests the indentation after a generic statement and some faces

--EMACSCMD:(jit-lock-fontify-now)

procedure Generic_Param is

   generic
      type Item_T is private;
      --EMACSCMD:(test-face "all" 'font-lock-keyword-face)
      type Item_Ptr_T is access all Item_T;
      type A (<>) is private;
   package Generic_List_Unbounded_Double is
      type Handle_T is tagged null record;
      procedure Append (The_Item : in Item_T;
                        To_The_List : in out Handle_T);
   end Generic_List_Unbounded_Double;

   package body Generic_List_Unbounded_Double is
      procedure Append (The_Item : in Item_T; To_The_List : in out Handle_T) is
      begin
         --gdb: break 19
         null;
         --gdb: print the_item
      end Append;
   end Generic_List_Unbounded_Double;


   package Route_Item is
      type Item_Type_T is (Route, Fix, Airport);
      type Rte_Item_T (Item_Type : Item_Type_T := Fix) is record
         case Item_Type is
            when Fix | Airport =>
               null;
            when Route =>
               null;
         end case;
      end record;
      type Rte_Item_Ptr_T is access all Rte_Item_T;
   end Route_Item;

   --EMACSCMD:(test-face "Generic_List_Unbounded_Double" 'font-lock-function-name-face)
   package Route_Item_List is new Generic_List_Unbounded_Double
     (Route_Item.Rte_Item_T, Route_Item.Rte_Item_Ptr_T, Integer);

   List : Route_Item_List.Handle_T;
   Item : Route_Item.Rte_Item_T;

begin
   Route_Item_List.Append (Item, List);
end Generic_Param;
