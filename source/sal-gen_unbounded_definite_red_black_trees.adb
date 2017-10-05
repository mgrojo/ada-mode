--  Abstract :
--
--  Generic unbounded red-black tree with definite elements.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body SAL.Gen_Unbounded_Definite_Red_Black_Trees is

   function Count_Tree (Item : in Node_Access) return Ada.Containers.Count_Type
   is
      use all type Ada.Containers.Count_Type;
      Result : Ada.Containers.Count_Type := 0;
   begin
      if Item.Left /= null then
         Result := Result + Count_Tree (Item.Left);
      end if;

      if Item.Right /= null then
         Result := Result + Count_Tree (Item.Right);
      end if;

      return Result + 1;
   end Count_Tree;

   procedure Left_Rotate (Tree : in out Pkg.Tree; X : in Node_Access);
   procedure Right_Rotate (Tree : in out Pkg.Tree; X : in Node_Access);

   procedure Fixup (Tree : in out Pkg.Tree; Z : in out Node_Access)
   is
      --  [1] 13.3 RB-Insert-Fixup (T, z)
      Y : Node_Access;
   begin
      while Z.Parent /= null and then Z.Parent.Color = Red loop
         if Z.Parent = Z.Parent.Parent.Left then
            Y := Z.Parent.Parent.Right;
            if Y /= null and then Y.Color = Red then
               Z.Parent.Color        := Black;
               Y.Color               := Black;
               Z.Parent.Parent.Color := Red;
               Z                     := Z.Parent.Parent;
            else
               if Z = Z.Parent.Right then
                  Z := Z.Parent;
                  Left_Rotate (Tree, Z);
               end if;
               Z.Parent.Color        := Black;
               Z.Parent.Parent.Color := Red;
               Right_Rotate (Tree, Z.Parent.Parent);
            end if;
         else
            Y := Z.Parent.Parent.Left;
            if Y /= null and then Y.Color = Red then
               Z.Parent.Color        := Black;
               Y.Color               := Black;
               Z.Parent.Parent.Color := Red;
               Z                     := Z.Parent.Parent;
            else
               if Z = Z.Parent.Left then
                  Z := Z.Parent;
                  Right_Rotate (Tree, Z);
               end if;
               Z.Parent.Color        := Black;
               Z.Parent.Parent.Color := Red;
               Left_Rotate (Tree, Z.Parent.Parent);
            end if;
         end if;
      end loop;
      Tree.Root.Color := Black;
   end Fixup;

   procedure Free_Tree (Item : in out Node_Access)
   is begin
      if Item.Left /= null then
         Free_Tree (Item.Left);
      end if;

      if Item.Right /= null then
         Free_Tree (Item.Right);
      end if;

      Free (Item);
   end Free_Tree;

   procedure Left_Rotate (Tree : in out Pkg.Tree; X : in Node_Access)
   is
      --  [1] 13.2 Left-Rotate (T, x)
      Y : constant Node_Access := X.Right;
   begin
      X.Right := Y.Left;
      if Y.Left /= null then
         Y.Left.Parent := X;
      end if;
      Y.Parent := X.Parent;
      if X.Parent = null then
         Tree.Root := Y;
      elsif X = X.Parent.Left then
         X.Parent.Left := Y;
      else
         X.Parent.Right := Y;
      end if;
      Y.Left := X;
      X.Parent := Y;
   end Left_Rotate;

   procedure Right_Rotate (Tree : in out Pkg.Tree; X : in Node_Access)
   is
      --  [1] 13.2 Right-Rotate (T, x)
      Y : constant Node_Access := X.Left;
   begin
      X.Left := Y.Right;
      if Y.Right /= null then
         Y.Right.Parent := X;
      end if;
      Y.Parent := X.Parent;
      if X.Parent = null then
         Tree.Root := Y;
      elsif X = X.Parent.Right then
         X.Parent.Right := Y;
      else
         X.Parent.Left := Y;
      end if;
      Y.Right  := X;
      X.Parent := Y;
   end Right_Rotate;

   ----------
   --  Public subprograms, spec order

   overriding procedure Finalize (Object : in out Tree)
   is begin
      if Object.Root /= null then
         Free_Tree (Object.Root);
      end if;
   end Finalize;

   function Has_Element (Cursor : in Pkg.Cursor) return Boolean
   is begin
      return Cursor.Node /= null;
   end Has_Element;

   function Constant_Reference
     (Container : aliased in Tree;
      Position  :         in Cursor)
     return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Node.all.Element'Access);
   end Constant_Reference;

   function Constant_Reference
     (Container : aliased in Tree;
      Key       :         in Key_Type)
     return Constant_Reference_Type
   is begin
      raise Not_Implemented; -- FIXME: need Find
      return (Element => Container.Root.Element'Access);
   end Constant_Reference;

   function Variable_Reference
     (Container : aliased in Tree;
      Position  :         in Cursor)
     return Variable_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Node.all.Element'Access);
   end Variable_Reference;

   function Variable_Reference
     (Container : aliased in Tree;
      Key       :         in Key_Type)
     return Variable_Reference_Type
   is begin
      raise Not_Implemented; -- FIXME: need Find
      return (Element => Container.Root.Element'Access);
   end Variable_Reference;

   type Ascending_Order_Iterator is new Iterators.Forward_Iterator with record
      Root : Node_Access;
   end record;

   overriding function First (Iterator : in Ascending_Order_Iterator) return Cursor;
   overriding function Next (Iterator : in Ascending_Order_Iterator; Position : in Cursor) return Cursor;

   function Ascending_Order (Tree : in Pkg.Tree) return Iterators.Forward_Iterator'Class
   is begin
      return Ascending_Order_Iterator'(Root => Tree.Root);
   end Ascending_Order;

   overriding function First (Iterator : in Ascending_Order_Iterator) return Cursor
   is
      Node : Node_Access := Iterator.Root;
   begin
      if Node /= null then
         loop
            exit when Node.Left = null;
            Node := Node.Left;
         end loop;
      end if;
      return
        (Node       => Node,
         Left_Done  => True,
         Right_Done => False);
   end First;

   overriding function Next (Iterator : in Ascending_Order_Iterator; Position : in Cursor) return Cursor
   is begin
      if Position.Node = null then
         return (null, True, True);

      elsif Position.Left_Done or Position.Node.Left = null then
         if Position.Right_Done or Position.Node.Right = null then
            if Position.Node.Parent = null then
               return (null, True, True);
            else
               declare
                  Node : constant Node_Access := Position.Node.Parent;
                  Temp : constant Cursor      :=
                    (Node       => Node,
                     Left_Done  => Node.Right = Position.Node or Node.Left = Position.Node,
                     Right_Done => Node.Right = Position.Node);
               begin
                  if Temp.Right_Done then
                     return Next (Iterator, Temp);
                  else
                     return Temp;
                  end if;
               end;
            end if;
         else
            declare
               Node : constant Node_Access := Position.Node.Right;
               Temp : constant Cursor      :=
                 (Node       => Node,
                  Left_Done  => Node.Left = null,
                  Right_Done => False);
            begin
               if Temp.Left_Done then
                  return Temp;
               else
                  return Next (Iterator, Temp);
               end if;
            end;
         end if;
      else
         declare
            Node : constant Node_Access := Position.Node.Left;
            Temp : constant Cursor      :=
              (Node       => Node,
               Left_Done  => Node.Left = null,
               Right_Done => False);
         begin
            if Temp.Left_Done then
               return Temp;
            else
               return Next (Iterator, Temp);
            end if;
         end;
      end if;
   end Next;

   function Count (Tree : in Pkg.Tree) return Ada.Containers.Count_Type
   is begin
      if Tree.Root = null then
         return 0;
      else
         return Count_Tree (Tree.Root);
      end if;
   end Count;

   procedure Insert (Tree : in out Pkg.Tree; Element : in Element_Type)
   is
      --  [1] 13.3 RB-Insert (T, z)
      Z : Node_Access := new Node'(Element, null, null, null, Red);
      Y : Node_Access := null;
      X : Node_Access := Tree.Root;
   begin
      while X /= null loop
         Y := X;
         if Key (Z.Element) < Key (X.Element) then
            X := X.Left;
         else
            X := X.Right;
         end if;
      end loop;

      Z.Parent := Y;
      if Y = null then
         Tree.Root := Z;
      elsif Key (Z.Element) < Key (Y.Element) then
         Y.Left := Z;
      else
         Y.Right := Z;
      end if;

      if Z = Tree.Root then
         Z.Color := Black;
      else
         Fixup (Tree, Z);
      end if;
   end Insert;

end SAL.Gen_Unbounded_Definite_Red_Black_Trees;
