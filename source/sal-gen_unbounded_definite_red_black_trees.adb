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
   --  Public subprograms

   overriding procedure Finalize (Object : in out Tree)
   is begin
      if Object.Root /= null then
         Free_Tree (Object.Root);
      end if;
   end Finalize;

   function Constant_Reference
     (Container : aliased in Tree;
      Position  :         in Cursor)
     return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Node.all.Element'Access);
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
