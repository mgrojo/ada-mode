--  Abstract :
--
--  Test automatically converting object.method to method.object.
--

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)

package body Ada_Mode.Refactor_Object_Method_To_Method_Object is

   --EMACSCMD:(test-moom "Post" "Length (Container)")
   function Is_Full (Container : in Vector) return Boolean with
     Post => Is_Full'Result = (Length (Container) = Capacity);
   --EMACSCMD:(test-refactor-inverse)

   --EMACSCMD:(test-moom "Post" "Length (\"&\"'Result)")
   --EMACSCMD:(test-moom "for all I" "Element (\"&\"'Result")
   --EMACSCMD:(test-moom ", J)) and" "Element (\"&\"'Result")
   function "&" (Left : in Vector; Right : in Element_Type) return Vector with
     Post => Length ("&"'Result) = Length (Left) + 1 and
             (for all I in Index_Type'First .. Last_Index (Left) => Element (Left, I) = Element ("&"'Result, "&"'Result, I, J)) and
             Element ("&"'Result, Last_Index ("&"'Result)) = Right;
   --EMACSCMD:(test-refactor-inverse)

   --EMACSCMD:(test-moom "Pre" "Length (Container)")
   --EMACSCMD:(test-moom "Post" "Length (Container)'Old")
   --EMACSCMD:(test-moom "Post" "Element (Container, Last_Index (Container))")
   --EMACSCMD:(test-moom "for all I" "Element (Container'Old, I)")
   procedure Append (Container : in out Vector; New_Item : in Element_Type) with
     Pre  => Container.Length < Capacity,
     Post => Length (Container) = Length (Container)'Old + 1 and
             Element (Container, Last_Index (Container)) = New_Item and
             (for all I in Index_Type'First .. Last_Index (Container) - 1 =>
                Element (Container'Old, I) = Element (Container, I));
   --EMACSCMD:(test-refactor-inverse)

   procedure Parse_One_Item
   is begin
      loop
         --EMACSCMD:(test-moom "then" "Length (Config.Insert_Delete)")
         exit when not Success or
           Action.Item.Verb = Accept_It or
           (if Shared_Token_Goal = Invalid_Token_Index
            then Length (Config.Insert_Delete) = 0
            else Config.Current_Shared_Token > Shared_Token_Goal);
         --EMACSCMD:(test-refactor-inverse)
      end loop;
   end Parse_One_Item;

end Ada_Mode.Refactor_Object_Method_To_Method_Object;
