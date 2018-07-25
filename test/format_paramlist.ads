--  Let the body compile
package Format_Paramlist is

   type Z is tagged record
      Z_Int : Integer;
   end record;


   Default_Z : constant Z := (others => <>);

   procedure X (Y : in     Z 'Class := Default_Z;
                B : access Integer;
                A :    out Integer);

   type Tupple_Type is record
      A : Integer;
      B : Integer;
   end record;

   --  Aggregate in default expression.
   procedure Put_File_Header
     (Comment_Syntax : in String;
      Tuple          : in Tupple_Type := (others => <>));

end Format_Paramlist;
