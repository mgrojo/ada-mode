--  Indentation is sometimes changed when there are comments in
--  the middle

package Adacore_8114_010 is

   type Correct_Indentation is
     (
      Value_1,
      Value_2
     );

   type Wrong_Indentation is
     (
      Value_1,
      Value_2
     );

   type Wrong_Indentation_Two is
     (  Value_0,
      Value_1,
      Value_2
     );


   procedure Correct_Sub
     (
      Prm : Integer
     );

   procedure Incorrect_Sub
     --  This comment used to make the Prm parameter to be wrongly indented
     (
      Prm : Integer -- indentation changed
     );

end Adacore_8114_010;
