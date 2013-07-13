-- Just keep the compiler happy
package Mats_Weber_Bugs is
   Error : exception;

   package Test_3 is
      task Yy;
      package P is
         procedure Q;
      end P;
   end Test_3;

end Mats_Weber_Bugs;
