package body Ada_Mode.Recover_Change_Name is

   procedure New_Name_1
   is
      A : Integer;
      B : Integer;

   begin -- New_Name_1
      A := B;
      A := B;
   end New_Name_1;

   procedure Same_Name_2
   is
      A : Integer;
      B : Integer;

   begin -- Same_Name_2
      A := B;
      A := B;
   end Same_Name_2;

   procedure New_Name_3
   is
      A : Integer;
      B : Integer;
   begin -- New_Name_3
      A := B;
      A := B;
   end New_Name_3;

end Ada_Mode.Recover_Change_Name;
