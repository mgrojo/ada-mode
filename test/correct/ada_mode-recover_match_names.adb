pragma License (Modified_Gpl);

package body Ada_Mode.Recover_Match_Names is

   task body Accept_Statement_0
   is
      A : Integer;
   begin
      accept Start do
         loop
         end loop;
      end Start;
   end Accept_Statement_0;

   procedure Block_Statement_0
   is
      A : Integer;
   begin
   Block_1:
      declare
      begin
         loop
         end loop;
      end Block_1;
   end Block_Statement_0;

   procedure Block_Statement_1
   is
      A : Integer;
   begin
   Block_1:
      begin
         loop
         end loop;
      end Block_1;
   end Block_Statement_1;

   protected body Entry_Body_0
   is
      entry E1 when A is
      begin
         loop
         end loop;
      end E1;
   end Entry_Body_0;

   procedure Loop_Statement_0
   is
      A : Integer;
   begin
   Loop_1 :
      for I in B'Range loop
         if then
         end if;
      end loop Loop_1;
   end Loop_Statement_0;

   procedure Loop_Statement_1
   is
      A : Integer;
   begin
   Loop_1 :
      loop
         exit when C;
         if then
         end if;
      end loop Loop_1;
   end Loop_Statement_1;

   package body Package_Body_0 is
      A : Integer;
   begin -- for package_body_1
      loop
      end loop;
   end Package_Body_0;

   package body Package_Body_1 is
      procedure A
      is
         B : Integer;
      begin
         C;
      end A;
   end Package_Body_1;

   package Package_Specification_0 is
      A : Integer;
   private
      type A is record
      end record;
   end Package_Specification_0;

   package Package_Specification_1 is
      A : Integer;
      type A is record
      end record;
   end Package_Specification_1;

   protected body Protected_Body_0 is
      entry E2 when True is begin end E2;
      entry E1 when True is begin
      end E1;
   end Protected_Body_0;

   procedure Subprogram_Body_0
   is
      A : Integer;
   begin
      loop
      end loop;
   end Subprogram_Body_0;

   task body Task_Body_0
   is
      A : Integer;
   begin
      loop
      end loop;
   end Task_Body_0;

end Ada_Mode.Recover_Match_Names;
