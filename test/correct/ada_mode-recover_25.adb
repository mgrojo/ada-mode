procedure Ada_Mode.Recover_25
is
   procedure Put_Usage
   is
      use Ada.Text_IO;
   begin
   end Put_Usage;

   File_Name : constant String := Ada.Command_Line.Argument (1);
   File : SMM.ID3.File;

   Frames : SMM.ID3.Frame_Lists.List;
begin
   File.Open (File_Name);

   Frames := File.All_Frames;

   for Frame of Frames loop
      Put_Line (Frame.ID & " '" & (-Frame.Data) & "'");
   end loop;

   File.Close;
end Ada_Mode.Recover_25;
