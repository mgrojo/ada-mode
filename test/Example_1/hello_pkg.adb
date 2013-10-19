with Ada.Text_IO;
package Hello_Pkg is
   procedure Say_Hello
   is begin
      Ada.Text_IO.Put_Line ("Hello from hello_pkg.adb");
   end Say_Hello;
end Hello_Pkg;
