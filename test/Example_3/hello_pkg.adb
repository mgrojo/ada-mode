with Ada.Text_Io;
package Hello_Pkg is
   procedure Say_Hello
   is begin
      Ada.Text_Io.Put_Line ("Hello from hello_pkg.adb");
   end Say_Hello;
end Hello_Pkg;
