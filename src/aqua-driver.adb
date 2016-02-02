with Aqua.CPU;
with Aqua.Images;
with Aqua.IO;
with Aqua.Loaders;
with Aqua.Primitives.Init;

with Aqua.Paths;

procedure Aqua.Driver is
begin
   Aqua.IO.Set_IO_Path (Aqua.Paths.Config_Path);
   Aqua.Primitives.Init.Create_Primitives;

   declare
      Loader : constant Aqua.Loaders.Null_Loader_Access :=
                 new Aqua.Loaders.Null_Loader;
      Image  : constant Aqua.Images.Image_Type := Aqua.Images.New_Image;
      CPU    : Aqua.CPU.Aqua_CPU_Type (Image, Loader);
   begin
      Image.Load ("test.o32");
      Image.Link;
      CPU.Execute (16#1000#, (1 => 0));
   end;

end Aqua.Driver;
