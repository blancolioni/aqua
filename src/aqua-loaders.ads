with Aqua.Execution;

package Aqua.Loaders is

   type Null_Loader is new Aqua.Execution.Loader_Interface with private;

   overriding function Load_Object
     (Loader    : in out Null_Loader;
      File_Name : String)
      return access External_Object_Interface'Class
   is (null);

   type Null_Loader_Access is access all Null_Loader'Class;

private

   type Null_Loader is new Aqua.Execution.Loader_Interface with null record;

end Aqua.Loaders;
