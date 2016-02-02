with Aqua.Architecture;

package Aqua.CPU.Traps is

   procedure Handle_Get_Property
     (CPU            : in out Aqua_CPU_Type'Class;
      Argument_Count : Natural;
      Property_Name  : Word);

   procedure Handle_Set_Property
     (CPU  : in out Aqua_CPU_Type'Class;
      Name : Word);

   procedure Handle_Iterator_Start
     (CPU   : in out Aqua_CPU_Type'Class);

   procedure Handle_Iterator_Next
     (CPU   : in out Aqua_CPU_Type'Class;
      R     : Aqua.Architecture.Register_Index);

end Aqua.CPU.Traps;
