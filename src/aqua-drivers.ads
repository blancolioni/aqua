package Aqua.Drivers is

   type Aqua_Driver_Interface is interface;

   type Aqua_Driver is access all Aqua_Driver_Interface'Class;

   function Address_Count (Driver : Aqua_Driver_Interface) return Address
                           is abstract;

   function Get_Octet
     (Driver : Aqua_Driver_Interface;
      Addr   : Address)
      return Octet
      is abstract;

   procedure Set_Octet
     (Driver : in out Aqua_Driver_Interface;
      Addr   : Address;
      Value  : Octet)
   is abstract;

   function Text_Writer return Aqua_Driver;

end Aqua.Drivers;
