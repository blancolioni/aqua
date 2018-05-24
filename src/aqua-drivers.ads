package Aqua.Drivers is

   type Driver_Register_Count is new Address range 0 .. 4096;
   subtype Driver_Register_Range is Driver_Register_Count range 0 .. 4095;

   type Root_Aqua_Driver
     (Last_Address : Driver_Register_Range)
   is abstract tagged private;

   type Aqua_Driver is access all Root_Aqua_Driver'Class;

   procedure Update
     (Driver : in out Root_Aqua_Driver)
   is abstract;

   function Monitored
     (Driver   : Root_Aqua_Driver;
      Register : Driver_Register_Range)
      return Boolean;

   function Get_Octet
     (Driver : Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range)
      return Octet;

   procedure Set_Octet
     (Driver : in out Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range;
      Value  : Octet);

   function Get_Word
     (Driver : Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range)
      return Word;

   procedure Set_Word
     (Driver : in out Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range;
      Value  : Word);

   function Changed_Octet
     (Driver : Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range)
      return Boolean;

   function Changed_Word
     (Driver : Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range)
      return Boolean;

   function Changed_Range
     (Driver : Root_Aqua_Driver'Class;
      Base   : Driver_Register_Range;
      Bound  : Driver_Register_Count)
      return Boolean;

   procedure Clear_Changes
     (Driver : in out Root_Aqua_Driver'Class);

   function Text_Writer return Aqua_Driver;
   function Character_Handling return Aqua_Driver;

private

   type Device_Registers is
     array (Driver_Register_Range range <>) of Octet;

   type Register_Flags is
     array (Driver_Register_Range range <>) of Boolean;

   type Root_Aqua_Driver
     (Last_Address : Driver_Register_Range)
   is abstract tagged
      record
         Rs      : Device_Registers (0 .. Last_Address) := (others => 0);
         Changed : Register_Flags (0 .. Last_Address)   := (others => False);
      end record;

   function Monitored
     (Driver   : Root_Aqua_Driver;
      Register : Driver_Register_Range)
      return Boolean
   is (True);

   function Changed_Range
     (Driver : Root_Aqua_Driver'Class;
      Base   : Driver_Register_Range;
      Bound  : Driver_Register_Count)
      return Boolean
   is (Bound > Base
       and then (for some X in Base .. Bound - 1 => Driver.Changed (X)));

   function Changed_Octet
     (Driver : Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range)
      return Boolean
   is (Driver.Changed_Range (Addr, Addr + 1));

   function Changed_Word
     (Driver : Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range)
      return Boolean
   is (Driver.Changed_Range ((Addr / 4) * 4, (Addr / 4) * 4 + 4));

   function Get_Octet
     (Driver : Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range)
      return Octet
   is (Driver.Rs (Addr));

end Aqua.Drivers;
