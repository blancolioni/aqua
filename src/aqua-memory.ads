private with Ada.Containers.Ordered_Maps;
with Aqua.Drivers;

package Aqua.Memory is

   type Memory_Type is tagged private;

   function Get_Octet
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Octet
     with Inline_Always;

   procedure Set_Octet
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Value  : Octet)
     with Inline_Always;

   function Get_Value
     (Memory : Memory_Type'Class;
      Addr   : Address;
      Size   : Data_Size)
      return Word
     with Inline_Always;

   procedure Set_Value
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Size   : Data_Size;
      Value  : Word)
     with Inline_Always;

   function Get_Word
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Word
   is (Get_Value (Memory, Addr, Word_32_Size))
   with Inline_Always;

   procedure Set_Word
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Value  : Word)
     with Inline_Always;

   function Used_Memory
     (Memory : Memory_Type'Class)
      return Natural;

   procedure Install_Driver
     (Memory : in out Memory_Type'Class;
      Start  : Address;
      Driver : Aqua.Drivers.Aqua_Driver);

private

   Page_Bits : constant := 12;
   Page_Size : constant := 2 ** Page_Bits;
   subtype Page_Address is Address range 0 .. Page_Size - 1;

   Directory_Bits : constant := Payload_Bits - Page_Bits;
   Directory_Size : constant := 2 ** Directory_Bits;
   subtype Directory_Address is Address range 0 .. Directory_Size - 1;

   type Page_Data is array (Page_Address) of Octet;

   type Page_Flag is (Flag_R, Flag_W, Flag_X, Flag_Driver);

   type Page_Flags is array (Page_Flag) of Boolean with Component_Size => 1;

   package Driver_Maps is
     new Ada.Containers.Ordered_Maps (Address, Aqua.Drivers.Aqua_Driver,
                                      "=" => Aqua.Drivers."=");

   type Page_Type is
      record
         Data       : Page_Data;
         Driver_Map : Driver_Maps.Map;
         Flags      : Page_Flags;
      end record;

   type Page_Access is access Page_Type;

   type Directory_Type is array (Directory_Address) of Page_Access;

   type Memory_Type is tagged
      record
         Directory  : Directory_Type;
         Page_Count : Natural := 0;
      end record;

   function Flag_Is_Set
     (Memory : Memory_Type'Class;
      Addr   : Address;
      Flag   : Page_Flag)
      return Boolean
   is (Memory.Directory (Addr / Page_Size) /= null
       and then Memory.Directory (Addr / Page_Size).Flags (Flag));

   procedure Set_Flag
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Flag   : Page_Flag;
      Value  : Boolean);

   function Get_Page
     (Mem : Memory_Type;
      Addr : Address)
      return Page_Access
   is (Mem.Directory (Addr / Page_Size));

   procedure Ensure_Page
     (Mem : in out Memory_Type;
      Addr : Address);

   function Used_Memory
     (Memory : Memory_Type'Class)
      return Natural
   is (Memory.Page_Count * Page_Size);

end Aqua.Memory;
