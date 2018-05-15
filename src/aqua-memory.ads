private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Doubly_Linked_Lists;

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

   function Can_Read
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Boolean;

   function Can_Write
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Boolean;

   function Can_Execute
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Boolean;

   function Used_Memory
     (Memory : Memory_Type'Class)
      return Natural;

   procedure Install_Driver
     (Memory : in out Memory_Type'Class;
      Start  : Address;
      Driver : Aqua.Drivers.Aqua_Driver);

   procedure Set_Access_Flags
     (Memory  : in out Memory_Type'Class;
      Base    : Address;
      Bound   : Address;
      R, W, X : Boolean);

   procedure Set_Flags
     (Memory  : in out Memory_Type'Class;
      Addr    : Address;
      R, W, X : Boolean := False);

   procedure Add_Monitor
     (Memory  : in out Memory_Type'Class;
      Addr    : Address);

   procedure Remove_Monitor
     (Memory : in out Memory_Type'Class;
      Addr   : Address);

   procedure Begin_Transaction (Memory : in out Memory_Type'Class);
   procedure End_Transaction (Memory : in out Memory_Type'Class);

private

   Page_Bits : constant := 12;
   Page_Size : constant := 2 ** Page_Bits;
   subtype Page_Address_Range is Address range 0 .. Page_Size - 1;
   function Page_Address (Addr : Address) return Page_Address_Range
   is (Addr mod Page_Size);

   Directory_Bits : constant := 12;
   Directory_Size : constant := 2 ** Directory_Bits;
   subtype Directory_Address_Range is Address range 0 .. Directory_Size - 1;
   function Directory_Address (Addr : Address) return Directory_Address_Range
   is (Addr / Page_Size mod Directory_Size);

   Table_Bits : constant := Word_Size - Directory_Bits - Page_Bits;
   Table_Size : constant := 2 ** Table_Bits;
   subtype Table_Address_Range is Address range 0 .. Table_Size - 1;
   function Table_Address (Addr : Address) return Table_Address_Range
   is (Addr / Directory_Size / Page_Size);

   type Page_Data is array (Page_Address_Range) of Octet;

   type Page_Flag is
     (Flag_P, Flag_U1, Flag_U2, Flag_Monitor,
      Flag_R, Flag_W, Flag_X, Flag_Driver);

   type Page_Flags is array (Page_Flag) of Boolean with Component_Size => 1;

   type Directory_Page_Flags is array (Directory_Address_Range) of Page_Flags
     with Component_Size => 8;

   package Driver_Maps is
     new Ada.Containers.Ordered_Maps (Address, Aqua.Drivers.Aqua_Driver,
                                      "=" => Aqua.Drivers."=");

   type Page_Type is
      record
         Data       : Page_Data;
         Driver_Map : Driver_Maps.Map;
      end record;

   type Page_Access is access Page_Type;

   type Directory_Page_Array is
     array (Directory_Address_Range) of Page_Access;

   type Directory_Entry is
      record
         Pages : Directory_Page_Array;
         Flags : Directory_Page_Flags;
      end record;

   type Directory_Access is access Directory_Entry;

   type Table_Type is array (Table_Address_Range) of Directory_Access;

   package Changed_Driver_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Aqua.Drivers.Aqua_Driver, Aqua.Drivers."=");

   package Monitored_Addresses is
     new Ada.Containers.Doubly_Linked_Lists (Address);

   type Memory_Type is tagged
      record
         Table      : Table_Type;
         Page_Count : Natural := 0;
         Changes    : Changed_Driver_Lists.List;
         Monitors   : Monitored_Addresses.List;
      end record;

   function Flag_Is_Set
     (Memory : Memory_Type'Class;
      Addr   : Address;
      Flag   : Page_Flag)
      return Boolean;

   procedure Set_Flag
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Flag   : Page_Flag;
      Value  : Boolean);

   function Get_Page
     (Mem : Memory_Type;
      Addr : Address)
      return Page_Access;

   procedure Ensure_Page
     (Mem : in out Memory_Type;
      Addr : Address);

   function Used_Memory
     (Memory : Memory_Type'Class)
      return Natural
   is (Memory.Page_Count * Page_Size);

   function Can_Read
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Boolean
   is (Memory.Flag_Is_Set (Addr, Flag_R));

   function Can_Write
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Boolean
   is (Memory.Flag_Is_Set (Addr, Flag_W));

   function Can_Execute
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Boolean
   is (Memory.Flag_Is_Set (Addr, Flag_X));

end Aqua.Memory;
