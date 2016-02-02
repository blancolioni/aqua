private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Fixed.Less_Case_Insensitive;
private with Ada.Strings.Unbounded;

private with Aqua.Memory;

with Aqua.Architecture;

package Aqua.Assembler is

   type Root_Assembly_Type is
     new Memory_Interface with private;

   procedure Start (A          : in out Root_Assembly_Type);

   procedure Set_Source_File
     (A    : in out Root_Assembly_Type;
      Path : String);

   procedure Set_Source_Location
     (A      : in out Root_Assembly_Type;
      Line   : Natural;
      Column : Natural);

   procedure Append_Octet
     (A : in out Root_Assembly_Type'Class;
      X : Octet);

   procedure Append_Word
     (A : in out Root_Assembly_Type'Class;
      W : Word);

   procedure Append
     (A    : in out Root_Assembly_Type'Class;
      W    : Word;
      Size : Aqua.Data_Size);

   procedure Bind_Action
     (A      : in out Root_Assembly_Type'Class;
      Group  : String;
      Before : Boolean;
      Parent : String;
      Child  : String);

   function Reference_Label
     (A        : in out Root_Assembly_Type'Class;
      Name     : String;
      Relative : Boolean)
      return Word;

   function Reference_Temporary_Label
     (A       : in out Root_Assembly_Type'Class;
      Label   : Natural;
      Forward : Boolean)
      return Word;

   function Reference_Branch_Label
     (A        : in out Root_Assembly_Type'Class;
      Name     : String)
      return Word;

   function Reference_Temporary_Branch_Label
     (A       : in out Root_Assembly_Type'Class;
      Label   : Natural;
      Forward : Boolean)
      return Word;

   function Reference_String
     (A : in out Root_Assembly_Type'Class;
      X : String)
      return Word;

   function Reference_Property_Name
     (A    : in out Root_Assembly_Type'Class;
      Name : String)
      return Word;

   procedure Define_Label
     (A    : in out Root_Assembly_Type'Class;
      Name : String);

   procedure Define_External_Label
     (A    : in out Root_Assembly_Type'Class;
      Name : String);

   procedure Define_Exported_Label
     (A    : in out Root_Assembly_Type'Class;
      Name : String);

   procedure Define_Temporary_Label
     (A     : in out Root_Assembly_Type'Class;
      Label : Natural);

   procedure Define_Value
     (A     : in out Root_Assembly_Type'Class;
      Name  : String;
      Value : Word);

   procedure Define_Name
     (A     : in out Root_Assembly_Type'Class;
      Name  : String;
      Value : String);

   function Is_Defined
     (A    : Root_Assembly_Type'Class;
      Name : String)
      return Boolean;

   function Get_Value
     (A    : Root_Assembly_Type'Class;
      Name : String)
      return Word;

   function Is_Register
     (A : Root_Assembly_Type'Class;
      Name : String)
      return Boolean;

   function Get_Register
     (A : Root_Assembly_Type'Class;
      Name : String)
      return Aqua.Architecture.Register_Index
     with Pre => A.Is_Register (Name);

   procedure Write_Listing (A : Root_Assembly_Type'Class);

   procedure Write_Image
     (A : Root_Assembly_Type'Class;
      Path : String);

   type Assembly is access all Root_Assembly_Type'Class;

private

   type Reference_Info is
      record
         Addr     : Address;
         Relative : Boolean;
         Branch   : Boolean;
      end record;

   package Label_Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Reference_Info);

   type Label_Info is
      record
         References      : Label_Reference_Lists.List;
         Defined         : Boolean := False;
         External        : Boolean := False;
         Register_Alias  : Boolean := False;
         String_Constant : Boolean := False;
         Value           : Word    := 0;
      end record;

   package Label_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type        => String,
        Element_Type    => Label_Info,
        "<"             => Ada.Strings.Fixed.Less_Case_Insensitive);

   package Temporary_Label_Vectors is
     new Ada.Containers.Vectors (Natural, Natural);

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Natural, String);

   type Binding_Info is
      record
         Group_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Start       : Address;
         Before      : Boolean;
         Parent_Text : Ada.Strings.Unbounded.Unbounded_String;
         Child_Text  : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Binding_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Binding_Info);

   type Source_Position is
      record
         Start    : Address;
         Line     : Natural;
         Column   : Natural;
      end record;

   package Source_Position_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Source_Position);

   type Root_Assembly_Type is
     new Memory_Interface with
      record
         Source_Path    : Ada.Strings.Unbounded.Unbounded_String;
         Source_Locs    : Source_Position_Lists.List;
         Low            : Address := Address'Last;
         High           : Address := 0;
         PC             : Address := 0;
         Next_String    : Word    := 0;
         Memory         : Aqua.Memory.Memory_Type;
         Labels         : Label_Maps.Map;
         Temporaries    : Temporary_Label_Vectors.Vector;
         String_Lits    : String_Vectors.Vector;
         Bindings       : Binding_Info_Vectors.Vector;
      end record;

   overriding function Get_Octet
     (Assembly : Root_Assembly_Type;
      Addr     : Address)
      return Octet
     is (Assembly.Memory.Get_Octet (Addr));

   overriding procedure Set_Octet
     (Assembly : in out Root_Assembly_Type;
      Addr   : Address;
      Value  : Octet);

   procedure Ensure_Label
     (A         : in out Root_Assembly_Type'Class;
      Name      : String;
      Is_String : Boolean);

end Aqua.Assembler;
