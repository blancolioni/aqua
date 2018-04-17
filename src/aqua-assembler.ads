private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Fixed.Less_Case_Insensitive;
private with Ada.Strings.Unbounded;

with Aqua.Memory;

with Aqua.Architecture;

package Aqua.Assembler is

   type Root_Assembly_Type is
     new Aqua.Memory.Memory_Type with private;

   procedure Start (A          : in out Root_Assembly_Type);

   procedure Set_Source_File
     (A    : in out Root_Assembly_Type;
      Path : String);

   procedure Set_Source_Location
     (A      : in out Root_Assembly_Type;
      Line   : Natural;
      Column : Natural);

   procedure Set_Segment
     (A    : in out Root_Assembly_Type;
      Name : String);

   procedure Code_Segment
     (A    : in out Root_Assembly_Type);

   procedure Text_Segment
     (A    : in out Root_Assembly_Type);

   procedure Data_Segment
     (A    : in out Root_Assembly_Type);

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

   procedure Exception_Handler
     (A                       : in out Root_Assembly_Type'Class;
      Base_Label, Bound_Label : String;
      Handler_Label           : String);

   procedure Set_Start_Label
     (A     : in out Root_Assembly_Type'Class;
      Label : String);

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

   function Is_Named_Number
     (A    : Root_Assembly_Type'Class;
      Name : String)
      return Boolean;

   procedure Set_Deferred
     (A    : in out Root_Assembly_Type;
      Name : String);

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

   procedure Free
     (A : in out Assembly);

private

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

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
         Named_Number    : Boolean := False;
         Deferred        : Boolean := False;
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

   type Exception_Info is
      record
         Start_Label   : Ada.Strings.Unbounded.Unbounded_String;
         End_Label     : Ada.Strings.Unbounded.Unbounded_String;
         Handler_Label : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Exception_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Exception_Info);

   type Source_Position is
      record
         Start    : Address;
         Line     : Natural;
         Column   : Natural;
      end record;

   package Source_Position_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Source_Position);

   type Segment_Record is
      record
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         R, W, X     : Boolean := False;
         Initialised : Boolean := False;
         Base, Bound : Address;
      end record;

   package Segment_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Segment_Record);

   type Root_Assembly_Type is
     new Aqua.Memory.Memory_Type with
      record
         Source_Path     : Ada.Strings.Unbounded.Unbounded_String;
         Source_Locs     : Source_Position_Lists.List;
         Labels          : Label_Maps.Map;
         Segment_List    : Segment_Lists.List;
         Current_Segment : Segment_Lists.Cursor;
         Temporaries     : Temporary_Label_Vectors.Vector;
         Bindings        : Binding_Info_Vectors.Vector;
         Handlers        : Exception_Info_Vectors.Vector;
         Start_Label     : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   procedure Ensure_Label
     (A         : in out Root_Assembly_Type'Class;
      Name      : String);

   function Current (A : Root_Assembly_Type'Class) return Address
   is (Segment_Lists.Element (A.Current_Segment).Bound);

end Aqua.Assembler;
