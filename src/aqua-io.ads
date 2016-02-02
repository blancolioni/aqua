private with Ada.Sequential_IO;

package Aqua.IO is

   procedure Set_IO_Path
     (Path : String);
   --  Aqua will only read or write files contained in the directory
   --  specified by Path

   function Current_IO_Path return String;

   type File_Type is limited private;

   procedure Create (File : in out File_Type;
                     Name : String);

   procedure Open (File : in out File_Type;
                   Name : String);

   procedure Close (File : in out File_Type);

   procedure Write_Octet
     (File  : File_Type;
      Value : Octet);

   procedure Read_Octet
     (File  : File_Type;
      Value : out Octet);

   procedure Write_Word
     (File  : File_Type;
      Value : Word);

   procedure Read_Word
     (File  : File_Type;
      Value : out Word);

   procedure Write_Address
     (File  : File_Type;
      Value : Address);

   procedure Read_Address
     (File  : File_Type;
      Value : out Address);

   procedure Write_String_Literal
     (File  : File_Type;
      Value : String);

   function Read_String_Literal
     (File : File_Type)
      return String;

   function Hex_Image
     (Value : Word)
      return String;

   function Hex_Image
     (Value : Word;
      Size  : Data_Size)
      return String;

   function Octal_Image
     (Value : Word)
      return String;

   function Hex_Image
     (Value : Address)
      return String;

   function Hex_Image
     (Value : Octet)
      return String;

private

   package Octet_IO is
     new Ada.Sequential_IO (Octet);

   type File_Type is limited
      record
         F : Octet_IO.File_Type;
      end record;

end Aqua.IO;
