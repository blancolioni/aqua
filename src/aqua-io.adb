with Ada.Directories;
with Ada.Strings.Unbounded;

package body Aqua.IO is

   Local_IO_Path : Ada.Strings.Unbounded.Unbounded_String;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      Octet_IO.Close (File.F);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Name : String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Local_IO_Path = Null_Unbounded_String then
         raise Constraint_Error
           with "Aqua: scratch path not set";
      end if;
      Octet_IO.Create (File.F, Octet_IO.Out_File,
                      Ada.Directories.Compose
                        (To_String (Local_IO_Path), Name));
   end Create;

   ---------------------
   -- Current_IO_Path --
   ---------------------

   function Current_IO_Path return String is
   begin
      return Ada.Strings.Unbounded.To_String (Local_IO_Path);
   end Current_IO_Path;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Value : Word)
      return String
   is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Result     : String (1 .. 8);
      Acc        : Word := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digits (Natural (Acc mod 16) + 1);
         Acc := Acc / 16;
      end loop;
      return Result;
   end Hex_Image;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Value : Address)
      return String
   is
   begin
      return Hex_Image (Word (Value));
   end Hex_Image;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Value : Octet)
      return String
   is
      Result : constant String := Hex_Image (Word (Value));
   begin
      return Result (Result'Last - 1 .. Result'Last);
   end Hex_Image;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Value : Word;
      Size  : Data_Size)
      return String
   is
      Result : constant String := Hex_Image (Value);
   begin
      case Size is
         when Word_8_Size =>
            return Result (7 .. 8);
         when Word_16_Size =>
            return Result (5 .. 8);
         when Word_32_Size =>
            return Result;
      end case;
   end Hex_Image;

   -----------------
   -- Octal_Image --
   -----------------

   function Octal_Image
     (Value : Word)
      return String
   is
      Octal_Digits : constant String := "01234567";
      Result     : String (1 .. 12);
      Acc        : Natural := Natural (Value);
   begin
      for I in reverse Result'Range loop
         Result (I) := Octal_Digits (Acc mod 8 + 1);
         Acc := Acc / 8;
      end loop;
      return Result;
   end Octal_Image;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Name : String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Local_IO_Path = Null_Unbounded_String then
         raise Constraint_Error
           with "Aqua: scratch path not set";
      end if;
      Octet_IO.Open (File.F, Octet_IO.In_File,
                    Ada.Directories.Compose
                      (To_String (Local_IO_Path), Name));
   end Open;

   ------------------
   -- Read_Address --
   ------------------

   procedure Read_Address
     (File  : File_Type;
      Value : out Address)
   is
      W : Word;
   begin
      Read_Word (File, W);
      Value := Get_Address (W);
   end Read_Address;

   ---------------
   -- Read_Octet --
   ---------------

   procedure Read_Octet
     (File  : File_Type;
      Value : out Octet)
   is
   begin
      Octet_IO.Read (File.F, Value);
   end Read_Octet;

   -------------------------
   -- Read_String_Literal --
   -------------------------

   function Read_String_Literal
     (File : File_Type)
      return String
   is
      Length : Word;
   begin
      Read_Word (File, Length);
      return Result : String (1 .. Natural (Length)) do
         for I in Result'Range loop
            declare
               X : Octet;
            begin
               Read_Octet (File, X);
               Result (I) := Character'Val (X);
            end;
         end loop;
      end return;
   end Read_String_Literal;

   ---------------
   -- Read_Word --
   ---------------

   procedure Read_Word
     (File  : File_Type;
      Value : out Word)
   is
      X : array (1 .. 4) of Octet;
   begin
      for I in X'Range loop
         Read_Octet (File, X (I));
      end loop;
      Value := 0;
      for I in reverse X'Range loop
         Value := Value * 256 + Word (X (I));
      end loop;
   end Read_Word;

   -----------------
   -- Set_IO_Path --
   -----------------

   procedure Set_IO_Path
     (Path : String)
   is
   begin
      Local_IO_Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
   end Set_IO_Path;

   -------------------
   -- Write_Address --
   -------------------

   procedure Write_Address
     (File  : File_Type;
      Value : Address)
   is
   begin
      Write_Word (File, To_Address_Word (Value));
   end Write_Address;

   ----------------
   -- Write_Octet --
   ----------------

   procedure Write_Octet
     (File  : File_Type;
      Value : Octet)
   is
   begin
      Octet_IO.Write (File.F, Value);
   end Write_Octet;

   --------------------------
   -- Write_String_Literal --
   --------------------------

   procedure Write_String_Literal
     (File  : File_Type;
      Value : String)
   is
   begin
      Write_Word (File, Word (Value'Length));
      for Ch of Value loop
         Write_Octet (File, Character'Pos (Ch));
      end loop;
   end Write_String_Literal;

   ----------------
   -- Write_Word --
   ----------------

   procedure Write_Word
     (File  : File_Type;
      Value : Word)
   is
      It : Word := Value;
   begin
      for I in 1 .. 4 loop
         Write_Octet (File, Octet (It mod 256));
         It := It / 256;
      end loop;
   end Write_Word;

end Aqua.IO;
