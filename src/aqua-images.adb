with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Aqua.Arithmetic;
with Aqua.IO;

package body Aqua.Images is

   Trace_Link : constant Boolean := False;
   Trace_Load : constant Boolean := False;
   Trace_Code : constant Boolean := False;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Image : Root_Image_Type'Class;
      Binder : not null access
        procedure (Group_Name  : String;
                   Before      : Boolean;
                   Parent_Name : String;
                   Child_Name  : String;
                   Start       : Address))
   is
      use Ada.Strings.Unbounded;
   begin
      for Binding of Image.Bindings loop
         Binder
           (Group_Name  => To_String (Binding.Group),
            Before      => Binding.Before,
            Parent_Name => To_String (Binding.Parent_Text),
            Child_Name  => To_String (Binding.Child_Text),
            Start       => Binding.Start);
      end loop;
   end Bind;

   ---------------
   -- Code_High --
   ---------------

   function Code_High
     (Image : Root_Image_Type'Class)
      return Address
   is
   begin
      return Image.Code_High;
   end Code_High;

   --------------
   -- Code_Low --
   --------------

   function Code_Low
     (Image : Root_Image_Type'Class)
      return Address
   is
   begin
      return Image.Code_Low;
   end Code_Low;

   -----------------
   -- Have_String --
   -----------------

   function Have_String
     (Image : Root_Image_Type'Class;
      Value : Word)
      return Boolean
   is
   begin
      return Natural (Get_String_Reference (Value))
        <= Image.String_Vector.Last_Index;
   end Have_String;

   ---------------
   -- Heap_High --
   ---------------

   function Heap_High (Image : Root_Image_Type'Class) return Address is
   begin
      return Image.High;
   end Heap_High;

   --------------
   -- Heap_Low --
   --------------

   function Heap_Low (Image : Root_Image_Type'Class) return Address is
   begin
      return Image.Low;
   end Heap_Low;

   ----------
   -- Link --
   ----------

   procedure Link
     (Image : in out Root_Image_Type'Class)
   is
      use Link_Maps;
      Have_Error : Boolean := False;
   begin
      for Position in Image.Link_Map.Iterate loop
         declare
            Info : constant Link_Info := Element (Position);
         begin
            if Trace_Link then
               Ada.Text_IO.Put
                 ("[" & Aqua.IO.Hex_Image (Info.Value) & "]"
                  & Key (Position)
                  & ":");
            end if;

            for Ref of Info.References loop
               if Trace_Link then
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (Ref.Addr));
               end if;
               if Info.Is_String then
                  declare
                     Addr : constant Address := Ref.Addr;
                     Index : constant String_Reference :=
                               String_Reference (Info.Value);
                     Value : constant Word := To_String_Word (Index);
                  begin
                     Image.Set_Word (Addr, Value);
                  end;
               elsif Ref.Branch then
                  declare
                     Target : constant Address :=
                                Aqua.Arithmetic.Relative_Address
                                  (Ref.Addr, Get_Address (Info.Value));
                  begin
                     if Ref.Addr > Get_Address (Info.Value) then
                        Ada.Text_IO.Put_Line
                          ("branch backward: "
                           & IO.Hex_Image (Ref.Addr)
                           & " "
                           & IO.Hex_Image (Info.Value)
                           & " "
                           & IO.Hex_Image (Target));
                     end if;
                     Image.Set_Value
                             (Ref.Addr, Word_16_Size, Word (Target));
                  end;
               elsif Ref.Relative then
                  declare
                     W : constant Word :=
                           To_Address_Word
                             (Aqua.Arithmetic.Relative_Address
                                (Ref.Addr, Get_Address (Info.Value)));
                  begin
                     Image.Set_Word
                       (Ref.Addr, W);
                  end;
               else
                  Image.Set_Word
                    (Ref.Addr, Info.Value);
               end if;
            end loop;

            if Trace_Link then
               Ada.Text_IO.New_Line;
            end if;

            if not Info.Has_Value
              and then not Info.References.Is_Empty
            then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "undefined reference: " & Key (Position));
               Have_Error := True;
            end if;
         end;
      end loop;
      if Have_Error then
         raise Constraint_Error with "Link error";
      end if;
   end Link;

   ----------
   -- Load --
   ----------

   procedure Load
     (Image : in out Root_Image_Type'Class;
      Name  : in     String)
   is
      use Aqua.IO;
      File : File_Type;
      Binding_Count  : Word;
      Low            : Word;
      High           : Word;
      External_Count : Word;
      String_Count   : Word;
   begin
      Ada.Text_IO.Put_Line ("image: loading " & Name);

      Open (File, Name);

      Read_Word (File, Binding_Count);
      Read_Word (File, Low);
      Read_Word (File, High);
      Read_Word (File, External_Count);
      Read_Word (File, String_Count);

      Ada.Text_IO.Put_Line
        (Name
         & ": bindings:" & Word'Image (Binding_Count)
         & "; externals:" & Word'Image (External_Count)
         & "; strings:" & Word'Image (String_Count)
         & " range "
         & Hex_Image (Low) & " - " & Hex_Image (High)
         & "; new range "
         & Hex_Image (Image.High + Get_Address (Low))
         & " - "
         & Hex_Image (Image.High + Get_Address (High)));

      for I in 1 .. Binding_Count loop
         declare
            use Ada.Strings.Unbounded;
            Header     : Word;
            Start      : Address;
            Has_Parent : Boolean := False;
            Has_Child  : Boolean := False;
            Binding    : Binding_Info;
         begin
            Read_Word (File, Header);

            Binding.Before := (Header and 1) = 1;
            Has_Parent := (Header and 2) = 2;
            Has_Child := (Header and 4) = 4;

            Binding.Group := To_Unbounded_String (Read_String_Literal (File));

            if Has_Parent then
               Binding.Parent_Text :=
                 To_Unbounded_String (Read_String_Literal (File));
            end if;

            if Has_Child then
               Binding.Child_Text :=
                 To_Unbounded_String (Read_String_Literal (File));
            end if;

            Read_Address (File, Start);
            Binding.Start := Start + Image.High;

            Image.Bindings.Append (Binding);

         end;
      end loop;

      declare
         use Ada.Strings.Unbounded;
         Source_File_Name : constant Unbounded_String :=
                              To_Unbounded_String (Read_String_Literal (File));
         Position_Count   : Word;
         Start            : Address;
         Line, Column     : Word;
      begin
         Read_Word (File, Position_Count);
         for I in 1 .. Position_Count loop
            Read_Word (File, Line);
            Read_Word (File, Column);
            Read_Address (File, Start);
            Image.Locations.Append
              ((Source_File_Name, Start + Image.High,
               Natural (Line), Natural (Column)));
         end loop;
      end;

      for Addr in 0 .. High - Low loop

         if Trace_Code then
            if Addr mod 16 = 0 then
               if Addr > 0 then
                  Ada.Text_IO.New_Line;
               end if;
               Ada.Text_IO.Put (Aqua.IO.Hex_Image (Address (Addr)
                                + Image.High));
            end if;
         end if;

         declare
            X : Octet;
         begin
            Read_Octet (File, X);

            if Trace_Code then
               Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (X));
            end if;

            Image.Set_Octet (Image.High + Address (Addr), X);
         end;
      end loop;

      if Trace_Code then
         Ada.Text_IO.New_Line;
      end if;

      for I in 1 .. External_Count + String_Count loop
         declare
            Length   : Word;
            Refs     : Word;
            Flags    : Octet;
            Defined  : Boolean;
            Deferred : Boolean;
         begin
            Read_Word (File, Length);
            Read_Word (File, Refs);
            Read_Octet (File, Flags);

            Defined := (Flags and 1) = 1;
            Deferred := (Flags and 2) = 2;

            declare
               S : String (1 .. Natural (Length));
               X : Octet;
               Info : Link_Info;
            begin
               for J in S'Range loop
                  Read_Octet (File, X);
                  S (J) := Character'Val (X);
               end loop;

               if not Deferred and then Image.Link_Map.Contains (S) then
                  Info := Image.Link_Map (S);
                  if Info.Has_Value and then Defined then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "redefined: " & S);
                  end if;
               end if;

               if Defined then
                  Info.Has_Value := True;
               end if;

               if I <= External_Count then
                  if Trace_Load then
                     Ada.Text_IO.Put
                       ((if Deferred then "D" else "E")
                        & Integer'Image (-Integer (I)) & ": "
                        & S);
                     Ada.Text_IO.Set_Col (20);
                  end if;

                  Image.Label_Vector.Append (S);

                  if Defined then
                     Read_Word (File, Info.Value);

                     if Is_Address (Info.Value) then
                        Info.Value :=
                          To_Address_Word
                            (Get_Address (Info.Value)
                             - Get_Address (Low)
                             + Image.High);
                     end if;
                  end if;
                  Info.Is_String := False;
               else
                  if False then
                     Ada.Text_IO.Put_Line
                       ("S"
                        & Integer'Image (-Integer (I - External_Count)) & ": "
                        & S);
                  end if;
                  Image.String_Vector.Append (S);
                  Info.Value := Word (Image.String_Vector.Last_Index);
                  Info.Is_String := True;
                  Info.Has_Value := True;
               end if;

               for J in 1 .. Refs loop
                  declare
                     Addr : Address;
                     Relative : Octet;
                  begin
                     Read_Address (File, Addr);
                     Read_Octet (File, Relative);
                     Info.References.Append
                       ((Addr     => Addr + Image.High,
                         Relative => Boolean'Val (Relative mod 2),
                         Branch   => Boolean'Val (Relative / 2 mod 2)));
                     if Trace_Load then
                        if I <= External_Count then
                           Ada.Text_IO.Put
                             (" " & Aqua.IO.Hex_Image (Addr + Image.High));
                           Ada.Text_IO.Put
                             ((if Relative mod 2 = 1 then "r" else ""));
                           Ada.Text_IO.Put
                             ((if Relative mod 2 = 1 then "b" else ""));
                        end if;
                     end if;
                  end;
               end loop;

               if Trace_Load then
                  if I <= External_Count then
                     Ada.Text_IO.New_Line;
                  end if;
               end if;

               if Image.Link_Map.Contains (S) then
                  Image.Link_Map (S) := Info;
               else
                  Image.Link_Map.Insert (S, Info);
               end if;

            end;
         end;

      end loop;

      Image.High := Image.High
        + (Get_Address (High) - Get_Address (Low) + 4);
      Image.Code_High := Image.High;

      Close (File);

   end Load;

   ---------------
   -- New_Image --
   ---------------

   function New_Image return Image_Type is
   begin
      return new Root_Image_Type;
   end New_Image;

   ----------
   -- Save --
   ----------

   procedure Save
     (Image : Root_Image_Type'Class;
      Path  : String)
   is
      pragma Unreferenced (Image);
      pragma Unreferenced (Path);
   begin
      null;
   end Save;

   ----------
   -- Show --
   ----------

   function Show (Image : Root_Image_Type'Class;
                  Value : Word)
                  return String
   is
   begin
      if Is_Address (Value) then
         return Aqua.IO.Hex_Image (Get_Address (Value));
      elsif Is_Integer (Value) then
         return "#" & Aqua_Integer'Image (Get_Integer (Value));
      elsif Is_External_Reference (Value) then
         return "[" & Aqua.IO.Hex_Image (Value) & "]";
      elsif Is_String_Reference (Value) then
         return '"'
           & Image.String_Vector (Natural (Get_String_Reference (Value)))
           & '"';
      else
         return Aqua.IO.Hex_Image (Value);
      end if;
   end Show;

   --------------------------
   -- Show_Source_Position --
   --------------------------

   function Show_Source_Position
     (Image : Root_Image_Type'Class;
      Addr  : Address)
      return String
   is
      use Ada.Strings, Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      File_Name : Unbounded_String := To_Unbounded_String ("unknown");
      Line      : Natural := 0;
      Column    : Natural := 0;
   begin
      for Loc of Image.Locations loop
         if Loc.Start > Addr then
            declare
               Name : constant String := To_String (File_Name);
            begin
               if Name /= "" then
                  return Ada.Directories.Simple_Name (To_String (File_Name))
                    & ":" & Trim (Natural'Image (Line), Left)
                    & ":" & Trim (Natural'Image (Column), Left);
               end if;
            end;
         else
            File_Name := Loc.Source_File;
            Line      := Loc.Line;
            Column    := Loc.Column;
         end if;
      end loop;

      return "unknown";

   end Show_Source_Position;

   ------------------
   -- String_Count --
   ------------------

   function String_Count
     (Image : Root_Image_Type'Class)
      return Natural
   is
   begin
      return Image.String_Vector.Last_Index + 1;
   end String_Count;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Image : Root_Image_Type'Class;
      Value : Word)
      return String
   is
   begin
      return Image.String_Vector (Natural (Get_String_Reference (Value)));
   end To_String;

end Aqua.Images;
