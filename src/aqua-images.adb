with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

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

   -------------------------
   -- Get_Handler_Address --
   -------------------------

   function Get_Handler_Address
     (Image        : Root_Image_Type'Class;
      Trap_Address : Address)
      return Address
   is
   begin
      for Info of Image.Handlers loop
         if Trap_Address in Info.Base_Address .. Info.Bound_Address then
            return Info.Handler_Address;
         end if;
      end loop;
      return 0;
   end Get_Handler_Address;

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
               if Ref.Branch then
                  declare
                     Target : constant Address :=
                                Ref.Addr - Info.Value;
                  begin
                     if Ref.Addr > Info.Value then
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
                     W : constant Word := Info.Value - Ref.Addr;
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
                  (-Info.Defn_File)
                  & ": undefined reference: "
                  & Key (Position));
               Have_Error := True;
            end if;

            if Info.Start then
               Image.Start := Info.Value;
            end if;

         end;
      end loop;

      for Handler of Image.Handlers loop
         declare
            procedure Check
              (Label : Ada.Strings.Unbounded.Unbounded_String;
               Addr  : out Address);

            -----------
            -- Check --
            -----------

            procedure Check
              (Label : Ada.Strings.Unbounded.Unbounded_String;
               Addr  : out Address)
            is
               Key : constant String := -Label;
            begin
               if not Image.Link_Map.Contains (Key)
                 or else not Image.Link_Map.Element (Key).Has_Value
               then
                  Ada.Text_IO.Put_Line
                    ("undefined reference to " & Key);
                  Have_Error := True;
               else
                  declare
                     Info : constant Link_Info :=
                              Image.Link_Map.Element (Key);
                  begin
                     Addr := Info.Value;
                  end;
               end if;
            end Check;

         begin
            Check (Handler.Base_Label, Handler.Base_Address);
            Check (Handler.Bound_Label, Handler.Bound_Address);
            Check (Handler.Handler_Label, Handler.Handler_Address);

            if Trace_Link and then not Have_Error then
               Ada.Text_IO.Put_Line
                 ((-Handler.Handler_Label)
                  & ": "
                  & Aqua.IO.Hex_Image (Handler.Base_Address)
                  & " .. "
                  & Aqua.IO.Hex_Image (Handler.Bound_Address));
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
      Handler_Count  : Word;
      Low            : Word;
      High           : Word;
      External_Count : Word;
   begin

      if Trace_Load then
         Ada.Text_IO.Put_Line ("image: loading " & Name);
      end if;

      Open (File, Name);

      Read_Word (File, Binding_Count);
      Read_Word (File, Handler_Count);
      Read_Word (File, Low);
      Read_Word (File, High);
      Read_Word (File, External_Count);

      if Trace_Load then
         Ada.Text_IO.Put_Line
           (Name
            & ": bindings:" & Word'Image (Binding_Count)
            & "; handlers:" & Word'Image (Handler_Count)
            & "; externals:" & Word'Image (External_Count)
            & " range "
            & Hex_Image (Low) & " - " & Hex_Image (High)
            & "; new range "
            & Hex_Image (Image.High + Low)
            & " - "
            & Hex_Image (Image.High + High));
      end if;

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

      for I in 1 .. External_Count loop
         declare
            Length   : Word;
            Refs     : Word;
            Flags    : Octet;
            Defined  : Boolean;
            Deferred : Boolean;
            Start    : Boolean;
            Exists   : Boolean := False;
         begin
            Read_Word (File, Length);
            Read_Word (File, Refs);
            Read_Octet (File, Flags);

            Defined := (Flags and 1) = 1;
            Deferred := (Flags and 2) = 2;
            Start := (Flags and 4) = 4;

            declare
               S : String (1 .. Natural (Length));
               X : Octet;
               Info : Link_Info;
            begin
               for J in S'Range loop
                  Read_Octet (File, X);
                  S (J) := Character'Val (X);
               end loop;

               if Image.Link_Map.Contains (S) then
                  Exists := True;
                  Info := Image.Link_Map (S);

                  if Info.Has_Value and then Defined then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "multiple definitions for " & S
                        & " in " & (-Info.Defn_File)
                        & " and " & Ada.Directories.Base_Name (Name));
                  end if;

                  if Trace_Load and then Deferred then
                     Ada.Text_IO.Put_Line
                       (S & ": map contains deferred entry");
                  end if;
               end if;

               Info.Start := Start;
               Info.Defn_File := +(Ada.Directories.Base_Name (Name));

               if Defined then
                  Info.Has_Value := True;
               end if;

               if Trace_Load then
                  if Exists then
                     Ada.Text_IO.Put ("e");
                  else
                     Ada.Text_IO.Put ("-");
                  end if;
                  if Info.Has_Value then
                     Ada.Text_IO.Put ("v");
                  else
                     Ada.Text_IO.Put ("-");
                  end if;
                  if Deferred then
                     Ada.Text_IO.Put ("d");
                  else
                     Ada.Text_IO.Put ("-");
                  end if;

                  Ada.Text_IO.Put ("-");

                  Ada.Text_IO.Put
                    (Integer'Image (Integer (I)) & ": "
                     & S);
                  Ada.Text_IO.Set_Col (60);
               end if;

               Image.Label_Vector.Append (S);

               if Defined then
                  Read_Word (File, Info.Value);
                  Info.Value := Info.Value - Low + Image.High;
               end if;

               if Info.Has_Value and then Trace_Load then
                  Ada.Text_IO.Put (Aqua.IO.Hex_Image (Info.Value));
               end if;

               if Trace_Load then
                  Ada.Text_IO.Set_Col (72);
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
                             ((if Relative / 2 mod 2 = 1 then "b" else ""));
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
                  if Trace_Load then
                     Ada.Text_IO.Put ("updating: ");
                  end if;
                  Image.Link_Map (S) := Info;
               else
                  if Trace_Load then
                     Ada.Text_IO.Put ("inserting: ");
                  end if;
                  Image.Link_Map.Insert (S, Info);
               end if;

               if Trace_Load then
                  Ada.Text_IO.Put_Line (S);
               end if;

            end;
         end;

      end loop;

      Image.High := Image.High
        + (High - Low + 4);
      Image.Code_High := Image.High;

      for I in 1 .. Handler_Count loop
         declare
            Base    : constant String :=
                        Aqua.IO.Read_String_Literal (File);
            Bound   : constant String :=
                        Aqua.IO.Read_String_Literal (File);
            Handler : constant String :=
                        Aqua.IO.Read_String_Literal (File);
         begin
            Image.Handlers.Append
              (Exception_Info'
                 (Base_Label    => +Base,
                  Bound_Label   => +Bound,
                  Handler_Label => +Handler,
                  others        => 0));
         end;
      end loop;

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
   is (Aqua.IO.Hex_Image (Value));

   --------------------------------
   -- Show_Known_Source_Position --
   --------------------------------

   function Show_Known_Source_Position
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
                  if Line > 1 or else Column > 1 then
                     return Ada.Directories.Simple_Name (To_String (File_Name))
                       & ":" & Trim (Natural'Image (Line), Left)
                       & ":" & Trim (Natural'Image (Column), Left);
                  else
                     return Ada.Directories.Simple_Name
                       (To_String (File_Name));
                  end if;
               end if;
            end;
         else
            File_Name := Loc.Source_File;
            Line      := Loc.Line;
            Column    := Loc.Column;
         end if;
      end loop;

      return "";

   end Show_Known_Source_Position;

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
                  return Ada.Directories.Base_Name (To_String (File_Name))
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

end Aqua.Images;
