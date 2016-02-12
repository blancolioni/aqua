with Ada.Text_IO;

with Aqua.IO;

package body Aqua.Assembler is

   function Temporary_Label_Image
     (Label : Natural;
      Index : Positive)
      return String;

   function Branch_Offset
     (PC          : Address;
      Destination : Address)
      return Word;

   ------------
   -- Append --
   ------------

   procedure Append
     (A    : in out Root_Assembly_Type'Class;
      W    : Word;
      Size : Aqua.Data_Size)
   is
   begin
      A.High :=
        Address'Max (A.High, A.PC + Address (Data_Octets (Size)));
      A.Low  := Address'Min (A.Low, A.PC);
      A.Set_Value (A.PC, Size, W);
      A.PC := A.PC + Address (Data_Octets (Size));
   end Append;

   ------------------
   -- Append_Octet --
   ------------------

   procedure Append_Octet
     (A : in out Root_Assembly_Type'Class;
      X : Octet)
   is
   begin
      A.Append (Word (X), Word_8_Size);
   end Append_Octet;

   -----------------
   -- Append_Word --
   -----------------

   procedure Append_Word
     (A : in out Root_Assembly_Type'Class;
      W : Word)
   is
   begin
      A.Append (W, Word_32_Size);
   end Append_Word;

   -----------------
   -- Bind_Action --
   -----------------

   procedure Bind_Action
     (A      : in out Root_Assembly_Type'Class;
      Group  : String;
      Before : Boolean;
      Parent : String;
      Child  : String)
   is
      use Ada.Strings.Unbounded;
      Info : Binding_Info;
      Group_String : constant String :=
                       '"' & Group & '"';
      Parent_String : constant String :=
                        '"' & Parent & '"';
      Child_String  : constant String :=
                        '"' & Child & '"';
   begin
      Ensure_Label (A, Group_String, True);
      Ensure_Label (A, Parent_String, True);
      if Child /= "" then
         Ensure_Label (A, Child_String, True);
      end if;
      Info := (To_Unbounded_String (Group),
               A.PC,
               Before,
               To_Unbounded_String (Parent),
               To_Unbounded_String (Child));
      A.Bindings.Append (Info);
   end Bind_Action;

   -------------------
   -- Branch_Offset --
   -------------------

   function Branch_Offset
     (PC          : Address;
      Destination : Address)
      return Word
   is
      Offset : Address;
   begin
      if Destination >= PC + 2 then
         Offset := Destination - PC - 2;
      else
         Offset := 16#1_0000# - (PC + 2 - Destination);
      end if;
      return Word (Offset mod 65536);
   end Branch_Offset;

   ---------------------------
   -- Define_Exported_Label --
   ---------------------------

   procedure Define_Exported_Label
     (A    : in out Root_Assembly_Type'Class;
      Name : String)
   is
   begin
      A.Ensure_Label (Name, False);
      declare
         Info : Label_Info := A.Labels (Name);
      begin
         Info.External := True;
         Info.Defined := False;
         A.Labels (Name) := Info;
      end;
   end Define_Exported_Label;

   ---------------------------
   -- Define_External_Label --
   ---------------------------

   procedure Define_External_Label
     (A    : in out Root_Assembly_Type'Class;
      Name : String)
   is
   begin
      A.Ensure_Label (Name, False);
      declare
         Info : Label_Info := A.Labels (Name);
      begin
         Info.External := True;
         Info.Defined := False;
         A.Labels (Name) := Info;
      end;
   end Define_External_Label;

   ------------------
   -- Define_Label --
   ------------------

   procedure Define_Label
     (A    : in out Root_Assembly_Type'Class;
      Name : String)
   is
   begin
      A.Define_Value (Name, To_Address_Word (A.PC));
   end Define_Label;

   -----------------
   -- Define_Name --
   -----------------

   procedure Define_Name
     (A     : in out Root_Assembly_Type'Class;
      Name  : String;
      Value : String)
   is
   begin
      if A.Labels.Contains (Value) then
         if not A.Labels (Value).Defined then
            raise Constraint_Error
              with "undefined: " & Value;
         end if;
      end if;
      if A.Labels.Contains (Name) then
         if A.Labels (Name).Defined then
            raise Constraint_Error
              with "duplicate definition: " & Name;
         else
            raise Constraint_Error with
              "definition of '" & Name & "' is too late";
         end if;
      end if;

      A.Labels.Insert (Name, A.Labels (Value));

   end Define_Name;

   ----------------------------
   -- Define_Temporary_Label --
   ----------------------------

   procedure Define_Temporary_Label
     (A     : in out Root_Assembly_Type'Class;
      Label : Natural)
   is
   begin
      while A.Temporaries.Last_Index < Label loop
         A.Temporaries.Append (0);
      end loop;

      A.Temporaries (Label) := A.Temporaries (Label) + 1;
      A.Define_Label (Temporary_Label_Image (Label, A.Temporaries (Label)));
   end Define_Temporary_Label;

   ------------------
   -- Define_Value --
   ------------------

   procedure Define_Value
     (A     : in out Root_Assembly_Type'Class;
      Name  : String;
      Value : Word)
   is
   begin
      A.Ensure_Label (Name, False);
      declare
         Info : Label_Info := A.Labels (Name);
      begin
         if Info.Defined then
            raise Constraint_Error
              with "redefinition of label '" & Name & "'";
         end if;

         Info.Defined := True;
         Info.Named_Number := True;
         Info.Value   := Value;

         for Ref of Info.References loop
            declare
               Addr     : constant Address := Ref.Addr;
               Dest     : constant Address := Get_Address (Value);
               Relative : constant Boolean := Ref.Relative;
               Branch   : constant Boolean := Ref.Branch;
            begin
               if Branch then
                  declare
                     Offset : constant Word :=
                                Branch_Offset (Addr, Dest);
                  begin
                     A.Set_Value (Addr, Word_16_Size, Offset);
                  end;
               elsif Relative then
                  pragma Assert (Is_Address (Info.Value));
                  A.Set_Word (Addr,
                              To_Address_Word
                                (Get_Address (Info.Value) - Addr));
               else
                  A.Set_Word (Addr, Info.Value);
               end if;
            end;
         end loop;
         A.Labels (Name) := Info;
      end;
   end Define_Value;

   ------------------
   -- Ensure_Label --
   ------------------

   procedure Ensure_Label
     (A         : in out Root_Assembly_Type'Class;
      Name      : String;
      Is_String : Boolean)
   is
   begin
      if not A.Labels.Contains (Name) then
         declare
            Info : constant Label_Info :=
                     (References      => Label_Reference_Lists.Empty_List,
                      Defined         => False,
                      External        => False,
                      Deferred        => False,
                      Register_Alias  => False,
                      String_Constant => Is_String,
                      Named_Number    => False,
                      Value           =>
                        (if Is_String
                         then A.Next_String
                         else 0));
         begin
            A.Labels.Insert (Name, Info);
            if Is_String then
               A.String_Lits.Append (Name);
               A.Next_String := A.Next_String + 1;
            end if;
         end;
      end if;
   end Ensure_Label;

   ------------------
   -- Get_Register --
   ------------------

   function Get_Register
     (A : Root_Assembly_Type'Class;
      Name : String)
      return Aqua.Architecture.Register_Index
   is
   begin
      return Aqua.Architecture.Register_Index (A.Labels (Name).Value);
   end Get_Register;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (A    : Root_Assembly_Type'Class;
      Name : String)
      return Word
   is
   begin
      if A.Labels.Contains (Name) then
         return A.Labels (Name).Value;
      else
         raise Constraint_Error
           with "undefined: " & Name;
      end if;
   end Get_Value;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined
     (A    : Root_Assembly_Type'Class;
      Name : String)
      return Boolean
   is
   begin
      return A.Labels.Contains (Name);
   end Is_Defined;

   ---------------------
   -- Is_Named_Number --
   ---------------------

   function Is_Named_Number
     (A    : Root_Assembly_Type'Class;
      Name : String)
      return Boolean
   is
   begin
      return A.Labels.Contains (Name)
        and then A.Labels (Name).Named_Number;
   end Is_Named_Number;

   -----------------
   -- Is_Register --
   -----------------

   function Is_Register
     (A : Root_Assembly_Type'Class;
      Name : String)
      return Boolean
   is
   begin
      return A.Labels.Contains (Name)
        and then A.Labels (Name).Register_Alias;
   end Is_Register;

   ----------------------------
   -- Reference_Branch_Label --
   ----------------------------

   function Reference_Branch_Label
     (A        : in out Root_Assembly_Type'Class;
      Name     : String)
      return Word
   is
   begin
      A.Ensure_Label (Name, False);

      declare
         Info : Label_Info := A.Labels (Name);
      begin
         Info.References.Append ((A.PC, Relative => False, Branch => True));
         A.Labels (Name) := Info;
         if Info.Defined then
            return Branch_Offset (A.PC, Get_Address (Info.Value));
         else
            return 0;
         end if;
      end;
   end Reference_Branch_Label;

   ---------------------
   -- Reference_Label --
   ---------------------

   function Reference_Label
     (A        : in out Root_Assembly_Type'Class;
      Name     : String;
      Relative : Boolean)
      return Word
   is
   begin
      A.Ensure_Label (Name, False);

      declare
         Info : Label_Info := A.Labels (Name);
      begin
         Info.References.Append ((A.PC, Relative, False));
         A.Labels (Name) := Info;

         if Info.Defined then
            if Relative then
               pragma Assert (Is_Address (Info.Value));
               declare
                  Target : constant Address := Get_Address (Info.Value);
                  Addr   : constant Address := A.PC;
                  Offset : constant Address := Target - Addr;
                  Index  : constant Word := To_Address_Word (Offset);
               begin
                  return Index;
               end;
            else
               return Info.Value;
            end if;
         else
            return 0;
         end if;
      end;
   end Reference_Label;

   -----------------------------
   -- Reference_Property_Name --
   -----------------------------

   function Reference_Property_Name
     (A    : in out Root_Assembly_Type'Class;
      Name : String)
      return Word
   is
   begin
      A.Ensure_Label (Name,  True);
      A.Labels (Name).References.Append ((A.PC, False, False));
      return A.Next_String - 1;
   end Reference_Property_Name;

   ----------------------
   -- Reference_String --
   ----------------------

   function Reference_String
     (A : in out Root_Assembly_Type'Class;
      X : String)
      return Word
   is
   begin
      A.Ensure_Label (X,  True);
      A.Labels (X).References.Append ((A.PC, False, False));
      return A.Labels (X).Value;
   end Reference_String;

   --------------------------------------
   -- Reference_Temporary_Branch_Label --
   --------------------------------------

   function Reference_Temporary_Branch_Label
     (A       : in out Root_Assembly_Type'Class;
      Label   : Natural;
      Forward : Boolean)
      return Word
   is
   begin
      while A.Temporaries.Last_Index < Label loop
         A.Temporaries.Append (0);
      end loop;
      if Forward then
         return Reference_Branch_Label
           (A, Temporary_Label_Image (Label, A.Temporaries (Label) + 1));
      else
         return Reference_Branch_Label
           (A, Temporary_Label_Image (Label, A.Temporaries (Label)));
      end if;
   end Reference_Temporary_Branch_Label;

   -------------------------------
   -- Reference_Temporary_Label --
   -------------------------------

   function Reference_Temporary_Label
     (A       : in out Root_Assembly_Type'Class;
      Label   : Natural;
      Forward : Boolean)
      return Word
   is
   begin
      while A.Temporaries.Last_Index < Label loop
         A.Temporaries.Append (0);
      end loop;
      if Forward then
         return Reference_Label
           (A, Temporary_Label_Image (Label, A.Temporaries (Label) + 1),
            True);
      else
         return Reference_Label
           (A, Temporary_Label_Image (Label, A.Temporaries (Label)),
              True);
      end if;
   end Reference_Temporary_Label;

   ------------------
   -- Set_Deferred --
   ------------------

   procedure Set_Deferred
     (A    : in out Root_Assembly_Type;
      Name : String)
   is
   begin
      A.Ensure_Label (Name, Is_String => False);
      A.Labels (Name).Deferred := True;
   end Set_Deferred;

   --------------
   -- Set_Octet --
   --------------

   overriding procedure Set_Octet
     (Assembly : in out Root_Assembly_Type;
      Addr   : Address;
      Value  : Octet)
   is
   begin
      Assembly.Memory.Set_Octet (Addr, Value);
   end Set_Octet;

   ---------------------
   -- Set_Source_File --
   ---------------------

   procedure Set_Source_File
     (A    : in out Root_Assembly_Type;
      Path : String)
   is
   begin
      A.Source_Path :=
        Ada.Strings.Unbounded.To_Unbounded_String (Path);
   end Set_Source_File;

   -------------------------
   -- Set_Source_Location --
   -------------------------

   procedure Set_Source_Location
     (A      : in out Root_Assembly_Type;
      Line   : Natural;
      Column : Natural)
   is
      New_Position : constant Source_Position :=
                       (A.PC, Line, Column);
   begin
      A.Source_Locs.Append (New_Position);
   end Set_Source_Location;

   -----------
   -- Start --
   -----------

   procedure Start (A          : in out Root_Assembly_Type) is
      use Aqua.Architecture;
   begin
      for R in Register_Index loop
         declare
            X  : constant String := Register_Index'Image (R);
            Rx : constant String :=
                   "R" & X (2 .. X'Last);
            Info : constant Label_Info :=
                     (References      => Label_Reference_Lists.Empty_List,
                      Defined         => False,
                      External        => False,
                      Register_Alias  => True,
                      String_Constant => False,
                      Named_Number    => False,
                      Deferred        => False,
                      Value           => Word (R));
         begin
            A.Labels.Insert (Rx, Info);
            if R = 9 then
               A.Labels.Insert ("AGG", Info);
            elsif R = 10 then
               A.Labels.Insert ("CTR", Info);
            elsif R = 11 then
               A.Labels.Insert ("PV", Info);
            elsif R = 12 then
               A.Labels.Insert ("OP", Info);
            elsif R = 13 then
               A.Labels.Insert ("FP", Info);
            elsif R = 14 then
               A.Labels.Insert ("SP", Info);
            elsif R = 15 then
               A.Labels.Insert ("PC", Info);
            end if;
         end;
      end loop;
   end Start;

   ---------------------------
   -- Temporary_Label_Image --
   ---------------------------

   function Temporary_Label_Image
     (Label : Natural;
      Index : Positive)
      return String
   is
      Pre : String := Natural'Image (Label);
      Post : String := Positive'Image (Index);
   begin
      Pre (Pre'First) := 'T';
      Post (Post'First) := '.';
      return Pre & Post;
   end Temporary_Label_Image;

   -----------------
   -- Write_Image --
   -----------------

   procedure Write_Image
     (A : Root_Assembly_Type'Class;
      Path : String)
   is
      use Aqua.IO;
      use Label_Maps;
      File : File_Type;
      String_Count   : Word := 0;
      External_Count : Word := 0;
   begin

      for Position in A.Labels.Iterate loop
         if Element (Position).String_Constant then
            String_Count := String_Count + 1;
         elsif Element (Position).External
           or else Element (Position).Deferred
         then
            External_Count := External_Count + 1;
         end if;
      end loop;

      Create (File, Path);
      Write_Word (File, Word (A.Bindings.Length));
      Write_Address (File, A.Low);
      Write_Address (File, A.High);
      Write_Word (File, External_Count);
      Write_Word (File, String_Count);

      for Binding of A.Bindings loop
         declare
            use Ada.Strings.Unbounded;
            Header : Word := 0;
            Have_Parent : constant Boolean :=
                            Binding.Parent_Text /= Null_Unbounded_String;
            Have_Child  : constant Boolean :=
                            Binding.Child_Text /= Null_Unbounded_String;
         begin

            if Binding.Before then
               Header := Header + 1;
            end if;
            if Have_Parent then
               Header := Header + 2;
            end if;
            if Have_Child then
               Header := Header + 4;
            end if;

            Write_Word (File, Header);

            Write_String_Literal (File, To_String (Binding.Group_Name));
            if Have_Parent then
               Write_String_Literal (File, To_String (Binding.Parent_Text));
            end if;
            if Have_Child then
               Write_String_Literal (File, To_String (Binding.Child_Text));
            end if;

            Write_Address (File, Binding.Start);
         end;
      end loop;

      declare
         S : constant String :=
               Ada.Strings.Unbounded.To_String (A.Source_Path);
      begin
         Write_String_Literal (File, S (2 .. S'Last - 1));
      end;

      Write_Word (File, Word (A.Source_Locs.Length));
      for Loc of A.Source_Locs loop
         Write_Word (File, Word (Loc.Line));
         Write_Word (File, Word (Loc.Column));
         Write_Address (File, Loc.Start);
      end loop;

      for Addr in A.Low .. A.High loop
         Write_Octet (File, A.Memory.Get_Octet (Addr));
      end loop;

      for Position in A.Labels.Iterate loop
         declare
            Label : constant String := Key (Position);
            Info  : constant Label_Info := Element (Position);
         begin
            if Info.External or else Info.Deferred then
               Write_Word (File, Word (Label'Length));
               Write_Word (File, Word (Info.References.Length));
               Write_Octet
                 (File,
                  Boolean'Pos (Info.Defined)
                  + 2 * Boolean'Pos (Info.Deferred));
               for Ch of Label loop
                  Write_Octet (File, Character'Pos (Ch));
               end loop;
               if Info.Defined then
                  Write_Word (File, Info.Value);
               end if;
               for Ref of Info.References loop
                  Write_Address (File, Ref.Addr);
                  Write_Octet
                    (File,
                     Boolean'Pos (Ref.Relative)
                         + 2 * Boolean'Pos (Ref.Branch));
               end loop;
            end if;
         end;
      end loop;

      for S of A.String_Lits loop
         declare
            Info        : constant Label_Info :=
                            A.Labels (S);
            Raw_Text    : constant String := S;
            String_Text : String (1 .. Raw_Text'Length);
            Count       : Natural := 0;
            Index       : Positive := Raw_Text'First + 1;
         begin

            if Raw_Text'Length >= 2
              and then Raw_Text (Raw_Text'First) = '"'
              and then Raw_Text (Raw_Text'Last) = '"'
            then
               --  Remove surrounding quotes, and convert two double quotes
               --  into one.
               while Index < Raw_Text'Last loop
                  if Index < Raw_Text'Last - 1
                    and then Raw_Text (Index) = '"'
                    and then Raw_Text (Index + 1) = '"'
                  then
                     Index := Index + 1;
                  end if;
                  Count := Count + 1;
                  String_Text (Count) := Raw_Text (Index);
                  Index := Index + 1;
               end loop;
            else
               String_Text := Raw_Text;
               Count := Raw_Text'Length;
            end if;

            Write_Word (File, Word (Count));
            Write_Word (File, Word (Info.References.Length));
            Write_Octet (File, 0);
            for I in 1 .. Count loop
               Write_Octet (File, Character'Pos (String_Text (I)));
            end loop;

            for Ref of Info.References loop
               Write_Address (File, Ref.Addr);
               Write_Octet (File, Boolean'Pos (Ref.Relative));
            end loop;
         end;
      end loop;

      Close (File);

   end Write_Image;

   -------------------
   -- Write_Listing --
   -------------------

   procedure Write_Listing (A : Root_Assembly_Type'Class) is
      use Ada.Text_IO;
      use Label_Maps;
   begin
      Put_Line ("Labels:");
      for Position in A.Labels.Iterate loop
         if Element (Position).Defined
           and then not Element (Position).String_Constant
         then
            Put ("    " & Key (Position));
            Set_Col (30);
            Put_Line (Aqua.IO.Hex_Image (Element (Position).Value));
         end if;
      end loop;

      New_Line;

      Put_Line ("Strings:");
      for Position in A.Labels.Iterate loop
         if Element (Position).String_Constant then
            Put_Line ("    " & Key (Position));
         end if;
      end loop;

      Put ("External:");
      for Position in A.Labels.Iterate loop
         if not Element (Position).Defined
           and then not Element (Position).Register_Alias
           and then not Element (Position).String_Constant
         then
            Put (" " & Key (Position));
         end if;
      end loop;
      New_Line;
   end Write_Listing;

end Aqua.Assembler;
