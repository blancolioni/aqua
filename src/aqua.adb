package body Aqua is

   ---------
   -- Get --
   ---------

   function Get
     (Source : Word;
      Size   : Data_Size)
      return Word
   is
   begin
      case Size is
         when Word_8_Size =>
            return Source and 16#0000_00FF#;
         when Word_16_Size =>
            return Source and 16#0000_FFFF#;
         when Word_32_Size =>
            return Source;
      end case;
   end Get;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address (Value : Word) return Address is
   begin
      return Address (Value and Payload_Mask);
   end Get_Address;

   --------------
   -- Get_Bits --
   --------------

   function Get_Bits
     (Value : Word;
      Start : Bit_Index;
      Count : Bit_Index)
      return Word
   is
      Result : Word := Value;
   begin
      Result := Result and (2 ** Natural (Start + 1) - 1);
      Result := Result / 2 ** Natural (Start + 1 - Count);
      return Result;
   end Get_Bits;

   ----------------------------
   -- Get_External_Reference --
   ----------------------------

   function Get_External_Reference
     (Value : Word)
      return External_Reference
   is
   begin
      return External_Reference (Value and Payload_Mask);
   end Get_External_Reference;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer (Value : Word) return Aqua_Integer is
   begin
      if (Value and 16#0800_0000#) = 0 then
         return Aqua_Integer (Value);
      elsif Value = 16#0800_0000# then
         return Aqua_Integer'First;
      else
         return -Aqua_Integer (16#1000_0000# - Value);
      end if;
   end Get_Integer;

   --------------------------
   -- Get_String_Reference --
   --------------------------

   function Get_String_Reference
     (Value : Word)
      return String_Reference
   is
   begin
      return String_Reference (Value and Payload_Mask);
   end Get_String_Reference;

   ---------
   -- Set --
   ---------

   procedure Set
     (Target : in out Word;
      Size   : in     Data_Size;
      Value  : in     Word)
   is
   begin
      case Size is
         when Word_8_Size =>
            Target := (Target and 16#FFFF_FF00#)
              or (Value and 16#0000_00FF#);
         when Word_16_Size =>
            Target := (Target and 16#FFFF_0000#)
              or (Value and 16#0000_FFFF#);
         when Word_32_Size =>
            Target := Value;
      end case;
   end Set;

   ---------------------
   -- To_Address_Word --
   ---------------------

   function To_Address_Word (Addr : Address) return Word is
   begin
      return Set_Tag (Word (Addr), Address_Tag);
   end To_Address_Word;

   ----------------------
   -- To_External_Word --
   ----------------------

   function To_External_Word
     (Reference : External_Reference)
      return Word
   is
   begin
      return Set_Tag (Word (Reference), External_Tag);
   end To_External_Word;

   ---------------------
   -- To_Integer_Word --
   ---------------------

   function To_Integer_Word (Value : Aqua_Integer) return Word is
   begin
      if Value >= 0 then
         return Word (Value);
      elsif Value = Aqua_Integer'First then
         return 16#0800_0000#;
      else
         return 16#1000_0000# - Word (abs Value);
      end if;
   end To_Integer_Word;

   --------------------
   -- To_String_Word --
   --------------------

   function To_String_Word
     (Reference : String_Reference)
      return Word
   is
   begin
      return Set_Tag (Word (Reference), String_Tag);
   end To_String_Word;

end Aqua;
