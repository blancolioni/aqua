with Ada.Strings.Fixed;

with Aqua.IO;
with Aqua.Primitives;
with Aqua.Words;

package body Aqua.Objects.Arrays is

   ------------
   -- Append --
   ------------

   procedure Append
     (Object : in out Root_Array_Type;
      Value  : Word)
   is
   begin
      Object.Vector.Append (Value);
   end Append;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element
     (Object : Root_Array_Type;
      Index  : Aqua_Integer)
      return Word
   is
   begin
      return Object.Vector.Element (Positive (Index));
   end Get_Element;

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (Object : in out Root_Array_Type;
      Name   : in String)
      return Word
   is
      Is_Index : Boolean := True;
   begin
      if Name = "length" then
         return To_Integer_Word
           (Aqua_Integer (Object.Vector.Length));
      else
         for Ch of Name loop
            if Ch not in '0' .. '9' then
               Is_Index := False;
               exit;
            end if;
         end loop;

         if Is_Index then
            declare
               Index : constant Natural := Natural'Value (Name);
            begin
               if Index = 0 then
                  return 0;
               end if;

               if Index > Object.Vector.Last_Index then
                  return 0;
               end if;

               return Object.Vector.Element (Index);
            end;
         else
            declare
               Primitive_Name : constant String :=
                                  "array__" & Name;
               Primitive      : constant Subroutine_Reference :=
                                  Aqua.Primitives.Get_Primitive
                                    (Primitive_Name);
            begin
               if Primitive /= 0 then
                  return Aqua.Words.To_Subroutine_Word (Primitive);
               else
                  raise Constraint_Error with
                    "expected an array index but found " & Name;
               end if;
            end;
         end if;
      end if;
   end Get_Property;

   ------------------
   -- Has_Property --
   ------------------

   overriding function Has_Property
     (Object : in Root_Array_Type;
      Name   : in String)
      return Boolean
   is
   begin
      if Name = "length" then
         return True;
      else
         for Ch of Name loop
            if Ch not in '0' .. '9' then
               return False;
            end if;
         end loop;

         declare
            Index : constant Natural := Natural'Value (Name);
         begin
            return Index in 1 .. Object.Vector.Last_Index;
         end;
      end if;
   end Has_Property;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index
     (Object : Root_Array_Type)
      return Aqua_Integer
   is
   begin
      return Aqua_Integer (Object.Vector.Last_Index);
   end Last_Index;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (It       : in out Root_Array_Iterator;
      Finished :    out Boolean)
   is
   begin
      if Object_Vectors.Has_Element (It.Position) then
         It.Current := Object_Vectors.Element (It.Position);
         Object_Vectors.Next (It.Position);
         Finished := False;
      else
         Finished := True;
      end if;
   end Next;

   ---------------------
   -- Scan_Properties --
   ---------------------

   overriding procedure Scan_Properties
     (Object   : Root_Array_Type;
      Process  : not null access
        procedure (Property_Name : String;
                   Property_Value : Aqua.Word))
   is
   begin
      for I in 1 .. Object.Vector.Last_Index loop
         declare
            Name : constant String :=
                     Ada.Strings.Fixed.Trim
                       (Positive'Image (I),
                        Ada.Strings.Left);
         begin
            Process (Name, Object.Vector.Element (I));
         end;
      end loop;
   end Scan_Properties;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (Object : in out Root_Array_Type;
      Name   : in     String;
      Value  : in     Word)
   is
   begin
      for Ch of Name loop
         if Ch not in '0' .. '9' then
            raise Constraint_Error with
              "array index required; found " & Name;
         end if;
      end loop;

      declare
         Index : constant Natural := Natural'Value (Name);
      begin
         if Index = 0 then
            raise Constraint_Error with
              "array index must be greater than zero";
         end if;

         while Index > Object.Vector.Last_Index loop
            Object.Vector.Append (0);
         end loop;

         Object.Vector.Replace_Element (Index, Value);
      end;

   end Set_Property;

   -------------------
   -- Set_Reference --
   -------------------

   overriding procedure Set_Reference
     (Object : in out Root_Array_Type;
      Reference : External_Reference)
   is
   begin
      Object.Ref := Reference;
   end Set_Reference;

   -------------------
   -- Set_Reference --
   -------------------

   overriding procedure Set_Reference
     (It        : in out Root_Array_Iterator;
      Reference : External_Reference)
   is
   begin
      It.Ref := Reference;
   end Set_Reference;

   ----------
   -- Show --
   ----------

   overriding function Show
     (Object : Root_Array_Type)
      return String
   is
      function Elements_Image (Start : Positive) return String;

      --------------------
      -- Elements_Image --
      --------------------

      function Elements_Image (Start : Positive) return String is
      begin
         if Start <= Object.Vector.Last_Index then
            return (if Start = 1 then "" else ",")
              & Aqua.IO.Hex_Image (Object.Vector (Start))
              & Elements_Image (Start + 1);
         else
            return "]";
         end if;
      end Elements_Image;

   begin
      return "[" & Elements_Image (1);
   end Show;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Object : Root_Array_Type)
      return Aqua.Iterators.Aqua_Iterator_Interface'Class
   is
   begin
      return Result : Root_Array_Iterator do
         Result.Position := Object.Vector.First;
         Result.Current  := 0;
      end return;
   end Start;

end Aqua.Objects.Arrays;
