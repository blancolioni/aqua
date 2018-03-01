with Ada.Strings.Unbounded;

with Aqua.Primitives;
with Aqua.Words;

package body Aqua.Objects is

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (Object : in out Root_Object_Type;
      Name   : in String)
      return Aqua.Values.Property_Value
   is
      Result : Aqua.Values.Property_Value;
   begin
      if Object.Map.Contains (Name) then
         Result := Object.Map.Element (Name);
      else
         declare
            Object_Primitive_Name : constant String :=
                                      "object__" & Name;
            Object_Primitive      : constant Primitive_Reference :=
                                      Aqua.Primitives.Get_Primitive
                                        (Object_Primitive_Name);
         begin
            if Object_Primitive /= 0 then
               Result :=
                 Aqua.Values.To_Word_Value
                   (Aqua.Words.To_Primitive_Word (Object_Primitive));
            else
               Result :=
                 Aqua.Values.To_Word_Value (0);
            end if;
         end;
      end if;
      return Result;
   end Get_Property;

   -------------------
   -- Get_Reference --
   -------------------

   overriding function Get_Reference
     (Object : Root_Object_Type)
      return External_Reference
   is
   begin
      return Object.Ref;
   end Get_Reference;

   -------------------
   -- Get_Reference --
   -------------------

   overriding function Get_Reference
     (It : Root_Object_Iterator)
      return External_Reference
   is
   begin
      return It.Ref;
   end Get_Reference;

   ------------------
   -- Has_Property --
   ------------------

   overriding function Has_Property
     (Object : in Root_Object_Type;
      Name   : in String)
      return Boolean
   is
   begin
      return Object.Map.Contains (Name)
        or else Aqua.Primitives.Get_Primitive
          (Name => "object__" & Name) /= 0;
   end Has_Property;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (It       : in out Root_Object_Iterator;
      Finished :    out Boolean)
   is
   begin
      if Object_Maps.Has_Element (It.Position) then
         It.Current := Object_Maps.Element (It.Position);
         Object_Maps.Next (It.Position);
         Finished := False;
      else
         Finished := True;
      end if;
   end Next;

   ---------------------
   -- Scan_Properties --
   ---------------------

   overriding procedure Scan_Properties
     (Object  : Root_Object_Type;
      Process : not null access
        procedure (Name  : String;
                   Value : Aqua.Values.Property_Value))
   is
   begin
      for Position in Object.Map.Iterate loop
         Process (Object_Maps.Key (Position),
                  Object_Maps.Element (Position));
      end loop;
   end Scan_Properties;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (Object : in out Root_Object_Type;
      Name   : in     String;
      Value  : in     Aqua.Values.Property_Value)
   is
   begin
      if Object.Map.Contains (Name) then
         Object.Map.Replace (Name, Value);
      else
         Object.Map.Insert (Name, Value);
      end if;
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : in out Object_Interface'Class;
      Name   : in     String;
      Value  : in     Word)
   is
   begin
      Object.Set_Property (Name, Aqua.Values.To_Word_Value (Value));
   end Set_Property;

   -------------------
   -- Set_Reference --
   -------------------

   overriding procedure Set_Reference
     (Object : in out Root_Object_Type;
      Reference : External_Reference)
   is
   begin
      Object.Ref := Reference;
   end Set_Reference;

   -------------------
   -- Set_Reference --
   -------------------

   overriding procedure Set_Reference
     (It : in out Root_Object_Iterator;
      Reference : External_Reference)
   is
   begin
      It.Ref := Reference;
   end Set_Reference;

   ----------
   -- Show --
   ----------

   overriding function Show
     (Object         : Root_Object_Type;
      Recursive_Show : access
        function (Value : Word) return String)
      return String
   is
      use Ada.Strings.Unbounded;
      use Object_Maps;
      Result : Unbounded_String;
   begin
      for Position in Object.Map.Iterate loop
         if Result = Null_Unbounded_String then
            Result := To_Unbounded_String ("(");
         else
            Result := Result & ", ";
         end if;
         Result := Result
           & Key (Position) & " => "
           & Recursive_Show (Aqua.Values.To_Word (Element (Position)));
      end loop;
      if Result = Null_Unbounded_String then
         return "()";
      else
         return To_String (Result) & ")";
      end if;
   end Show;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Object : Root_Object_Type)
      return Aqua.Iterators.Aqua_Iterator_Interface'Class
   is
   begin
      return Result : Root_Object_Iterator do
         Result.Position := Object.Map.First;
         Result.Current  := Aqua.Values.To_Word_Value (0);
      end return;
   end Start;

end Aqua.Objects;
