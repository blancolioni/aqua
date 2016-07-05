with Ada.Strings.Unbounded;

package body Aqua.Objects.Lists is

   ------------
   -- Append --
   ------------

   procedure Append
     (Object : in out Root_List_Type;
      Value  : Word)
   is
   begin
      Object.List.Append (Value);
   end Append;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (It       : in out Root_List_Iterator;
      Finished :    out Boolean)
   is
   begin
      if Object_Lists.Has_Element (It.Position) then
         It.Current := Object_Lists.Element (It.Position);
         Object_Lists.Next (It.Position);
         Finished := False;
      else
         Finished := True;
      end if;
   end Next;

   ---------------------
   -- Scan_Properties --
   ---------------------

   overriding procedure Scan_Properties
     (Object   : Root_List_Type;
      Process  : not null access
        procedure (Property_Name : String;
                   Property_Value : Aqua.Word))
   is
   begin
      for Item of Object.List loop
         Process ("", Item);
      end loop;
   end Scan_Properties;

   -------------------
   -- Set_Reference --
   -------------------

   overriding procedure Set_Reference
     (It        : in out Root_List_Iterator;
      Reference : External_Reference)
   is
   begin
      It.Ref := Reference;
   end Set_Reference;

   ----------
   -- Show --
   ----------

   overriding function Show
     (Object         : Root_List_Type;
      Recursive_Show : access
        function (Value : Aqua.Word) return String)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      if Object.List.Is_Empty then
         Result := To_Unbounded_String ("[]");
      else
         for Item of Object.List loop
            if Result /= Null_Unbounded_String then
               Result := Result & ", ";
            end if;
            Result := Result & Recursive_Show (Item);
         end loop;
         Result := "[" & Result;
      end if;
      return To_String (Result);
   end Show;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Object : Root_List_Type)
      return Aqua.Iterators.Aqua_Iterator_Interface'Class
   is
   begin
      return Result : Root_List_Iterator do
         Result.Position := Object.List.First;
         Result.Current  := 0;
      end return;
   end Start;

end Aqua.Objects.Lists;
