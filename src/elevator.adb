with Ada.Text_IO;
with Util.Http.Clients;
with Util.Http.Clients.Curl;
with Ada.Strings.Unbounded;
with GNATCOLL.JSON;

with Position_Types;  use Position_Types;

package body Elevator is
   Biggest_Batch_Size : constant := 100;

   procedure Update_Elevation (Points   : in out Position_Vector.Vector;
                               From     : Natural;
                               To       : Natural;
                               Data_Str : in String);

   procedure Elevate_Points (Points : in out Position_Vector.Vector; API_Key: in String) is
      package Ustr renames Ada.Strings.Unbounded;
      use type Ustr.Unbounded_String;
      use type Position_Types.Coord;

      Http     : Util.Http.Clients.Client;
      URI      : Ustr.Unbounded_String;
      Response : Util.Http.Clients.Response;

      Batch_Size : Natural;
      Offset : Natural := Points.First_Index;
      N_Points : Natural := Points.Last_Index - Points.First_Index + 1;
   begin
      while N_Points > 0 loop
         if N_Points - Biggest_Batch_Size > 0 then
            Batch_Size := Biggest_Batch_Size;
         else
            Batch_Size := N_Points;
         end if;

         URI := Ustr.To_Unbounded_String ("https://maps.googleapis.com/maps/api/elevation/json?locations=");
         for I in Offset..(Offset + Batch_Size - 1) loop
            URI := URI & Points(I).Lat & "," & Points(I).Lon;
            if I < (Offset + Batch_Size - 1) then
               URI := URI & "|";
            end if;
         end loop;
         URI := URI & "&key=" & API_Key;
         --  Ada.Text_IO.Put_Line ("URI: " & Ustr.To_String(URI));
         Http.Get (Ustr.To_String(URI), Response);
         --  Ada.Text_IO.Put_Line ("Code: " & Natural'Image (Response.Get_Status));
         --  Ada.Text_IO.Put_Line (Response.Get_Body);

         if Response.Get_Status = 200 then
            Update_Elevation (Points, Offset, Offset + Batch_Size - 1,
                              Response.Get_Body);
         end if;

         Offset := Offset + Batch_Size;
         N_Points := N_Points - Batch_Size;
      end loop;
   end Elevate_Points;

   procedure Update_Elevation (Points   : in out Position_Vector.Vector;
                               From     : Natural;
                               To       : Natural;
                               Data_Str : in String) is
      use GNATCOLL.JSON;
      Data : JSON_Value;
      Results_Data : JSON_Value;
      Results : JSON_Array;
      N_Results : Natural;
      Point_Data : JSON_Value;
      Elevation  : JSON_Value;
   begin
      Data := Read (Data_Str);
      if Data.Kind /= JSON_Object_Type then
         Ada.Text_IO.Put_Line ("Got invalid data (not an object):");
         Ada.Text_IO.Put_Line (Data_Str);
         return;
      end if;
      Results_Data := Data.Get ("results");
      if Results_Data.Kind /= JSON_Array_Type then
         Ada.Text_IO.Put_Line ("Got invalid data (not an array in the object):");
         Ada.Text_IO.Put_Line (Data_Str);
         return;
      end if;

      --  Ada.Text_IO.Put_Line ("Got valid data");
      Results := Results_Data.Get;
      N_Results := Length (Results);

      if N_Results /= (To - From + 1) then
         Ada.Text_IO.Put_Line ("Incorrect number of results!");
         return;
      end if;

      for I in 1..N_Results loop
         Point_Data := Get (Results, I);
         Elevation := Point_Data.Get ("elevation");
         --  Ada.Text_IO.Put_Line ("Elevation: " & Elevation_Type(Get_Long_Float (Elevation)));
         case Kind (Elevation) is
            when JSON_Float_Type => Points(From + I - 1).Elevation := Elevation_Type(Get_Long_Float (Elevation));
            when JSON_Int_Type =>
               declare
                  Value : Integer;
               begin
                  Value := Get (Elevation);
                  Points(From + I - 1).Elevation := Elevation_Type(Value);
               end;
            when others => Ada.Text_IO.Put_Line ("Unexpected elevation data: " & Write (Elevation));
         end case;
      end loop;
   end Update_Elevation;

begin
   Util.Http.Clients.Curl.Register;
end Elevator;
