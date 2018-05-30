with Ada.Text_IO;
with Util.Http.Clients;
with Util.Http.Clients.Curl;
with Ada.Strings.Unbounded;
with GNATCOLL.JSON;
with Ada.Calendar;

with Position_Types;  use Position_Types;

package body Elevator is
   package Ustr renames Ada.Strings.Unbounded;

   Locations_Per_Request : constant := 250;
   N_Tasks : constant := 10;
   Requests_Per_Second : constant := 50;

   procedure Update_Elevation (Points   : in out Position_Vector.Vector;
                               From     : Natural;
                               To       : Natural;
                               Data_Str : in String);

   task type Elevate_Task (Points : access Position_Vector.Vector) is
      entry Elevate (From   : in Natural;
                     To     : in Natural;
                     API_Key: in Ustr.Unbounded_String);
      entry Finish;
   end Elevate_Task;

   procedure Elevate_Points_Sub (Points : in out Position_Vector.Vector;
                                 From   : in Natural;
                                 To     : in Natural;
                                 API_Key: in Ustr.Unbounded_String) is
      use type Ustr.Unbounded_String;
      use type Position_Types.Coord;
      use type Ada.Calendar.Time;

      Http     : Util.Http.Clients.Client;
      URI      : Ustr.Unbounded_String;
      Response : Util.Http.Clients.Response;

      Batch_Size : Natural;
      Offset : Natural := From;
      N_Points : Natural := To - From + 1;
      Requested : Ada.Calendar.Time;
   begin
      while N_Points > 0 loop
         if N_Points - Locations_Per_Request > 0 then
            Batch_Size := Locations_Per_Request;
         else
            Batch_Size := N_Points;
         end if;

         if (for all I in Offset..(Offset + Batch_Size - 1) =>
               Points(I).Elevation /= Unknown_Elevation) then
            --  nothing to do here if all points already have elevation
            return;
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
         Requested := Ada.Calendar.Clock;
         Http.Get (Ustr.To_String(URI), Response);
         --  Ada.Text_IO.Put_Line ("Code: " & Natural'Image (Response.Get_Status));
         --  Ada.Text_IO.Put_Line (Response.Get_Body);

         if Response.Get_Status = 200 then
            Update_Elevation (Points, Offset, Offset + Batch_Size - 1,
                              Response.Get_Body);
         end if;

         Offset := Offset + Batch_Size;
         N_Points := N_Points - Batch_Size;

         delay until Requested + Duration(1.00 / (Requests_Per_Second / N_Tasks));
      end loop;
   end Elevate_Points_Sub;

   task body Elevate_Task is
      T_From   : Natural;
      T_To     : Natural;
      T_API_Key: Ustr.Unbounded_String;
   begin
      accept Elevate (From   : in Natural;
                      To     : in Natural;
                      API_Key: in Ustr.Unbounded_String) do
         T_From := From;
         T_To := To;
         T_API_Key := API_Key;
      end Elevate;
      Elevate_Points_Sub (Points.all, T_From, T_To, T_API_Key);
      accept Finish do
         null;
      end Finish;
   end Elevate_Task;

   procedure Elevate_Points (Points : in out Position_Vector.Vector; API_Key: in String) is
      type Elevate_Task_Access is access Elevate_Task
        with Storage_Size => (2 * N_Tasks) * Elevate_Task'Max_Size_In_Storage_Elements;

      Tasks : array (0..N_Tasks - 1) of Elevate_Task_Access;
      N_Points : Natural := Points.Last_Index - Points.First_Index + 1;
      Work_Item_Size : Natural := N_Points / N_Tasks;
      From : Natural;
      To   : Natural;
      U_API_Key : Ustr.Unbounded_String := Ustr.To_Unbounded_String (API_Key);
   begin
      --  Ada.Text_IO.Put_Line ("N_Points: " & Natural'Image(N_Points));
      for I in 0..(N_Tasks - 2) loop
         From := I * Work_Item_Size;
         To   := ((I + 1) * Work_Item_Size) - 1;
         Tasks(I) := new Elevate_Task (Points'Access);
         Tasks(I).Elevate (From, To, U_API_Key);
      end loop;
      From := (N_Tasks - 1) * Work_Item_Size;
      To   := (N_Tasks * Work_Item_Size) + (N_Points mod N_Tasks) - 1;
      Tasks(N_Tasks - 1) := new Elevate_Task (Points'Access);
      Tasks(N_Tasks - 1).Elevate (From, To, U_API_Key);

      for I in 0..(N_Tasks - 1) loop
         Tasks(I).Finish;
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
         declare
            Status : String := Data.Get ("status");
         begin
            Ada.Text_IO.Put_Line ("status: " & Status);
         end;
         return;
      end if;

      for I in 1..N_Results loop
         if Points(From + I - 1).Elevation = Unknown_Elevation then
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
         else
            --  respect the original value
            --  TODO: detect and report big differences?
            null;
         end if;
      end loop;
   end Update_Elevation;

begin
   Util.Http.Clients.Curl.Register;
end Elevator;
