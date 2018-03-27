with Ada.Text_IO;
with Util.Http.Clients;
with Util.Http.Clients.Curl;
with Ada.Strings.Unbounded;

with Position_Types;  use Position_Types;

package body Elevator is
   procedure Elevate_Points (Points : in out Position_Vector.Vector; API_Key: in String) is
      package Ustr renames Ada.Strings.Unbounded;
      use type Ustr.Unbounded_String;
      use type Position_Types.Coord;

      Http     : Util.Http.Clients.Client;
      URI      : Ustr.Unbounded_String;
      Response : Util.Http.Clients.Response;

      Batch_Size : constant := 100;
   begin
      URI := Ustr.To_Unbounded_String ("https://maps.googleapis.com/maps/api/elevation/json?locations=");
      for I in 1..Batch_Size loop
         URI := URI & Points(I).Lat & "," & Points(I).Lon;
         if I < Batch_Size then
            URI := URI & "|";
         end if;
      end loop;
      URI := URI & "&key=" & API_Key;
      --  Ada.Text_IO.Put_Line ("URI: " & Ustr.To_String(URI));
      Http.Get (Ustr.To_String(URI), Response);
      Ada.Text_IO.Put_Line ("Code: " & Natural'Image (Response.Get_Status));
      Ada.Text_IO.Put_Line (Response.Get_Body);
   end Elevate_Points;
begin
   Util.Http.Clients.Curl.Register;
end Elevator;
