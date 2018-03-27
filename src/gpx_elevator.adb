with Ada.Text_IO;
with Elevator;
with GPX_Reader;

procedure GPX_Elevator is
   package T_IO renames Ada.Text_IO;
   Reader : GPX_Reader.Reader;
begin
   Reader.Read_Points ("sample.gpx");
   Elevator.Elevate_Points (Reader.Points);
end GPX_Elevator;
