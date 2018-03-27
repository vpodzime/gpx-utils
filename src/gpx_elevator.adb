with Ada.Text_IO;
with Ada.IO_Exceptions;
with GNAT.OS_Lib;

with Elevator;
with GPX_Reader;

procedure GPX_Elevator is
   use Ada.Text_IO;
   Reader : GPX_Reader.Reader;
   File : File_Type;
begin
   begin
      Open (File => File,
            Mode => In_File,
            Name => ".api-key");
   exception
      when others =>
         Put_Line ("Failed to read the API key from the .api-key file");
         GNAT.OS_Lib.OS_Exit (1);
   end;

   declare
      API_Key : String := Get_Line (File);
   begin
      Reader.Read_Points ("sample.gpx");
      Elevator.Elevate_Points (Reader.Points, API_Key);
   end;
   GNAT.OS_Lib.OS_Exit (0);
end GPX_Elevator;
