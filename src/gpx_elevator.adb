with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Directories;
with Ada.Environment_Variables;
with GNAT.OS_Lib;
with GNAT.Command_Line;

with Elevator;
with GPX_Reader;
with Position_Types;

procedure GPX_Elevator is
   use Ada.Text_IO;
   use Ada.Directories;
   package Env renames Ada.Environment_Variables;

   Reader : GPX_Reader.Reader;
   File : File_Type;
   GPX_File_Path : String := GNAT.Command_Line.Get_Argument;
   Out_File_Path : String := GNAT.Command_Line.Get_Argument;
begin
   if GPX_File_Path'Length = 0 then
      Put_Line("Path to a GPX file required");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   if Out_File_Path'Length = 0 then
      Put_Line("Output file path required");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   declare
      Local_Key_File : String := ".api-key";
      Home_Key_File : String := Env.Value("HOME") & "/.gpx_utils/api-key";
   begin
      if Exists (Local_Key_File) then
         Open (File => File,
               Mode => In_File,
               Name => Local_Key_File);
      elsif Exists (Home_Key_File) then
         Open (File => File,
               Mode => In_File,
               Name => Home_Key_File);
      else
         Put_Line ("No API key file found");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   exception
      when others =>
         Put_Line ("Failed to read the API key from the .api-key file");
         GNAT.OS_Lib.OS_Exit (1);
   end;

   declare
      API_Key : String := Get_Line (File);
      use Position_Types;
   begin
      Put_Line ("Reading GPX data");
      Reader.Read_Points (GPX_File_Path);
      Put_Line ("Getting elevation data");
      Elevator.Elevate_Points (Reader.Points, API_Key);
      --  for Point of Reader.Points loop
      --     Put_Line("Point: " & Point);
      --  end loop;
   end;

   Put_Line ("Updating GPX data");
   Reader.Store_Elevation;

   Put_Line ("Writing GPX data");
   Reader.Write (Out_File_Path);

   Reader.Free;

   GNAT.OS_Lib.OS_Exit (0);
end GPX_Elevator;
