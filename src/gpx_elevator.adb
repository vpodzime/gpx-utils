with Sax.Readers;        use Sax.Readers;
with Input_Sources.File; use Input_Sources.File;
with GPX_Reader;         use GPX_Reader;
with Ada.Text_IO;

procedure GPX_Elevator is
   package T_IO renames Ada.Text_IO;

   Reader : GPX_Reader.Reader;
   Input  : File_Input;

   function Get_Pid return Integer;
   pragma Import(C, Get_Pid, "getpid");

begin
   Set_Public_Id (Input, "TomTom GPX data");
   Set_System_Id (Input, "sample.gpx");
   Open ("sample.gpx", Input);

   Set_Feature (Reader, Validation_Feature, False);

   T_IO.Put_Line ("Parsing...");
   Parse (Reader, Input);
   Close (Input);

   for Pos of Reader.Points loop
      T_IO.Put ("Got point: ");
      T_IO.Put (Coord'Image (Pos.Lon) & ", ");
      T_IO.Put_Line (Coord'Image (Pos.Lat));
   end loop;
   T_IO.Put_Line ("Number of points: " & Natural'Image (Reader.Get_N_Points));

   --  T_IO.Put_Line ("PID: " & Integer'Image(Get_Pid));
   --  loop
   --     delay 10_000.0;
   --  end loop;

end GPX_Elevator;
