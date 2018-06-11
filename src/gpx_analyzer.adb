with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Containers.Vectors;
with GNAT.OS_Lib;
with GNAT.Command_Line;
with DOM.Core;          use DOM.Core;
with DOM.Core.Elements;
with DOM.Core.Nodes;    use DOM.Core.Nodes;
with DOM.Core.Attrs;    use DOM.Core.Attrs;
with DOM.Core.Character_Datas;

with Elevator;
with GPX_Reader;
with Position_Types;    use Position_Types;

--  TODO: move all the XML manipulation into a separate package

procedure GPX_Analyzer is
   use Ada.Text_IO;
   use Ada.Directories;
   package Env renames Ada.Environment_Variables;

   type Distance is delta 0.01 digits 7;  -- distance in kilometers (-99_999..99_999)

   type Track_Point is new Position with record
      Dist : Distance := 0.00;
   end record;

   package Track_Point_Vector is new Ada.Containers.Vectors (Natural, Track_Point);

   function Get_Elevation (Point_Node : DOM.Core.Element) return Elevation_Type is
      Children : Node_List := DOM.Core.Elements.Get_Elements_By_Tag_Name (Point_Node, "ele");
      Child    : DOM.Core.Element;
      T_Node   : DOM.Core.Text;
      Ret      : Elevation_Type := 0.00;
   begin
      for I in 0..Length (Children) - 1 loop
         Child := Item (Children, I);
         T_Node := First_Child (Child);
         Ret := Elevation_Type'Value (DOM.Core.Character_Datas.Data (T_Node));
      end loop;
      Free (Children);
      return Ret;
   end Get_Elevation;

   function Read_Point (N : in Dom.Core.Node) return Track_Point is
      Pnt : Track_Point;
      A   : Attr;
   begin
      A := Get_Named_Item (Attributes (N), "lon");
      Pnt.Lon := Coord'Value (Value(A));
      A := Get_Named_Item (Attributes (N), "lat");
      Pnt.Lat := Coord'Value (Value(A));
      Pnt.Elevation := Get_Elevation (N);
      return Pnt;
   end Read_Point;

   package Track_Point_Reader is new GPX_Reader(Track_Point,
                                                Track_Point_Vector,
                                                Read_Point);
   package Track_Point_Elevator is new Elevator(Track_Point,
                                                Track_Point_Vector);

   Reader : Track_Point_Reader.Reader;
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
      Track_Point_Elevator.Elevate_Points (Reader.Points, API_Key);
      --  for Point of Reader.Points loop
      --     Put_Line("Point: " & Point);
      --  end loop;
   end;

   Put_Line ("Updating GPX data");
   Reader.Store_Elevation;

   Put_Line ("Writing GPX data");
   Reader.Write (Out_File_Path);

   declare
      use Position_Types;
      Total_Climb : Elevation_Type := 0.00;
      Total_Desc  : Elevation_Type := 0.00;
      Prev_Point  : Track_Point;
      Point       : Track_Point;
      Diff        : Elevation_Type;
   begin
      Prev_Point := Reader.Points(Reader.Points.First_Index);
      for I in Reader.Points.First_Index + 1..Reader.Points.Last_Index loop
         Point := Reader.Points(I);
         Diff := Point.Elevation - Prev_Point.Elevation;
         if Diff < 0.00 then
            Total_Desc := Total_Desc - Diff; --  minus negative => plus positive
         else
            Total_Climb := Total_Climb + Diff;
         end if;
         Prev_Point := Point;
      end loop;

      Put_Line ("Total climb:   " &  Total_Climb & " m");
      Put_Line ("Total descend: " &  Total_Desc & " m");
   end;

   Reader.Free;

   GNAT.OS_Lib.OS_Exit (0);
end GPX_Analyzer;
