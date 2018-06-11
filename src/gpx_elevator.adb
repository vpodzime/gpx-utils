with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Directories;
with Ada.Environment_Variables;
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

procedure GPX_Elevator is
   use Ada.Text_IO;
   use Ada.Directories;
   package Env renames Ada.Environment_Variables;

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

   function Read_Point (N : in Dom.Core.Node) return Position is
      Pos : Position;
      A   :   Attr;
   begin
      A := Get_Named_Item (Attributes (N), "lon");
      Pos.Lon := Coord'Value (Value(A));
      A := Get_Named_Item (Attributes (N), "lat");
      Pos.Lat := Coord'Value (Value(A));
      Pos.Elevation := Get_Elevation (N);
      return Pos;
   end Read_Point;

   package Base_Point_Reader is new GPX_Reader(Position_Types.Position,
                                               Position_Types.Position_Vector,
                                               Read_Point);

   package Base_Point_Elevator is new Elevator(Position_Types.Position,
                                               Position_Types.Position_Vector);

   Reader : Base_Point_Reader.Reader;
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
      Base_Point_Elevator.Elevate_Points (Reader.Points, API_Key);
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
