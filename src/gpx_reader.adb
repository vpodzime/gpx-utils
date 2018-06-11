with Sax.Readers;        use Sax.Readers;
with DOM.Readers;        use DOM.Readers;
with DOM.Core;           use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with DOM.Core.Attrs;     use DOM.Core.Attrs;
with DOM.Core.Elements;
with Input_Sources.File; use Input_Sources.File;
with Unicode.CES;
with Ada.Streams.Stream_IO;
with Ada.Containers;

with Position_Types;     use Position_Types;

package body GPX_Reader is
   --  overriding procedure Start_Element
   --    (Handler       : in out Reader;
   --     Namespace_URI : Unicode.CES.Byte_Sequence := "";
   --     Local_Name    : Unicode.CES.Byte_Sequence := "";
   --     Qname         : Unicode.CES.Byte_Sequence := "";
   --     Atts          : Sax.Attributes.Attributes'Class) is

   --     Pos: Position;
   --  begin
   --     if Local_Name = "trkpt" then
   --        Pos.Lon := Coord'Value (Get_Value (Atts, "lon"));
   --        Pos.Lat := Coord'Value (Get_Value (Atts, "lat"));
   --        Handler.Points.Append (Pos);
   --     end if;
   --  end Start_Element;

   --  overriding procedure End_Element
   --    (Handler       : in out Reader;
   --     Namespace_URI : Unicode.CES.Byte_Sequence := "";
   --     Local_Name    : Unicode.CES.Byte_Sequence := "";
   --     Qname         : Unicode.CES.Byte_Sequence := "") is null;
   --  begin
   --     if Local_Name = "trkpt" then
   --        Handler.N_Points := Handler.N_Points + 1;
   --     end if;
   --  end End_Element;

   procedure Read_Points (R: in out Reader; Path: in String) is
      Input    : File_Input;
      T_Reader : Tree_Reader;
      List     : Node_List;
      N        : Node;
      Doc      : Document;
   begin
      Set_Public_Id (Input, "GPX data");
      Set_System_Id (Input, Path);
      Open (Path, Input);

      Set_Feature (T_Reader, Namespace_Feature, True);
      Set_Feature (T_Reader, Namespace_Prefixes_Feature, True);
      Set_Feature (T_Reader, Validation_Feature, False);

      Parse (T_Reader, Input);
      Close (Input);

      R.T_Reader := T_Reader;
      Doc := Get_Tree (T_Reader);

      List := Get_Elements_By_Tag_Name (Doc, "trkpt");
      R.Points.Reserve_Capacity (Ada.Containers.Count_Type(Length (List)));
      for Index in 1 .. Length (List) loop
         N := Item (List, Index - 1);
         declare
            Pnt : Point := Read_Point (N);
         begin
            R.Points.Append (Pnt);
         end;
      end loop;
      Free (List);
   end Read_Points;

   procedure Store_Elevation (R : in out Reader) is
      List     : Node_List;
      N        : Node;
      Doc      : Document;
      Children : Node_List;
   begin
      Doc := Get_Tree (R.T_Reader);
      List := Get_Elements_By_Tag_Name (Doc, "trkpt");
      for Index in 1 .. Length (List) loop
         N := Item (List, Index - 1);
         Children := DOM.Core.Elements.Get_Elements_By_Tag_Name (N, "ele");
         if Length (Children) = 0 then
            --  only add the <ele> child node if there is none
            declare
               Ele_Node : DOM.Core.Element := Create_Element (Doc, "ele");
               Text_Node : DOM.Core.Text := Create_Text_Node (Doc, Elevation_Type'Image (R.Points(Index - 1).Elevation));
            begin
               Text_Node := Append_Child (Ele_Node, Text_Node);
               Ele_Node := Append_Child (N, Ele_Node);
            end;
         end if;
         Free (Children);
      end loop;
      Free (List);
   end Store_Elevation;

   procedure Write (R : in Reader; File_Path : String) is
      use Ada.Streams.Stream_IO;
      Output_File   : File_Type;
      Output_Stream : Stream_Access;
      Doc      : Document;
   begin
      Doc := Get_Tree (R.T_Reader);
      Create (Output_File, Name => File_Path);
      Output_Stream := Stream (Output_File);
      DOM.Core.Nodes.Write (Output_Stream, Doc, Pretty_Print => True);
      Close (Output_File);
   end;

   procedure Free (R : in out Reader) is
   begin
      Free (R.T_Reader);
   end Free;
end GPX_Reader;
