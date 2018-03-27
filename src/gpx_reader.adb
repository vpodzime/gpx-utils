with Sax.Readers;        use Sax.Readers;
with DOM.Readers;        use DOM.Readers;
with DOM.Core;           use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with DOM.Core.Attrs;     use DOM.Core.Attrs;
with Input_Sources.File; use Input_Sources.File;
with Unicode.CES;

with Position_Types;           use Position_Types;

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
      Doc      : Document;
      List     : Node_List;
      N        : Node;
   begin
      Set_Public_Id (Input, "GPX data");
      Set_System_Id (Input, Path);
      Open (Path, Input);

      Set_Feature (T_Reader, Validation_Feature, False);

      Parse (T_Reader, Input);
      Close (Input);

      Doc := Get_Tree (T_Reader);

      List := Get_Elements_By_Tag_Name (Doc, "trkpt");
      for Index in 1 .. Length (List) loop
         N := Item (List, Index - 1);
         declare
            Pos: Position;
            A:   Attr;
         begin
            A := Get_Named_Item (Attributes (N), "lon");
            Pos.Lon := Coord'Value (Value(A));
            A := Get_Named_Item (Attributes (N), "lat");
            Pos.Lat := Coord'Value (Value(A));
            R.Points.Append(Pos);
         end;
      end loop;

      Free (List);
      Free (T_Reader);
   end Read_Points;
end GPX_Reader;
