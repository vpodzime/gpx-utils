with Sax.Readers;        use Sax.Readers;
with DOM.Readers;        use DOM.Readers;
with DOM.Core;           use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with DOM.Core.Attrs;     use DOM.Core.Attrs;
with Input_Sources.File; use Input_Sources.File;
with Ada.Text_IO;

procedure GPX_Elevator is
   package T_IO renames Ada.Text_IO;

   Input  : File_Input;
   Reader : Tree_Reader;
   Doc    : Document;
   List   : Node_List;
   N      : Node;
   A      : Attr;
   Count  : Natural := 0;

begin
   Set_Public_Id (Input, "TomTom GPX data");
   Set_System_Id (Input, "sample.gpx");
   Open ("sample.gpx", Input);

   Set_Feature (Reader, Validation_Feature, False);

   T_IO.Put_Line ("Parsing...");
   Parse (Reader, Input);
   Close (Input);

   Doc := Get_Tree (Reader);

   List := Get_Elements_By_Tag_Name (Doc, "trkpt");
   for Index in 1 .. Length (List) loop
      N := Item (List, Index - 1);
      A := Get_Named_Item (Attributes (N), "lon");
      T_IO.Put ("Got point: ");
      T_IO.Put (Value (A) & ", ");
      A := Get_Named_Item (Attributes (N), "lat");
      T_IO.Put_Line (Value (A));
      Count := Count + 1;
   end loop;
   T_IO.Put_Line ("Number of points: " & Natural'Image (Count));

   Free (List);
   Free (Reader);

end GPX_Elevator;
