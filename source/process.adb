with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.FileInternal;        use RASCAL.FileInternal;
with RASCAL.Heap;                use RASCAL.Heap;
with RASCAL.Convert;
with RASCAL.FileExternal;        use RASCAL.FileExternal;
with RASCAL.ToolboxEventNames;
with RASCAL.Keyboard;
with RASCAL.Memory;

with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with System.Unsigned_Types;      use System.Unsigned_Types;
with Main;                       use Main;
with StuffList;                  use StuffList;                  
with Templates_Parser;
with Ada.Exceptions;
with Reporter;

package body Process is

   --

   package Utility             renames RASCAL.Utility;
   package FileInternal        renames RASCAL.FileInternal;     
   package Heap                renames RASCAL.Heap;             
   package Convert             renames RASCAL.Convert;          
   package FileExternal        renames RASCAL.FileExternal;     
   package ToolboxEventNames   renames RASCAL.ToolboxEventNames;
   package Keyboard            renames RASCAL.Keyboard;         
   package Memory              renames RASCAL.Memory;           

   --

   use type Templates_Parser.Vector_Tag;

   Line : Unbounded_String;

   Offset,i    : Integer       := 0;
   TabSpace    : Character_Set := To_Set("  " & ASCII.HT);
   Extent      : Integer;

   --

   procedure Read_Buffer(File_Buffer : in Heap_Block_Type) is
   begin
      Line   := U(Memory.Get_Line (Heap.Get_Address(File_Buffer),Offset));
      Offset := Offset + Length(Line)+1;
      Trim(Line,TabSpace,TabSpace);
   end Read_Buffer;

   --

   procedure Find (Str         : in String;
                   File_Buffer : in Heap_Block_Type) is
   begin
      loop
         exit when not (Offset < Extent);
         Read_Buffer(File_Buffer);
         if Length(Line) > 0 then
            exit when Index (Line,Str) > 0;
         end if;
      end loop;
   end Find;

   --

   function Get_Token(File_Buffer : in Heap_Block_Type) return Unbounded_String is
   begin
      if Offset < Extent then
         Read_Buffer(File_Buffer);
         if Length(line) > 0 then
            i := Index (Line,":");
            if i > 0 then
               return U(Slice(Line,i+1,Length(Line)));
            end if;
            return Line;
         end if;
      end if;
      return U("");
   end Get_Token;

   --

   procedure Write_List (Target : in String) is

      i        : StuffList.Position;
      Item     : Element_Pointer;
      Key_Name : Unbounded_String;
      Key_Event: Unbounded_String;
      Code     : Integer;

      Temp     : String := "<Wimp$ScrapDir>.TBKeyList" & ".List";

      Windows,Keys,Shows,Events : Templates_Parser.Vector_Tag;
      Key_Table_Translations    : Templates_Parser.Translate_Table (1..4);

      procedure Add (Window : in String;
                     Key    : in String;
                     Show   : in String;
                     Event  : in String) is
      begin
         Windows := Windows & Window;
         Keys    := Keys & Key;
         Shows   := Shows & Show;
         Events  := Events & Event;
      end Add;

   begin
      if not isEmpty(Model) then
         i := First(Model);
         loop
            Item := Retrieve (Model,i);
            begin
               if Element (Item.all.Key_Code,1) = '&' then
                  Item.all.Key_Code := U(Slice (Item.all.Key_Code,2,Length(Item.all.Key_Code)));
                  Code := Convert.Hex_To_Integer(S(Item.all.Key_Code));
               else
                  Code := strint(S(Item.all.Key_Code));
               end if;
               Key_Name := U(Keyboard.Get_KeyName(Code));
            exception
               when others => Code := 0; Key_Name := U("");
            end;            
            if Length(key_Name) = 0 then
               Key_Name := Item.all.Key_Code;
            end if;

            begin
               if Element (Item.all.Key_Event,1) = '&' then
                  Item.all.Key_Event := U(Slice (Item.all.Key_Event,2,Length(Item.all.Key_Event)));
                  Code := Convert.Hex_To_Integer(S(Item.all.Key_Event));
               else
                  Code := strint(S(Item.all.Key_Event));
               end if;
               Key_Event := U(ToolboxEventNames.Get_EventName(Code));
            exception
               when others => Code := 0; Key_Event := U("");
            end;            
            if Length(Key_Event) = 0 then
               Key_Event := Item.all.Key_Event;
            end if;

            Add (S(Item.all.Window),
                 S(Key_Name),
                 S(Item.all.Key_Show),
                 S(Key_Event));

            exit when IsLast(Model,i);
            GoAhead(Model,i);
         end loop;
         Delete_List(Model);                      
      end if;

      if FileExternal.Exists ("<TBKeyList$Dir>.Templates." & S(Template)) then
         FileExternal.Close_AllInPath ("<TBKeyList$Dir>.Templates." & S(Template));

         Key_Table_Translations(1) := (Templates_Parser.Assoc ("WINDOW",Windows));
         Key_Table_Translations(2) := (Templates_Parser.Assoc ("KEY",Keys));
         Key_Table_Translations(3) := (Templates_Parser.Assoc ("SHOW",Shows));
         Key_Table_Translations(4) := (Templates_Parser.Assoc ("EVENT",Events));

         declare
            File : FileHandle_Type(new UString'(U(Temp)),Write);
         begin
            FileInternal.Put_String
              (File,Templates_Parser.Parse ("<TBKeyList$Dir>.Templates." & S(Template), Key_Table_Translations));
         exception
            when others => Report_Error("PARSEERROR","");
         end;   
   
         FileExternal.Set_File_Type(Temp,16#fff#);         
         FileExternal.Move (Temp,Target,Copy_Option_Delete+Copy_Option_Force);
      else
         Report_Error("TEMPLATENOTFOUND","");
      end if;
      FileExternal.Wipe ("<Wimp$ScrapDir>.TBKeyList");
      FileExternal.Delete_File ("<Wimp$ScrapDir>.TBKeyList");
   end Write_List;

   --

   procedure Generate_List (Source : in String) is
      ResText : String := "<Wimp$ScrapDir>.TBKeyList" & ".ResText";
      Window,Flags,Key_Code,Key_Event,Key_Show : Unbounded_String;
   begin
      if not FileExternal.Exists ("<Wimp$ScrapDir>.TBKeyList") then
         FileExternal.Create_Directory ("<Wimp$ScrapDir>.TBKeyList");
      end if;
      FileExternal.Close_AllInPath ("<Wimp$ScrapDir>.TBKeyList");
       
      Call_OS_CLI ("WimpTask Run <CCRes$Dir> " & Source & " " & ResText);

      if FileExternal.Exists(ResText) then
         declare
            File        : FileHandle_Type(new UString'(U(ResText)),Read);
            File_Size   : Integer       := FileInternal.Get_Extent(File);
            File_Buffer : Heap_Block_Type(File_Size+1);
         begin
            Extent := File_Size;
            FileInternal.Load_File (ResText,Heap.Get_Address(File_Buffer));
            offset := 0;
            loop
               exit when not (Offset < Extent);
               Read_Buffer (File_Buffer);
               if Length(Line) > 0 then
                  if Index (Line,"window_object {") > 0 then
                     Find("object_name:",File_Buffer);
                     if Offset < Extent then
                        i := Index (Line,":");
                        Window := U(Slice(Line,i+2,Length(Line)-1));
                     end if;
                  elsif Index (Line,"keyboardshortcut_object {") > 0 then
                     Flags     := Get_Token(File_Buffer);
                     Key_Code  := Get_Token(File_Buffer);
                     Key_Event := Get_Token(File_Buffer);
                     Key_Show  := Get_Token(File_Buffer);
                     AddToRear (Model,new Item_Type'(Window,Flags,Key_Code,Key_Event,Key_Show));
                  end if;
               end if;
            end loop;
         end;
         FileExternal.Delete_File (ResText);
      end if;   
 exception
      when ex : others => Report_Error("GENLIST",Ada.Exceptions.Exception_Information (ex));      
   end Generate_List;

   --

end Process;