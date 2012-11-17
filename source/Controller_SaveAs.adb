with RASCAL.Error;               use RASCAL.Error;
with RASCAL.MessageTrans;        use RASCAL.MessageTrans;
with RASCAL.OS;                  use RASCAL.OS;
with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.FileExternal;        use RASCAL.FileExternal;
with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.FileName;

with Main;                       use Main;
with Interfaces.C;               use Interfaces.C;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Process;
with Reporter;

package body Controller_SaveAs is

   --

   package Error        renames RASCAL.Error;
   package MessageTrans renames RASCAL.MessageTrans;
   package OS           renames RASCAL.OS;          
   package Utility      renames RASCAL.Utility;     
   package FileExternal renames RASCAL.FileExternal;
   package WimpTask     renames RASCAL.WimpTask;    
   package FileName     renames RASCAL.FileName;    

   --

   procedure Handle (The : in TEL_SaveAs_SaveToFile) is

      Object : Object_ID := Get_Self_Id(Main_Task);
      Name   : String    := To_Ada(The.Event.all.Filename);
      E      : Error_Pointer          := Get_Error (Main_Task);
      M      : Error_Message_Pointer  := new Error_Message_Type;
      Result : Error_Return_Type      := XButton1;
      Dummy  : UString;
   begin
      Hide_Object(Object);
      if FileExternal.Exists (Name) then
         M.all.Token(1..9) := "OVERWRITE";
         M.all.Category    := Warning;
         M.all.Flags       := Error_Flag_Cancel;
         Dummy             := U(MessageTrans.Lookup("OVERWRITEBUTTON",E.all.Msg_Handle));
         M.all.Buttons(1..Length(Dummy)) := S(Dummy);
         Result                          := Error.Show_Message (E,M);
      end if;
      if Result = XButton1 then
         Process.Generate_List(S(Source_Path));
         Process.Write_List(Name);
      end if;
   end Handle;

   --

end Controller_SaveAs;
