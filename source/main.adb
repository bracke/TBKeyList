with Controller_SaveAs;         use Controller_SaveAs;
with Controller_DataLoad;       use Controller_DataLoad;
with Controller_Quit;           use Controller_Quit;
with Controller_Internet;       use Controller_Internet;
with Controller_Bugz;           use Controller_Bugz;
with Controller_Choices;        use Controller_Choices;
with Controller_Help;           use Controller_Help;
with Controller_Dummy;          use Controller_Dummy;
with Controller_Error;          use Controller_Error;
with Ada.Text_IO;               use Ada.Text_IO;
with Reporter;                  use Reporter;
with Smart_Arguments;           use Smart_Arguments;
with Ada.Exceptions;
with Ada.Command_Line;
with Process;

with RASCAL.Toolbox;            use RASCAL.Toolbox;
with RASCAL.Utility;            use RASCAL.Utility;
with RASCAL.Error;              use RASCAL.Error;
with RASCAL.MessageTrans;       use RASCAL.MessageTrans;
with RASCAL.ToolboxProgInfo;
with RASCAL.FileExternal;
with RASCAL.FileName;

package body Main is

   --

   package Toolbox                 renames RASCAL.Toolbox;
   package Utility                 renames RASCAL.Utility;        
   package Error                   renames RASCAL.Error;          
   package MessageTrans            renames RASCAL.MessageTrans;   
   package ToolboxProgInfo         renames RASCAL.ToolboxProgInfo;
   package FileExternal            renames RASCAL.FileExternal;   
   package FileName                renames RASCAL.FileName;       
   package WimpTask                renames RASCAL.WimpTask;
   package OS                      renames RASCAL.OS;

   --

   Source   : Smart_Arguments.Argument_Type;
   Target   : Smart_Arguments.Argument_Type;
   Temp     : Smart_Arguments.Argument_Type;

   --
   
   procedure Read_Choices is

      MCB    : Messages_Handle_Type  := Open_File(Choices_Read & ".Choices");
   begin
      begin
         Template := U(MessageTrans.Lookup("TEMPLATE",MCB));
      exception
         when others => Template := U("StrongHelp");
      end;
      if Length(Template) = 0 then
         Template := U("StrongHelp");
      end if;
   exception
      when Messages_File_Is_Closed => Report_Error("MSGCLOSED","");
      when e2 : others => Report_Error("CHOICES",Ada.Exceptions.Exception_Information (e2));
   end Read_Choices;

   --

   procedure Report_Error (Token : in String;
                           Info  : in String) is

      E        : Error_Pointer          := Get_Error (Main_Task);
      M        : Error_Message_Pointer  := new Error_Message_Type;
      Result   : Error_Return_Type;
   begin
      if Get_Status (Main_task) then
         M.all.Token(1..Token'Length) := Token;
         M.all.Param1(1..Info'Length) := Info;
         M.all.Category := Warning;
         M.all.Flags    := Error_Flag_OK;
         Result         := Error.Show_Message (E,M);
      else
         Put_Line(MessageTrans.Lookup(Token,E.all.Msg_Handle,Info));
      end if;
   end Report_Error;

   --

   procedure Main is
      ProgInfo_Window : Object_ID;
   begin
      if FileExternal.Exists(Choices_Read & ".Choices") then
         Read_Choices;
      end if;

      if Ada.Command_Line.Argument_Count > 0 then
         -- CLI - Use
         Create_Argument (Argument   => Source,
                          Short_Form => "-s",
                          Long_Form  => "-source",
                          Required   => true,
                          Description=> "The path of the resource file (&fae).");
   
         Create_Argument (Argument   => Target,
                          Short_Form => "-t",
                          Long_Form  => "-target",
                          Required   => true,
                          Description=> "The target path.");
   
         Create_Argument (Argument   => Temp,
                          Short_Form => "-temp",
                          Long_Form  => "-template",
                          Required   => false,
                          Description=> "The name of the template to be used. It overrides the setting in the choices file.");

         if All_Required_Arguments_Present then
            if not FileExternal.Exists (Get_Subargument(Source,1)) then
               Report_Error("SOURCENOTFOUND","");
            elsif not FileExternal.Exists (FileName.Get_Path(Get_Subargument(Target,1))) then
               Report_Error("TARGETNOTFOUND","");
            else
               if Argument_Present (Temp) then
                  if not FileExternal.Exists ("<TBKeyList$Dir>.Templates." & Get_Subargument(Temp,1)) then
                     Report_Error("TEMPLATENOTFOUND","");
                  else
                     Template := U(Get_Subargument(Temp,1));
                  end if;
               end if;
               Process.Generate_List(Get_Subargument(Source,1));
               Process.Write_List(Get_Subargument(Target,1));
               Read_Choices;
            end if;
         end if;
      else

         -- Messages
         Add_Listener (Main_Task,new MEL_Message_Bugz_Query);
         Add_Listener (Main_Task,new MEL_Message_ConfiX);
         Add_Listener (Main_Task,new MEL_Message_DataLoad);
         Add_Listener (Main_Task,new MEL_Message_Quit);
   
         -- Toolbox Events
         Add_Listener (Main_Task,new TEL_Quit_Quit);
         Add_Listener (Main_Task,new TEL_ViewManual_Type);
         Add_Listener (Main_Task,new TEL_ViewSection_Type);
         Add_Listener (Main_Task,new TEL_ViewIHelp_Type);
         Add_Listener (Main_Task,new TEL_ViewHomePage_Type);
         Add_Listener (Main_Task,new TEL_ViewChoices_Type);
         Add_Listener (Main_Task,new TEL_SendEmail_Type);
         Add_Listener (Main_Task,new TEL_CreateReport_Type);
         Add_Listener (Main_Task,new TEL_Toolbox_Error);
         Add_Listener (Main_Task,new TEL_SaveAs_SaveToFile);
         Add_Listener (Main_Task,new TEL_Dummy);
   
         -- Start task
         WimpTask.Set_Resources_Path(Main_Task,"<TBKeyListRes$Dir>");
         WimpTask.Initialise(Main_Task);
   
         ProgInfo_Window := Toolbox.Create_Object("ProgInfo",From_Template);
         ToolboxProgInfo.Set_Version(ProgInfo_Window,MessageTrans.Lookup("VERS",Get_Message_Block(Main_Task)));
         saveas_objectid := Toolbox.Create_Object("SaveAs",From_Template);
   
         WimpTask.Poll(Main_Task);

      end if;
   exception
      when e: others => Report_Error("UNTRAPPED",Ada.Exceptions.Exception_Information (e));
   end Main;

   --

end Main;
