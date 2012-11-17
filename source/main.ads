with StuffList;                  use StuffList;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;

with RASCAL.WimpTask;            use RASCAL.WimpTask;
with RASCAL.OS;                  use RASCAL.OS;
with RASCAL.Utility;             use RASCAL.Utility;

package Main is

   -- Constants
   app_name       : constant String := "TBKeyList";
   Choices_Write  : constant String := "<Choices$Write>." & app_name;
   Choices_Read   : constant String := "Choices:" & app_name;

   --
   Main_Task      : ToolBox_Task_Class;
   main_objectid  : Object_ID             := -1;
   saveas_objectid: Object_ID             := -1;
   main_winid     : Wimp_Handle_Type      := -1;
   Source_Path    : Unbounded_String;

   Model          : StuffList.List;

   Template       : Unbounded_String := U("StrongHelp");

   --

   procedure Read_Choices;
   
   --

   procedure Report_Error (Token : in String;
                           Info  : in String);

   --

   procedure Main;

   --

 end Main;