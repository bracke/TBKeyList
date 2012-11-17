with RASCAL.OS;                  use RASCAL.OS;
with RASCAL.Utility;             use RASCAL.Utility;
with RASCAL.Toolbox;             use RASCAL.Toolbox;
with RASCAL.Utility;             use RASCAL.Utility;

with Main;                       use Main;
with Interfaces.C;               use Interfaces.C;
with StuffList;                  use StuffList;
with Reporter;
with ADa.Exceptions;

package body Controller_DataLoad is

   --

   package OS       renames RASCAL.OS;
   package Utility  renames RASCAL.Utility;
   package Toolbox  renames RASCAL.Toolbox;

   --

   procedure Handle (The : in MEL_Message_DataLoad) is

      Path          : String  := To_Ada(The.Event.all.Full_Path);
      File_Type     : Integer := The.Event.all.File_Type;
   begin
      case File_Type is
      when 16#fae# => Source_Path := U(Path);
                      Show_Object (saveas_objectid,0,0,AtPointer);
      when others  => Report_Error("NORESFILE","");
      end case;
   end Handle;

   --

end Controller_DataLoad;
