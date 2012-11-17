--------------------------------------------------------------------------------
--                                                                            --
-- Copyright (C) 2004, RISC OS Ada Library (RASCAL) developers.               --
--                                                                            --
-- This library is free software; you can redistribute it and/or              --
-- modify it under the terms of the GNU Lesser General Public                 --
-- License as published by the Free Software Foundation; either               --
-- version 2.1 of the License, or (at your option) any later version.         --
--                                                                            --
-- This library is distributed in the hope that it will be useful,            --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of             --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU           --
-- Lesser General Public License for more details.                            --
--                                                                            --
-- You should have received a copy of the GNU Lesser General Public           --
-- License along with this library; if not, write to the Free Software        --
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA    --
--                                                                            --
--------------------------------------------------------------------------------

-- $Author$
-- $Date$
-- $Revision$

with RASCAL.Utility; use RASCAL.Utility;
with RASCAL.Convert;

package body RASCAL.ToolboxEventNames is

   Toolbox_Events : constant array (0..2) of ustring
                  := (U("Toolbox_Error"),
                      U("Toolbox_ObjectAutoCreated"),
                      U("Toolbox_ObjectDeleted"));

   Toolbox_Base : constant integer := 16#44EC0#;
   

   Menu_Events : constant array (0..3) of ustring
               := (U("Menu_AboutToBeShown"),
                   U("Menu_HasBeenHidden"),
                   U("Menu_SubMenu"),
                   U("Menu_Selection"));

   Menu_Base : constant integer := 16#828C0#;


   ColourDBox_Events : constant array (0..3) of ustring
                     := (U("ColourDbox_AboutToBeShown"),
                         U("ColourDbox_DialogueCompleted"),
                         U("ColourDbox_ColourSelected"),
                         U("ColourDbox_ColourChanged"));

   ColourDBox_Base : constant integer := 16#829C0#;


   ColourMenu_Events : constant array (0..2) of ustring
                     := (U("ColourMenu_AboutToBeShown"),
                         U("ColourMenu_HasBeenHidden"),
                         U("ColourMenu_Selection"));

   ColourMenu_Base : constant integer := 16#82980#;


   DCS_Events : constant array (0..4) of ustring
              := (U("DCS_AboutToBeShown"),
                  U("DCS_Discard"),
                  U("DCS_Save"),
                  U("DCS_DialogueCompleted"),
                  U("DCS_Cancel"));

   DCS_Base : constant integer := 16#82A80#;


   FileInfo_Events : constant array (0..1) of ustring
                   := (U("FileInfo_AboutToBeShown"),
                       U("FileInfo_DialogueCompleted"));

   FileInfo_Base : constant integer := 16#82AC0#;


   FontDBox_Events : constant array (0..2) of ustring
                   := (U("FontDbox_AboutToBeShown"),
                       U("FontDbox_DialogueCompleted"),
                       U("FontDbox_ApplyFont"));

   FontDBox_Base : constant integer := 16#82A00#;


   FontMenu_Events : constant array (0..2) of ustring
                   := (U("FontMenu_AboutToBeShown"),
                       U("FontMenu_HasBeenHidden"),
                       U("FontMenu_Selection"));

   FontMenu_Base : constant integer := 16#82A40#;


   Iconbar_Events : constant array (0..2) of ustring
                  := (U("Iconbar_Clicked"),
                      U("Iconbar_SelectAboutToBeShown"),
                      U("Iconbar_AdjustAboutToBeShown"));

   Iconbar_Base : constant integer := 16#82900#;


   PrintDBox_Events : constant array (0..5) of ustring
                    := (U("PrintDbox_AboutToBeShown"),
                        U("PrintDbox_DialogueCompleted"),
                        U("PrintDbox_SetUpAboutToBeShown"),
                        U("PrintDbox_Save"),
                        U("PrintDbox_SetUp"),
                        U("PrintDbox_Print"));

   PrintDBox_Base : constant integer := 16#82B00#;


   ProgInfo_Events : constant array (0..2) of ustring
                   := (U("ProgInfo_AboutToBeShown"),
                       U("ProgInfo_DialogueCompleted"),
                       U("ProgInfo_LaunchWebPage"));

   ProgInfo_Base : constant integer := 16#82B40#;


   Quit_Events : constant array (0..3) of ustring
               := (U("Quit_AboutToBeShown"),
                   U("Quit_Quit"),
                   U("Quit_DialogueCompleted"),
                   U("Quit_Cancel"));

   Quit_Base : constant integer := 16#82A90#;


   SaveAs_Events : constant array (0..5) of ustring
                 := (U("SaveAs_AboutToBeShown"),
                     U("SaveAs_DialogueCompleted"),
                     U("SaveAs_SaveAboutToBeDone"),
                     U("SaveAs_SaveToFile"),
                     U("SaveAs_FillBuffer"),
                     U("SaveAs_SaveCompleted"));

   SaveAs_Base : constant integer := 16#82BC0#;

   Scale_Events : constant array (0..2) of ustring
                := (U("Scale_AboutToBeShown"),
                    U("Scale_DialogueCompleted"),
                    U("Scale_ApplyFactor"));


   Scale_Base : constant integer := 16#00082C00#;

   Window_Events : constant array (0..15) of ustring
                 := (U("Window_AboutToBeShown"),
                     U("ActionButton_Selected"),
                     U("OptionButton_StateChanged"),
                     U("RadioButton_StateChanged"),
                     U("DisplayField_ValueChanged"),
                     U("WritableField_ValueChanged"),
                     U("Slider_ValueChanged"),
                     U("Draggable_DragStarted"),
                     U("Draggable_DragEnded"),
                     U("PopUp_AboutToBeShown"),
                     U("Adjuster_Clicked"),
                     U("NumberRange_ValueChanged"),
                     U("StringSet_ValueChanged"),
                     U("StringSet_AboutToBeShown"),
                     U("Window_HasBeenHidden"),
                     U("Window_GadgetLostFocus"));

   Window_Base : constant integer := 16#82880#;

   ResDisplay_Events : constant array (0..3) of ustring
                     := (U("ResDisplay_AboutToBeShown"),
                         U("ResDisplay_HasBeenHidden"),
                         U("ResDisplay_SelectionModified"),
                         U("ResDisplay_Activated"));

   ResDisplay_Base : constant integer := 16#1001C0#;


   ToolAction_Events : constant array (0..0) of ustring
                     := (0=>U("ToolAction_Selected"));

   ToolAction_Base : constant integer := 16#140140#;

   function Get_EventName(Event : in Integer) return String is
   begin
      case Event is
      when 16#0#                                                                           => return "No event";
      when 16#1#..16#FFFF#                                                                 => return "User event: &" & Convert.Integer_To_Hex(Event,6);
      when 16#10000#..16#3FFFF#                                                            => return "Protocol event: &" & Convert.Integer_To_Hex(Event,6);
      when Toolbox_Base+Toolbox_Events'First..Toolbox_Base+Toolbox_Events'Last             => return S(Toolbox_Events(Event-Toolbox_Base));
      when Quit_Base+Quit_Events'First..Quit_Base+Quit_Events'Last                         => return S(Quit_Events(Event-Quit_Base));
      when Menu_Base+Menu_Events'First..Menu_Base+Menu_Events'Last                         => return S(Menu_Events(Event-Menu_Base));
      when ColourDBox_Base+ColourDBox_Events'First..ColourDBox_Base+ColourDBox_Events'Last => return S(ColourDBox_Events(Event-ColourDBox_Base));
      when ColourMenu_Base+ColourMenu_Events'First..ColourMenu_Base+ColourMenu_Events'Last => return S(ColourMenu_Events(Event-ColourMenu_Base));
      when DCS_Base+DCS_Events'First..DCS_Base+DCS_Events'Last                             => return S(DCS_Events(Event-DCS_Base));
      when FileInfo_Base+FileInfo_Events'First..FileInfo_Base+FileInfo_Events'Last         => return S(FileInfo_Events(Event-FileInfo_Base));
      when FontDBox_Base+FontDBox_Events'First..FontDBox_Base+FontDBox_Events'Last         => return S(FontDBox_Events(Event-FontDBox_Base));
      when FontMenu_Base+FontMenu_Events'First..FontMenu_Base+FontMenu_Events'Last         => return S(FontMenu_Events(Event-FontMenu_Base));
      when Iconbar_Base+Iconbar_Events'First..Iconbar_Base+Iconbar_Events'Last             => return S(Iconbar_Events(Event-Iconbar_Base));
      when PrintDBox_Base+PrintDBox_Events'First..PrintDBox_Base+PrintDBox_Events'Last     => return S(PrintDBox_Events(Event-PrintDBox_Base));
      when ProgInfo_Base+ProgInfo_Events'First..ProgInfo_Base+ProgInfo_Events'Last         => return S(ProgInfo_Events(Event-ProgInfo_Base));
      when SaveAs_Base+SaveAs_Events'First..SaveAs_Base+SaveAs_Events'Last                 => return S(SaveAs_Events(Event-SaveAs_Base));
      when Scale_Base+Scale_Events'First..Scale_Base+Scale_Events'Last                     => return S(Scale_Events(Event-Scale_Base));
      when Window_Base+Window_Events'First..Window_Base+Window_Events'Last                 => return S(Window_Events(Event-Window_Base));
      when ResDisplay_Base+ResDisplay_Events'First..ResDisplay_Base+ResDisplay_Events'Last => return S(ResDisplay_Events(Event-ResDisplay_Base));
      when ToolAction_Base+ToolAction_Events'First..ToolAction_Base+ToolAction_Events'Last => return S(ToolAction_Events(Event-ToolAction_Base));
      when others => return "Unknown event: &"  & Convert.Integer_To_Hex(Event,6);
      end case;
   end Get_EventName;

end RASCAL.ToolboxEventNames;
