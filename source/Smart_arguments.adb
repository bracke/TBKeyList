--------------------------------------------------------------
--                                                                                                                         
--                       Smart_Arguments                                                                                                       
--                                                                          
--                  Copyright (C) 2003 Jeffrey Creem          
--                                                                          
-- This library is free software; you can redistribute it and/or     
-- modify it under the terms of the GNU General Public               
-- License as published by the Free Software Foundation; either      
-- version 2 of the License, or (at your option) any later version.  
--                                                                   
-- This library is distributed in the hope that it will be useful,   
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
-- General Public License for more details.                          
--                                                                  
-- You should have received a copy of the GNU General Public         
-- License along with this library; if not, write to the             
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      
-- Boston, MA 02111-1307, USA.                                       
--                                                                   
-- As a special exception, if other files instantiate generics from  
-- this unit, or you link this unit with other files to produce an   
-- executable, this  unit  does not  by itself cause  the resulting  
-- executable to be covered by the GNU General Public License. This  
-- exception does not however invalidate any other reasons why the   
-- executable file  might be covered by the  GNU Public License.     
--

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Text_IO;
use Text_IO;
use Ada.Strings.Unbounded;

package body Smart_Arguments is

  --
  -- There are a few things we could to do optimize this package
  -- but since parseing command line arguments is not expected to
  -- drive run time for even the most demanding command line, they
  -- have not been done. 
  -- Some ideas if I end up being wrong:
  --    1) In the linked list of arguments we could add a new
  --       element to the derived records that contains the index
  --       on the command line of the argument. This would make
  --       repeated calls to get subargs and args present go a lot
  --       faster.
  --    2) I can't think of any good reasons why we really need to
  --       pass the full record back to the user on the create. This
  --       could just be a pointer to one of the elements in our
  --       linked list.
  --    3) The routine All_Required_Arguments_Present is a N**2
  --       algorithm. Doing something here along with #1 would
  --       make this linear.

  Format_Output_Width : Text_IO.Count := 80;
  Max_Deadspace_On_Line : Natural := Integer(Format_Output_Width);


  --
  -- Procedure/Function: Format
  --
  -- Purpose : Writes the given Item to standard output such that
  --           if the addition of the current output and the item
  --           would extend past Format_Output_Width, we break
  --           the line and continue writing the rest of Item on
  --           subsequent lines. When we do the line breaks, we
  --           indent Indent spaces prior to starting output.
  --  
  --           As input spills to new lines, Indent spaces will
  --           be displayed prior to restarting the output.
  --
  procedure Format(Item : in String; Indent : in Natural := 0) is

    Last      : Integer;
    Space_Map : Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set(' ');
    Blank_Index : Integer;

  begin

    --
    -- If the Item fits, write it. If not, write what fits on this line then
    -- recurse to finish the rest. This should be made smarter to break at words if
    -- possible. 
    --
    if (Text_IO.Col + Item'length) <= Format_Output_Width then

      --
      -- If we are creating indented output, consume leading whitespace if present.
      --
      if Indent /= 0 then

        Text_IO.Put(Ada.Strings.Fixed.Trim(Item, Ada.Strings.Left));

      else

        Text_IO.Put(Item);

      end if;

    else
      --
      -- Figure out the portion of the Item that fits on the line, write it
      -- out and then recurse to print the rest of the line.
      --
      Last := Item'first + Integer(Format_Output_Width - Text_IO.Col);

      --
      -- If we appear to be breaking in the middle of a word, try not to.
      --
      if Item(Last) /= ' ' then

        Blank_Index := Ada.Strings.Fixed.Index
          (Source => Item(Item'first .. Last),
          Set    => Space_Map,
          Going  => Ada.Strings.Backward);

        if Last - Blank_Index < Max_Deadspace_On_Line then
          Last := Blank_Index;
        end if;

      end if;

      if Indent /= 0 then

        Text_IO.Put_Line(Ada.Strings.Fixed.Trim(Item(Item'first .. Last), Ada.Strings.Left));

      else

        Text_IO.Put_Line(Item(Item'first .. Last));

      end if;


      if Indent /= 0 then
        Text_IO.Set_Col(Text_IO.Count(Indent));
      end if;

      Format(Item => Item(Last + 1 .. Item'last), Indent => Indent);

    end if;

  end Format;


  procedure Format(Item : in Ada.Strings.Unbounded.Unbounded_String;
      Indent : Natural := 0) is

  begin

    Format(Item => To_String(Item), Indent => Indent);

  end Format;

  --
  -- Procedure/Function: Format_Newline
  --
  -- Purpose : Writes a new line in a manner that is "Format" aware.
  --

  procedure Format_Newline is

  begin

    --
    -- For now this is pretty easy since we just use text_io for
    -- bookkeeping.
    --
    Text_IO.New_Line;

  end Format_Newline;



  procedure Create_Argument (Argument : out Argument_Type;
      Short_Form              : in String;
      Long_Form               : in String := "";
      Number_Required_Subargs : in Natural := 0;
      Required                : in Boolean := false;
      Description             : in String := "";
      Subarg_Description      : in String := "") is

    New_Argument : Access_Argument_List_Node_Type;

  begin

    New_Argument := new Argument_List_Node_Type'(Short_Form => To_Unbounded_String(Short_Form),
      Long_Form               => To_Unbounded_String(Long_Form),
      Required                => Required,
      Number_Required_Subargs => Number_Required_Subargs,
      Subarg_Description      => To_Unbounded_String(Subarg_Description),
      Description             => To_Unbounded_String(Description),
      Next                    => Argument_List);

    Argument_List := New_Argument;

    Argument := Argument_Type(New_Argument.all);

  end Create_Argument;



  --
  -- Returns the Index for Ada.Command_Line.Argument that contains the
  -- argument indicated by the parameter. Raises Argument_Not_Present_Error if
  -- the argument can not be found.
  --
  function Index_Of_Argument(Argument : in Argument_Type) return Integer is

  begin

    --
    -- Each time through this loop we check the next element on the
    -- command line against the given argument and if it matches we
    -- return the index. If we exit the loop without finding a match
    -- a Argument_Not_Present_Error will be raised.
    --
    for Current_Arg_Index in 1 .. Ada.Command_Line.Argument_Count loop

      declare
        Current_Arg : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Command_Line.Argument(Current_Arg_Index));
      begin

        if Current_Arg = Argument.Short_Form or
            (Current_Arg = Argument.Long_Form and
            Argument.Long_Form /= Ada.Strings.Unbounded.Null_Unbounded_String) then

          return Current_Arg_Index;

        end if;

      end;

    end loop;

    raise Argument_Not_Present_Error;

  end Index_Of_Argument;


  function Argument_Present (
      Argument : in Argument_Type )
      return Boolean is

  begin

    return Index_Of_Argument(Argument) > 0;

  exception

    when Argument_Not_Present_Error =>

      return false;

  end Argument_Present;



  function Get_Subargument (
      Argument : in Argument_Type;
      Subargument_Index : in     Positive )
      return String is

    Base_Argument_Index : Natural := Index_Of_Argument(Argument);

  begin

    return Ada.Command_Line.Argument(Base_Argument_Index + Subargument_Index);

  end Get_Subargument;


  function Argument_Image(Argument : in Argument_Type;
      Short_Form : in Boolean := true) return String is

  begin
    if Short_Form then
      return To_String(Argument.Short_Form);
    else
      return To_String(Argument.Long_Form);
    end if;
  end Argument_Image;



  --
  -- Procedure/Function: All_Required_Arguments_Present
  --
  -- Purpose: Returns true if all of the arguments that were created
  --          with Required set are present on the command line.
  --

  function All_Required_Arguments_Present return Boolean is

    Current_Argument : Access_Argument_List_Node_Type := Argument_List;

  begin

    --
    -- Each time through this loop, we see if the current argument
    -- from the argument list is required and if it is, we check
    -- to see if it is on the command line. If not, we return false
    -- immediately. If we exit the loop, then all required arguments
    -- were found.
    --
    while Current_Argument /= null loop

      if Current_Argument.Required and then not
          Argument_Present(Current_Argument.all) then

        return false;

      end if;

      Current_Argument := Current_Argument.Next;

    end loop;

    return true;

  end All_Required_Arguments_Present;



  function Longest_Argument_Pair return Natural is

    Longest_Pair : Natural := 0;
    Current_Argument : Access_Argument_List_Node_Type := Argument_List;
    Current_Length   : Natural;

  begin
    --
    -- Each time through this loop, we create write an argument description
    -- for the an argument and then move on to the next argument until none
    -- are left.
    --
    while Current_Argument /= null loop

      Current_Length := Length(Current_Argument.Short_Form) + Length(Current_Argument.Long_Form);

      if Current_Length > Longest_Pair then

        Longest_Pair := Current_Length;

      end if;

      Current_Argument := Current_Argument.Next;

    end loop;

    return Longest_Pair;

  end Longest_Argument_Pair;


  procedure Display_Help
      (Format_For_Output_Width     : in Positive := 79;
      Short_Form_Only              : in Boolean;
      Pre_Argument_Command_Summary : in String := "") is

    Current_Argument : Access_Argument_List_Node_Type := Argument_List;
    Arg_Indent       : Natural := Longest_Argument_Pair + 3;
    Max_Indent       : Natural := Format_For_Output_Width - (Format_For_Output_Width * 2) / 3;

    Actual_Indent    : Natural := Arg_Indent;

  begin

    if Actual_Indent > Max_Indent then
      Actual_Indent := Max_Indent;
    end if;

    Format_Output_Width   := Text_IO.Count(Format_For_Output_Width);
    Max_Deadspace_On_Line := Integer(Format_Output_Width) / 8;

    Format(Item => "Usage: ");

    Format(Item => Ada.Command_Line.Command_Name & ' ');

    --
    -- Each time through this loop, we create write an argument description
    -- for the an argument and then move on to the next argument until none
    -- are left.
    --
    while Current_Argument /= null loop

      if Current_Argument.Required then

        Format(Item => Current_Argument.Short_Form);

      else

        Format(Item => "[" & Current_Argument.Short_Form);

      end if;

      if Current_Argument.Number_Required_Subargs > 0 then

        Format(Item => " " & Current_Argument.Subarg_Description);

      elsif Length(Current_Argument.Subarg_Description) > 0 then

        Format(Item => " [" & Current_Argument.Subarg_Description & "]");

      end if;


      if not Current_Argument.Required then

        Format(Item => "] ");

      else

        Format(Item => " ");

      end if;

      Current_Argument := Current_Argument.Next;

    end loop;

    if not Short_Form_Only then

      Format_Newline;

      if Pre_Argument_Command_Summary'length > 0 then
        Format(Pre_Argument_Command_Summary);
        Format_Newline;

      end if;

      Current_Argument := Argument_List;

      --
      -- Each time through this loop, we write out the short and long form for an
      -- argument followed by the arguments description.
      --
      while Current_Argument /= null loop

        Format(Item => "  " & Current_Argument.Short_Form & '/' & Current_Argument.Long_Form);

        Text_IO.Set_Col(Text_IO.Count(Actual_Indent));

        Format(Item   => Current_Argument.Description,
          Indent => Actual_Indent);

        Format_Newline;
        Format_Newline;

        Current_Argument := Current_Argument.Next;

      end loop;

    end if;

    Text_IO.New_Line;

  end Display_Help;


end Smart_Arguments;

