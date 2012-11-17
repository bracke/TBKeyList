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
-- Description
--  This package is designed to simplify the use of command 
--  line arguments for controlling a program. The built in
--  Ada facilities for command line argument processing are 
--  very basic. This package provide a higher level facility
--  for dealing with command lines. It is similar in purpose to GNU
--  getopts however if one is expecting this package to act
--  in the same manner they will be disappointed. 
--
--  The concept that this package uses is that client programs
--  make a series of calls to "create" the arguments that
--  the program can process. Once all of the parameters
--  are registered locally with this package then a simple
--  unified help output can be created and the client program
--  can refer to the command line arguments in a more meaningful
--  manner than Ada.Command_Line.Argument(4);
--
--                                                                          

with Ada.Strings.Unbounded;

package Smart_Arguments is

  type Argument_Type is tagged private; 

  Argument_Not_Present_Error : exception;


  --
  -- Procedure/Function: Create_Argument
  --
  -- Purpose: Creates/registers an argument that may be procecessed by
  --          this package. Short_Form is intended to be the typical
  --          single character (e.g. -f) style argument format. Long
  --          form is intended to be the more descriptive format for
  --          the argument (e.g. --file). No validation is done to ensure
  --          that the long form is different or longer than the long form.
  --
  --          Required indicates if the given command line argument is
  --          required. The only purpose (currently) of this parameter
  --          is to help in creating the automatically generated help output.
  --
  --          Description is used during the generation of the help
  --          output. It is appended after displaying the arugment 
  --          (e.g.  -f/--file  - Specifies the filename to use.)
  --  
  --          Number_Required_Subargs can be set non-zero if the argument
  --          (when present) requires some specific number of sub-arguments
  --
  --          Subarg_Description is used in the short form help to describe
  --          the nature of the subargs.        
  --
  procedure Create_Argument (Argument : out Argument_Type;
    Short_Form              : in String;
    Long_Form               : in String  := "";
    Number_Required_Subargs : in Natural := 0;
    Required                : in Boolean := false;
    Description             : in String  := "";
    Subarg_Description      : in String  := "");


  --
  -- Procedure/Function: Argument_Present
  --
  -- Purpose: Returns true if there is an argument on the command
  --          line that is indicated by the given Argument.
  --

  function Argument_Present (
    Argument : in Argument_Type )
    return Boolean;


  --
  -- Procedure/Function: Argument_Image
  --
  -- Purpose: Returns the Short_Form or Long_Form string that was
  --          used to create the Argument.
  --

  function Argument_Image
    (Argument   : in Argument_Type;
     Short_Form : in Boolean := true) return String;



  --
  -- Procedure/Function: Get_Subargument
  --
  -- Purpose: Returns the command line argument value that is offset
  --          from the given argument by Subargument_Index.
  --          For example, if Argument were created with "-m" as the
  --          short form and the command line contained:
  --             -a -b -c 1.0 2.0 -m 10.0 12.0 dog -q
  --
  --          Then Get_Subargument would return 10.0 when subargument
  --          is 1, 12.0 when 2 and dog when 3. In addition, it would
  --          return -q if subargument_Index were 4 (i.e. there is no
  --          attempt made to validate that the subargument does not
  --          overlap another command line argument though programs
  --          should not be designed to make use of this fact).
  --
  --          Raises an Argument_Not_Present_Error if the given
  --          argument is not on the command line.
  --
  function Get_Subargument (
    Argument          : in Argument_Type;
    Subargument_Index : in     Positive )
    return String;


  --
  -- Procedure/Function: All_Required_Arguments_Present
  --
  -- Purpose: Returns true if all of the arguments that were created
  --          with Required set are present on the command line
  --

  function All_Required_Arguments_Present return Boolean;



  --
  -- Procedure/Function: Display_Help
  --
  -- Purpose: Writes help output to standard output based on the
  --          arguments that have been created. If Short_Form_Only
  --          is indicated then only a brief synopsis of the legal
  --          arguments is displayed.
  -- 
  --          If Short_Form_Only is not specified, then the
  --          short form help will be displayed, followed by the
  --          Pre_Argument_Command_Summary followed by the
  --          line -by- line list of arguments and their descriptions.
  --

  procedure Display_Help
    (Format_For_Output_Width     : in Positive := 79;
    Short_Form_Only              : in Boolean;
    Pre_Argument_Command_Summary : in String := "");


private

  type Argument_Type is tagged 
     record 
        Short_Form              : Ada.Strings.Unbounded.Unbounded_String;  
        Long_Form               : Ada.Strings.Unbounded.Unbounded_String;  
        Required                : Boolean;  
        Number_Required_Subargs : Natural;  
        Subarg_Description      : Ada.Strings.Unbounded.Unbounded_String;  
        Description             : Ada.Strings.Unbounded.Unbounded_String;  
     end record; 

  type Argument_List_Node_Type; 

  type Access_Argument_List_Node_Type is access Argument_List_Node_Type; 


  type Argument_List_Node_Type is new Argument_Type with 
     record 
        Next : Access_Argument_List_Node_Type;  
     end record; 

  Argument_List : Access_Argument_List_Node_Type;

end Smart_Arguments;
