with RASCAL.OS;                use RASCAL.OS;
with RASCAL.DragNDrop;         use RASCAL.DragNDrop;

package Controller_DataLoad is
   
   type MEL_Message_DataLoad is new AMEL_Message_DataLoad with null record;

   --
   -- The user has drag'n'dropped something on a SpriteLib window / icon.
   --
   procedure Handle (The : in MEL_Message_DataLoad);

end Controller_DataLoad;
