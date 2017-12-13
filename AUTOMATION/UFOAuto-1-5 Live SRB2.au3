$Vname   = "UFO Auto Analyse"
$Version = "V1.5 Live"
$VDate   = "09-May-2017 @ 18:18"
$VAuthor = "Steve Broadbent, HAG"
$Vtitle  = $Vname & " - " & $Version

; Compiler options
Opt ("WinTitleMatchMode", 2)

; Include files
#include <GUIConstantsEx.au3>
#include <MsgBoxConstants.au3>

; Declare Constants
$UFO = "UFOAnalyzerV2" ; Program name
$Rwindow = "RStudio" ; R window title
$ClipDirRoot = "D:\MeteorsIn\UFO\UFOData\"
$ddlFileRoot = "D:\R Libraries\MeteorData\DDL Analysis\"

; Check if UFO Analyser running
If not WinExists($UFO) Then
  MsgBox(0, $Vtitle, "ABORT - " & $UFO & " not running")
  Exit
EndIf

If not WinExists($Rwindow) Then
  MsgBox(0, $Vtitle, "ABORT - " & $Rwindow & " not running")
  Exit
EndIf

; Declare Arrays
Local $Months[] = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
Local $Cameras[] = ["NORTH WEST","NORTH EAST","SOUTH EAST"]
Local $ddlFiles[] = ["Reduced 2016NovNW Frequency of DDL Values used since 2014","Reduced 2016NovNE Frequency of DDL Values used since 2014","Reduced 2016NovSE Frequency of DDL Values used since 2014"]
$nCameras = Ubound($Cameras)-1
$nddlFiles = Ubound($ddlFiles) -1
$most = $nddlFiles
If $nCameras > $most Then $most = $nCameras

; Create a suitable sized GUI with various controls.
$ysize = 110 + $most * 12
Local $hGUI = GUICreate("Choose a Camera and ddl file", 400, $ysize)

; Create a combobox control.
Local $iComboBox1 = GUICtrlCreateCombo($Cameras[0], 10, 10, 185, 20)
Local $iComboBox2 = GUICtrlCreateCombo($ddlFiles[0], 200, 10, 185, 20)
Local $iClose = GUICtrlCreateButton("Done", 160, $ysize - 40, 85, 25)

; Add additional items to the combobox.
$BoxString = ""
For $i=1 to $nCameras - 1
   $BoxString = $BoxString & $Cameras[$i]&'|'
Next
$BoxString = $BoxString & $Cameras[$nCameras]
GUICtrlSetData($iComboBox1, $BoxString)

$BoxString = ""
For $i=1 to $nddlFiles - 1
   $BoxString = $BoxString & $ddlFiles[$i]&'|'
Next
$BoxString = $BoxString & $ddlFiles[$nddlFiles]
GUICtrlSetData($iComboBox2, $BoxString)

; Display the GUI.
GUISetState(@SW_SHOW, $hGUI)

; Loop until the user exits.
$Camera = $Cameras[0]
$ddlFile = $ddlFiles[0]
While 1
  Switch GUIGetMsg()
	  Case $GUI_EVENT_CLOSE, $iClose
		  MsgBox(0, "", "Selected: " & $Camera & " : " & $ddlFile, 0, $hGUI)
		  ExitLoop
	  Case $iComboBox1
		  $Camera = GUICtrlRead($iComboBox1)
	  Case $iComboBox2
		  $ddlFile = GUICtrlRead($iComboBox2)
  EndSwitch
WEnd

; Delete the previous GUI and all controls.
GUIDelete($hGUI)

; Path to clip directory
$ClipDir = $ClipDirRoot & $Camera
; Path with file name for ddl values file
$ddlFile = $ddlFileRoot & $ddlFile & ".csv"

; Check ddl values file exists
If not FileExists($ddlFile) Then
  MsgBox(0, $Vtitle, "ABORT - " & $UFO & " no ddl data file")
  Exit
EndIf

; Start Main program block

$UFOhandle = WinGetHandle($UFO)
WinActivate($UFOhandle)
$Title = WinGetTitle($UFOhandle)
; Get date from window title
$SlashPos = StringInStr($Title,"\",0,-1)
$Details = StringMid($Title,$SlashPos + 2)
;MsgBox(0, "Debug", $UFOhandle & " - " & $Details)
$Yr = StringLeft($Details,4)
$Mo = StringMid($Details,5,2)
$Dy = StringMid($Details,7,2)
$Mn = $Months[Int($Mo-1)]
;MsgBox(0, "Debug", $Yr & "-" & $Mn & "-" & $Dy)

; Initialis main ddl loop
$ExitCode = 2 ; if no clip files
$StartCount = CountFiles($ClipDir)
$Continue = $StartCount > 0
$ddlHandle = FileOpen($ddlFile,0)
$ddlLine = FileReadLine($ddlHandle)
$ddlCount = 0

While $Continue ; Analysis -  loop through ddl values
  WinActivate($UFOhandle)

  $ddlLine = FileReadLine($ddlHandle)
  If @error = -1 Then
    $ExitCode = 1 ; no more ddl values
    ExitLoop
  EndIf
  ; Extract data from line in csv file
  $ddlTemp = StringSplit($ddlLine, ",")
  $ddlValue = $ddlTemp[1]
  $ddlCount += 1
  ;MsgBox(0, "Debug", $ddlLine & " : "  & $ddlValue)

  ; Analysis block
  $StartCount = CountFiles($ClipDir,"*.avi")
  $ClipCount = ControlGetText($UFOhandle,"",1038)
  ;MsgBox("0", "Start", "ddl = " & $ddlValue & _
   ;           ", No of Clips = " & $ClipCount & _
    ;          ", No of Clip files = " & $StartCount)

  If $StartCount > 0 Then ; set ddl and check for clips

    ; Change "ddl" value, id=2226
    ControlSetText($UFOhandle,"",2226,$ddlValue)
    sleep(200)

    ; Click "read dir", id=1035
    ControlClick($UFOHandle,"",1035)
    $nClips = Int($StartCount) ; there are always 7 files per clip
	; MsgBox(0, "", "Count of Clips : "& $StartCount)
    ; Check "no. of clips value" id=1038. Read finished when nClips = ClipCount
    $Done = False
    $Atimer = TimerInit()
    While not $Done
      $ClipCount = ControlGetText($UFOhandle,"",1038)
      If $ClipCount = $nClips Then
		 $Done = true
	  Else
		 If TimerDiff($Atimer) > 30000 Then
			MsgBox(0, "", "Timeout reading clip directory (30 sec), "&" Full sets : " & $nClips & ", Clips : " & $ClipCount)
			Exit
		 EndIf
	  EndIf
    Wend
    ;MsgBox(0,"Debug","nClips : " & $nClips & "  ClipCount : " & $ClipCount)

    ;MsgBox(0, "Debug", "Analyse All")
    ; Click "analyse all", id=1036
    ControlClick($UFOhandle,"",1036)
    sleep(200)

    ; Check for Analyse to finish - Processing window does not reappear for 5000 msec
    $Done = False
    $Atimer = TimerInit()
    While not $Done
      If WinExists("UFOAnalyzerV2 processing") Then
        $Atimer = TimerInit()
      EndIf
      If TimerDiff($Atimer) > 5000 Then $Done = True
    Wend
  Else ; no clips to analyse
    $Continue = False
    $ExitCode = 2
  EndIf  ; analyse clips

  ; Prepare R command
  $Rcommand = "Move_Accepted_Clips(" & $Yr & ',"' & _
                     $Mn & '","' & _
                     $Camera & '","' & _
                     $ddlValue & '",' & _
                     '"MeteorsIn","MeteorsOut"){ENTER}'
  ;MsgBox(0, "Debug", "R command : " & $Rcommand)

  ; Run R command here
  ;command to go to R run pane
  $Rhandle = WinGetHandle($Rwindow)
  WinActivate($Rhandle)
  sleep(500)
  Send ($Rcommand)
  sleep(10000) ; increased from 2000 to 10000 to allow data time to be moved from MeteorsIn ## SRB 03Aug17 ##

Wend ; ddl values loop
FileClose($ddlHandle)

if $ExitCode = 1 Then MsgBox(0, $Vtitle, "STOP - All ddl values used : " & $ddlCount)
if $ExitCode = 2 Then MsgBox(0, $Vtitle, "STOP - No files left in " & $ClipDir)

; End Main program block
Exit

;==================================================================================

Func  CountFiles($path,$WildCard="*.*")
 Local $Count=0
;~ Count files in current folder
 $search = FileFindFirstFile($path & "\" & $WildCard)
 If $search <> -1 Then
  While 1
   $file = FileFindNextFile($search)
   If @error Then ExitLoop
   $Count+=1
  WEnd
 EndIf
 FileClose($search)
;~ Recursive all subfolders
 Local $objFSO= ObjCreate("Scripting.FileSystemObject")
 Local $objFSO_Folder  = $objFSO.GetFolder($path)
 Local $colFSO_SubFolders  = $objFSO.GetFolder($path).Subfolders
 For $objFSO_File in  $colFSO_SubFolders
  $Count +=  CountFiles($objFSO_File.Path,$WildCard)
 Next
 Return $Count
EndFunc; ---------------------------------------------------------------------------------
