Detecthiddenwindows,on
If not %0%
	Run %A_ScriptDir%\bin\AutoHotkey.exe %A_ScriptDir%\MenuZ.ahk
Controlsettext,Edit1,%1%,ahk_class AutoHotkeyGUI,__MZC
