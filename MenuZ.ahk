#NoTrayIcon
Detecthiddenwindows,on
ControlGetText,mzc,Static1,ahk_class AutoHotkeyGUI,__MZC
If mzc <> __MZC
{
	RunWait %A_ScriptDir%\Lib\AutoHotkey.exe %A_ScriptDir%\MenuZCore.ahk
}
Controlsettext,Edit1,%1%,ahk_class AutoHotkeyGUI,__MZC
ExitApp
