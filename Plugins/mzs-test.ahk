;/=======================================================================/
; GetOpenWithList() {{{2
; 根据注册表获取当前扩展名的对应打开列表，并保存到MenuZ.auto文件中
GetOpenWithList()
{
	Global MenuZ
	ReturnObject := []
	Type := mzGetTypeString()
	If RegExMatch(Type,"i)^\.")
	{
	RegRead,exec,HKCR,%exec%\Shell\Open\Command
	m .= exec "`n"
	Loop,HKCR,%Type%\OpenWithList,1,1
	{
		RegRead,exec
		If RegExMatch(exec,"i).*\..*$")
		{
			RegRead,exec_path,HKCR,Applications\%exec%\shell\Open\Command\,
			If exec_path
				m .= exec_path "`n"
			Else
			{
				RegRead,exec_path,HKCR,Applications\%exec%\shell\Edit\Command\,
				If exec_path
					m .= exec_path "`n"
			}
		}
	}
	Loop,HKCU,Software\Classes\%Type%\OpenWithList,1,1
	{
		RegRead,exec
		If RegExMatch(exec,"i).*\..*$")
		{
			RegRead,exec_path,HKCR,Applications\%exec%\shell\Open\Command\,
			If exec_path
				m .= exec_path "`n"
			Else
			{
				RegRead,exec_path,HKCR,Applications\%exec%\shell\Edit\Command\,
				If exec_path
					m .= exec_path "`n"
			}
		}
	}
	Loop,HKCU,Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\%Type%\OpenWithList,1,1
	{
		RegRead,exec
		If RegExMatch(exec,"i).*\..*$")
		{
			RegRead,exec_path,HKCR,Applications\%exec%\shell\Open\Command\,
			If exec_path
				m .= exec_path "`n"
			Else
			{
				RegRead,exec_path,HKCR,Applications\%exec%\shell\Edit\Command\,
				If exec_path
					m .= exec_path "`n"
			}
		}
	}
	Loop,Parse,m,`n
	{
		If A_LoopField
		{
			Loop_str := RegExReplace(A_LoopField,"""")
			;msgbox % Loop_Str "`n" A_LoopField
			If FileExist(Loop_Str)
				Loop_exec := Loop_Str
			Else
				Loop,% Strlen(Loop_str)
				{
					Loop_exec := SubStr(Loop_str,1,strlen(Loop_str)-A_Index)
					If FileExist(Loop_exec)
						Break
				}
			If FileExist(Loop_exec)
				k := FileGetVersionInfo_AW(Loop_exec,"FileDescription")
			Else
				Continue
			If RegExMatch(Loop_Str,"%1")
				s := RegExReplace(Loop_Str,"%1","""{file:path}""")
			Else
				s := Loop_Str . " ""{file:path}"" "
			If s and k
			{
				ReturnObject[A_Index] := k
				DynMenuSetParam(k,"String",s)
			}
				;IniWrite,%s%,%AutoINI%,%Type%,系统打开方式(&S)\%k%
		}
	}
	Return ReturnObject
	}
}
; FileGetVersionInfo_AW() {{{2
; 获取文件名称，用于MenuZ.auto
FileGetVersionInfo_AW( peFile="", StringFileInfo="", Delimiter="|") {    ; Written by SKAN
; FileDescription | FileVersion | InternalName | LegalCopyright | OriginalFilename
; ProductName | ProductVersion | CompanyName | PrivateBuild | SpecialBuild | LegalTrademarks
; www.autohotkey.com/forum/viewtopic.php?t=64128          CD:24-Nov-2008 / LM:28-May-2010
 Static CS, HexVal, Sps="                        ", DLL="Version\"
 If ( CS = "" )
  CS := A_IsUnicode ? "W" : "A", HexVal := "msvcrt\s" (A_IsUnicode ? "w": "" ) "printf"
 If ! FSz := DllCall( DLL "GetFileVersionInfoSize" CS , Str,peFile, UInt,0 )
   Return "", DllCall( "SetLastError", UInt,1 )
 VarSetCapacity( FVI, FSz, 0 ), VarSetCapacity( Trans,8 * ( A_IsUnicode ? 2 : 1 ) )
 DllCall( DLL "GetFileVersionInfo" CS, Str,peFile, Int,0, UInt,FSz, UInt,&FVI )
 If ! DllCall( DLL "VerQueryValue" CS
    , UInt,&FVI, Str,"\VarFileInfo\Translation", UIntP,Translation, UInt,0 )
   Return "", DllCall( "SetLastError", UInt,2 )
 If ! DllCall( HexVal, Str,Trans, Str,"%08X", UInt,NumGet(Translation+0) )
   Return "", DllCall( "SetLastError", UInt,3 )
 Loop, Parse, StringFileInfo, %Delimiter%
 { subBlock := "\StringFileInfo\" SubStr(Trans,-3) SubStr(Trans,1,4) "\" A_LoopField
  If ! DllCall( DLL "VerQueryValue" CS, UInt,&FVI, Str,SubBlock, UIntP,InfoPtr, UInt,0 )
    Continue
  Value := DllCall( "MulDiv", UInt,InfoPtr, Int,1, Int,1, "Str"  )
  Info  .= Value ? ( ( InStr( StringFileInfo,Delimiter ) ? SubStr( A_LoopField Sps,1,24 )
        .  A_Tab : "" ) . Value . Delimiter ) : ""
} StringTrimRight, Info, Info, 1
Return Info
}
