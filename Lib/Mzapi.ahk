; ToMatch(str) {{{2
; 正则表达式转义
ToMatch(str){
    str := RegExReplace(str,"\+|\?|\.|\*|\{|\}|\(|\)|\||\^|\$|\[|\]|\\","\$0")
    Return RegExReplace(str,"\s","\s")
}
; ToReplace(str) {{{2
;
ToReplace(str){
    If RegExMatch(str,"\$")
        return  Regexreplace(str,"\$","$$$$")
    Else
        Return str
}
; SetMenuIcon(Name,Item,Value,Default=0){{{2

SetMenuIcon(Name,Item,Value,Default=0){
	Obj := []
    If RegExMatch(Value,"i)\{icon:[^\}]*\}",Icon)
    {
        Icon := SubStr(icon,7,strlen(icon)-7)
        Loop,Parse,Icon,|
        {

            If A_Index = 1
                IconFile := A_LoopField
            If A_Index = 2
                IconNumber :=  A_LoopField
            If A_Index = 3
                IconWidth := A_LoopField
        }
    }
    Else {
        Value := Trim(Value)  ;去掉多余的空格的制表符
        If FileExist(Value) {
            IconFile := Value
        }
        Else {
            Loop % Strlen(Value)
            {
                Exec := SubStr(Value,1,Strlen(Value)-A_Index)
                If InStr(FileExist(Exec),"D")
                    Break
                If FileExist(Exec) {
                    IconFile := Trim(Exec)
                    Break
                }
            }
        }
    }

    If Strlen(IconFile) > 0 {
        If Not RegExMatch(IconFile,"i)(\.ICO)|(\.CUR)|(\.ANI)|(\.EXE)|(\.DLL)|(\.CPL)|(\.SCR)$") {
            SplitPath, IconFile, , , Ext
            Ext := Strlen(Ext) ? "." Ext : "Folder"
            GetExtIcon(Ext,IconFile,IconNumber)
        }
        ;Menu,%Name%,Icon,%Item%,%IconFile%,%IconNumber%,%IconWidth%
    }

    If DefalutIcon
    {
        IconFile := A_ScriptDir "\ICONS\default.ico"
        ;Menu,%Name%,Icon,%Item%,%IconFile%
    }
	Obj["IconFile"] := IconFile
	Obj["IconNumber"] := IconNumber
	Obj["IconWidth"] := IconWidth
	Return Obj
}
; GetExtIcon(ext,ByRef IconFile,ByRef IconNumber) {{{1
GetExtIcon(ext,ByRef IconFile,ByRef IconNumber){

    If RegExMatch(ext,"i)^Folder$") {
        IconFile    := A_WinDir "\system32\shell32.dll"
        IconNumber  := 4
        Return
    }

    If RegExMatch(ext,"i)^Drive$") {
        IconFile    := A_WinDir "\system32\shell32.dll"
        IconNumber  := 9
        Return
    }

    If RegExMatch(ext,"i)^\.lnk$") {
        IconFile    := A_WinDir "\system32\shell32.dll"
        IconNumber  := 264
        Return
    }

    If RegExMatch(ext,"i)^\.mza$") {
        IconFile    := A_ScriptDir "\ICONS\MenuZ.ico"
        IconNumber  := 0
        Return
    }

    If RegExMatch(ext,"i)^NoExt$") {
        IconFile    := A_WinDir "\system32\shell32.dll"
        IconNumber  := 291
        Return
    }

    If RegExMatch(ext,"i)^[^\.]*$") {
        IconFile    := A_WinDir "\system32\shell32.dll"
        IconNumber  := 268
        Return
    }

    RegRead,file,HKEY_CLASSES_ROOT,%ext%
    If Strlen(file) = 0 {
        IconFile    := A_WinDir "\system32\shell32.dll"
        IconNumber  := 291
        Return
    }
    RegRead,IconString,HKEY_CLASSES_ROOT,%file%\DefaultIcon
    If ErrorLevel {
        IconFile := A_Windir "\system32\shell32.dll"
        IconNumber := 291
    }
    If RegExMatch(IconString,"%1")
    {
        RegRead,IconPath,HKCR,%file%\Shell\Open\Command
        ;IconPath := LTrim(ReplaceVar(IconPath),tm)
        IconPath := LTrim(IconPath,"""")
        If FileExist(IconPath)
            Loop_exec := IconPath
        Else
            Loop,% Strlen(IconPath)
            {
                Loop_exec := SubStr(IconPath,1,strlen(IconPath)-A_Index)
                If FileExist(Loop_exec)
                    Break
            }
        IconFile   := Loop_exec
        IconNumber := 2
        ;Return ReturnIcon(MenuName,ItemKey,IconPath,IconIndex)
    }
    Else
    {
        IconFile   := RegExReplace(IconString,",-?\d*","")
        IconNumber := RegExReplace(IconString,".*,","")
    }
    If Not RegExMatch(IconIndex,"^-?\d*$")
        IconNumber := ""
    Else
    {
        If IconNumber >= 0
            IconNumber++
    }
}
; mFileGetAttrib(file,type) {{{2
mFileGetAttrib(file,type){
    If Not FileExist(file) {
        ErrorLevel := True
        Return
    }
    If RegExMatch(type,"(`,f)|^f") {
        SplitPath, file, fn, ff, fe, fo, fd
        fd .= "\"
    }
    If RegExMatch(type,"(`,l)|^l") And RegExmatch(file,"i)\.lnk$")
        FileGetShortcut,%file%,lt,lf,la,ld,li,ln,lr
    If RegExMatch(type,"(`,a)|^a")
        FileGetAttrib, a, %file%
    If RegExMatch(type,"(`,s)|^s") {
        FileGetSize, s, %file%
        sb := s
        sk := Round((sb/1024))
        sm := Round((sk/1024))
        sg := Round((sm/1024))
        ;FileGetSize, sk, %file% ,k
        ;FileGetSize, sm, %file% ,m
    }
    If RegExMatch(type,"(`,t)|^t") {
        FileGetTime, t, %file% , m
        FormatTime , t , %t% , yyyy年MM月dd日 HH:mm:ss
        tm := t
        FileGetTime, tc, %file% , c
        FileGetTime, ta, %file% , a
    }
    If RegExMatch(type,"(`,v)|^v") {
        FileGetVersion, v, %file%
    }
    If RegExMatch(type,"(`,n)|^n") {
        If InStr(FileExist(file),"D")  {
            Loop,Parse,file
            {
                If RegExMatch(Substr(file,1-A_Index,1),"\\")
                {
                    n:= Substr(file,Strlen(file)-A_index+2)
                    Break
                }
            }
        }
        Else
            Splitpath,file,n
    }
    r := []
    Loop,Parse,Type,`,
        r[A_LoopField] := %A_LoopField%
    return r
}
