#Include %A_ScriptDir%\lib\WinClipAPI.ahk
#Include %A_ScriptDir%\lib\WinClip.ahk
#Include %A_ScriptDir%\lib\PUM_API.ahk
#Include %A_ScriptDir%\lib\PUM.ahk
#Include %A_ScriptDir%\lib\Toolbar.ahk
#Include %A_ScriptDir%\lib\sci.ahk
#Include %A_ScriptDir%\lib\TT.ahk
#Include %A_ScriptDir%\lib\Struct.ahk
#Include %A_ScriptDir%\lib\sizeof.ahk
#Include %A_ScriptDir%\lib\LTVCustomColors.ahk
#Include %A_ScriptDir%\lib\Anchor.ahk
#Include %A_ScriptDir%\lib\Class_CtlColors.ahk
#Include %A_ScriptDir%\lib\Acc.ahk
#Include %A_ScriptDir%\lib\WatchDirectory.ahk
WatchDirectory(A_ScriptDir  "\Plugins\*|.ahk" , "ReportChange")
OnMessage(0x05,"IconGUI_Size")
Menu,Tray,Icon,%A_ScriptDir%\Resource\Menuz.ico
; ==============================================
; 加载配置文件
Menus     := new ini(A_ScriptDir "\MenuZ.ini")
items     := new ini(A_ScriptDir "\config\item.ini")
Languages := new ini(A_ScriptDir "\config\language.ini")
Auto      := new ini(A_ScriptDir "\config\auto.ini")
SCI       := new scintilla
win_clip  := new WinClip
win_PUM   := new PUM
win_PUM.SetParams(GetPUMParams())
Lang      := "zh-cn"
SelectType := ""
Mode := "web"
Mode_Sep := "0"
;Global iString
return

MenuDefault:
	MenuDefault()
return
MenuDefault(){
msgbox ok
}


; 结束
 /*
*/
!z::
reload
return
!`::
GoSub,Debugger
;OpenConfigGUI()
;GoSub,ItemViewer
;GoSub,StyleSelect
;iCopy(2,0)
return
;API 列表{{{1
;获取选择的类型(0 是文本 , 1 是文件 , 2 是窗口(class))
mzGetType(){
	Global IsFile,SelectType
	Return IsFile ? 1 : RegExMatch(SelectType,"^Class$") ? 2 : 0
}
;获取选择的类型描述
mzGetTypeString(){
	Global SelectType
	s := mzGetType() 
	If s = 2
		return mzGetSelect()
	return SelectType
}
mzGetSelect(){
	Global iString
	return iString
}

vim_mzcopy(t,m){
	mzSetText("abcdefg")
}

mzSetText(text){
	Global IsFile,SaveString
	Isfile := False
	SaveString := Text
}
mzSetFiles(files){
	Global IsFile,SaveString
	Isfile := True
	SaveString := Files
}

ReportChange(f,t){
	msgbox % f "`n" t 
}


iCopy(timeout=1,method=1){
	Global win_clip,IsFile,SaveString
	win_clip.Snap(data)
	win_clip.Clear()
	if( method = 1 )
      SendInput, ^{Ins}
    else
      SendInput, ^{vk43sc02E} ;ctrl+c
    ClipWait,% timeout,1
	If ErrorLevel
		return False
	IsFile := DllCall("IsClipboardFormatAvailable","int",15)
	SaveString := clipboard
	win_clip.Restore(data)
	return true
}
iGetText(){
	Global IsFile,SaveString
	If IsFile = 0
		return SaveString
}
iGetFiles(){
	Global IsFile,SaveString
	If IsFile = 1
		return SaveString
}


iCreateMenuConfig(time,method) {
	Global win_clip,iString,Menus,Items,SelectType,iClass,iControl,copyfunc
	MouseGetPos,,,WinID,iControl
	WinGetClass,iClass,ahk_id %WinID%
	If Time {
		IsFunc(iClass "_mzCopy") ? %iClass%_mzCopy(time,method) : iCopy(time,method)
	;If Time And (iCopy(time,method) ) {
		; 文本 ====================
		If (iString := iGetText()) {
			sections := Menus.GetSectionsF(SelectType := iGetTextType(iString))
			Loop,Parse,sections,`n
				If strlen(A_LoopField)
				{
					s := Menus.GetKeyValue(A_LoopField)
					isort(s)
					;sort,s
					config .= s
				}
			Config .= AddLine(s)
			If SelectType <> AnyText
			{
				s := Menus.GetKeyValue("AnyText")
				isort(s)
				;sort,s
				Config .= s 
				Config .= AddLine(s)
			}
		}
		; 文件 ====================
		Else If (iString := iGetFiles()) {
			sections :=  Menus.GetSectionsF(SelectType := iGetFileType(iString))
			Loop,Parse,sections,`n
				If strlen(A_LoopField)
				{
					s := Menus.GetKeyValue(A_LoopField)
					isort(s)
					;sort,s
					config .= s
				}
			Config .= AddLine(s)
			s := Menus.GetKeyValue("AnyFile")
			isort(s)
			;sort,s
			Config .= s 
			Config .= AddLine(s)
		}
	}
	Else
	{
		; 窗口 ====================
		SelectType := "Class"
		sections := Menus.GetSectionsF(iString := iGetClass())
		Loop,Parse,sections,`n
			If strlen(A_LoopField)
				{
					s := Menus.GetKeyValue(A_LoopField)
					isort(s)
					;sort,s
					config .= s
				}
			Config .= AddLine(s)
			s := Menus.GetKeyValue("AnyClass")
			isort(s)
			;sort,s
			Config .= s 
			Config .= AddLine(s)
	}
	s := Menus.GetKeyValue("Any")
	isort(s)
	;sort,s
	Config .= s 
	Config .= AddLine(s)
	Return Config
}

iSort(string){
	Sort,string , F NumberSort
	return String
}
NumberSort(n,m){
	a1 := RegExReplace(n,"[^\d]*=.*")
	a2 := RegExReplace(m,"[^\d]*=.*")
	return a1 > a2 ? 1 : a1 < a2 ? -1 : 0 
}
!1::
	config := iCreateMenuConfig(0.8,0)
	CreateMenuHead(config)
	CreateMenuBody(config)
	CreateMenuTail(config)
return
!2::
	config := iCreateMenuConfig(0,0)
	CreateMenuHead(config)
	CreateMenuBody(config)
	CreateMenuTail(config)
return

CreateMenuBody(string,IsSub=false,config="",type=""){
	Global win_PUM,iString,Menus,Items,SelectType,win_Menu,Mode
	If IsSub
	{
		params := GetMenuParams(Items,String)
		This_Menu := win_PUM.CreateMenu(params)
		If not strlen(config)
			Config := Menus.GetKeyValue(Items.GetValue(String,"Sub"))
				If RegExMatch(String,ToMatch(Config))
				{
					Items.IniDelete(Item,"Sub")
					Return
				}

	}
	Else
	{
		This_Menu := win_Menu
		Config := String
	}
	Loop,Parse,config,`n
	{
		If not Strlen(A_LoopField)
			continue
		ItemName := RegExReplace(Trim(A_LoopField),"^\d*\s*=\s*")
		If RegExMatch(ItemName,"^-$")
			This_Menu.Add()
		Else
		{
			If RegExMatch(Trim(A_LoopField),"^\d*\s*=\s*-"){
				ItemName := RegExReplace(Trim(A_LoopField),"^\d*\s*=\s*-")
				IsSeparator := True
			}
			Else
				IsSeparator := False
			; 菜单有效性
			If not Check_Mode(ItemName)
				continue
			If not Check_Class(ItemName)
				continue
			If not Check_Control(ItemName)
				continue
			If not Check_Name(ItemName)
				continue
			If not Check_Path(ItemName)
				continue
			If not Check_Type(ItemName)
				continue
			If not Check_MultiType(ItemName)
				continue
			If not Check_If(ItemName)
				continue
			; 菜单有效性结束
			If IsSeparator
				This_Menu.Add()
			Else
			{
				params := GetMenuItemParams(Items,ItemName)
				params["name"] := ItemName
				params["uid"] := type
				If Strlen(dyn := Items.GetValue(ItemName,"DynMenu")){
					If Items.GetValue(ItemName,"DynMenuMethod"){
						This_Item :=  This_Menu.Add(params)
						params := GetMenuParams(Items,String)
						SubMenuObj := win_PUM.CreateMenu(params)
						This_Item.SetParams({"submenu":LoadMenuFromFunc(SubMenuObj,dyn)})
					}
					Else
						LoadMenuFromFunc(This_Menu,dyn)
					continue
				}
				This_Item :=  This_Menu.Add(params)
				If ( Menus.GetKeyValue(Items.GetValue(ItemName,"Sub")) )
					This_Item.SetParams({"submenu":CreateMenuBody(ItemName,Sub:=True)})
			}
		}
	}
	Return This_Menu.handle
}
CreateMenuHead(config) {
	Global win_PUM,iString,Menus,Items,SelectType,win_Menu,Lang,languages
	ItemName := iString
	win_Menu := win_PUM.CreateMenu(GetMenuParams(Menus,"Config"))
	If SelectType = Class
		WinGet,mIconFile,ProcessPath,ahk_class %iString%
	Else If SelectType = MultiFiles
	{
		;多文件
		ExtObj := []
		iFolderCount := 0
		iFileCount   := 0
		Loop,Parse,iString,`n
		{
			If not Strlen(A_LoopField)
				continue
			Ext := iGetFileType(A_LoopField)
			If not ExtObj[Ext]
				ExtObj[Ext] := 1
			Else
				ExtObj[Ext]++
			If Ext = Folder
				iFolderCount++
			Else
				iFileCount++
		}
		win_Menu.Add({"name":"[ " languages.GetValue(Lang,"Folder") " " iFolderCount " " languages.GetValue(Lang,"unit") " , " languages.GetValue(Lang,"file") " " iFileCount " " languages.GetValue(Lang,"unit") " ]","uid":"head","icon":A_WinDir "\system32\shell32.dll:134"})
		win_Menu.Add()
		Otherinit()
		for i , k in ExtObj
		{
			If ( ss := Menus.GetSectionsF(i) )
			{
				GetExtIcon(i,mIconFile,mIconNumber)
				mIcon := mIconFile ":" mIconNumber := mIconNumber > 0 ? mIconNumber - 1 : mIconNumber
				This_Item:= win_Menu.Add({"name":"[ " i " ] " languages.GetValue(Lang,"total") " " k " " languages.GetValue(Lang,"unit"),"Icon":mIcon})
				sc := ""
				Loop,Parse,ss,`n
				{
					If not strlen(A_LoopField)
						continue
					sc .= Menus.GetKeyValue(A_LoopField)
				}
				This_Item.SetParams({"submenu":CreateMenuBody("",True,sc,i)})
			}
			Else
				otherAdd(i,k)
		}
		If IsObject(otherfiles())
		{
			This_Item := win_Menu.Add({"name":"other","icon":A_WinDir "\system32\shell32.dll:134"})
			other := win_PUM.CreateMenu(GetMenuParams(Menus,"Config"))
			This_Item.SetParams({"submenu":LoadMenuFromOther(other)})
		}
		win_Menu.Add()
		return
	}
	Else If RegExMatch(SelectType,"(^\.)|(^Folder$)|(^Drive$)|(^NoExt$)")
	{
		If RegExMatch(SelectType,"i)^(\.ICO)|(\.CUR)|(\.ANI)|(\.EXE)|(\.DLL)|(\.CPL)|(\.SCR)$"){
            mIconFile := iString
            mIconNumber := 1
        }
        Else
            GetExtIcon(SelectType,mIconFile,mIconNumber)
	}
	Else
        GetExtIcon(SelectType,mIconFile,mIconNumber)
	mIcon := mIconFile ":" mIconNumber := mIconNumber > 0 ? mIconNumber - 1 : mIconNumber
	win_Menu.Add({"name":AdjustString(ItemName,24),"uid":"head","icon":mIcon})
	win_Menu.Add()
}
LoadMenuFromOther(m){
	Global otherObj
	for i , k in  otherObj
	{
		type := Trim(RegExReplace(k,"(^.*\[)|(\].*$)"))
		;msgbox % i "`n" k
		GetExtIcon(type,mIconFile,mIconNumber)
		mIcon := mIconFile ":" mIconNumber := mIconNumber > 0 ? mIconNumber - 1 : mIconNumber
		params := []
		params["name"] := k
		params["Icon"] := mIcon
		params["uid"] := "other-" type
		m.Add(params)
	}
	return m.handle
}
Otherinit(){
	Global otherObj,otherObj_Index
	Otherobj := ""
	otherObj_Index := ""
	otherObj_type := ""
}
OtherFiles(){
	Global otherObj
	return otherObj
}
otherAdd(i,count){
	Global otherObj,otherObj_Index,languages,Lang
	If not strlen(i)
		return
	If not IsObject(otherObj)
	{
		otherObj := []
		otherObj_type := []
		otherObj_Index := 0
	}
	otherObj_Index++
	otherObj[otherObj_Index] := "[ " i " ] " languages.GetValue(lang,"total") " " count " " languages.GetValue(Lang,"unit")
}
CreateMenuTail(config) {
	Global win_PUM,iString,Menus,Items,SelectType,win_Menu
	Hotkey,IfWinActive
	Hotkey,space,MenuDefault,on
	Hotkey,tab,OpenConfigGUI,on
	;win_Menu.Add()
	win_Menu.Add({"name":"配置","icon":A_ScriptDir "\resource\settings.ico:0","uid":"config"})
	Coordmode,Mouse,Screen
	MouseGetPos,mx,my
	obj := win_Menu.Show(mx,my,"animll")
	If RegExMatch(obj.uid,"i)^config")
		GoSub,OpenConfigGUI
	Else
		Interpreter(obj,Items.GetValue(obj.name,"string"))
}
CreateMenuConfig(time,method) {
	Global win_clip,iString,Menus,Items,SelectType,iClass,iControl
	WinGetClass,iClass,A
	ControlGetFocus,iControl,A
	If Time And (win_clip.iCopy(time,method) ) {
		; 文本 ====================
		If (iString := win_clip.iGetText()) {
			sections := Menus.GetSectionsF(SelectType := iGetTextType(iString))
			Loop,Parse,sections,`n
				If strlen(A_LoopField)
				{
					s := Menus.GetKeyValue(A_LoopField)
					sort,s
					config .= s
				}
			Config .= AddLine(s)
			If SelectType <> AnyText
			{
				s := Menus.GetKeyValue("AnyText")
				sort,s
				Config .= s 
				Config .= AddLine(s)
			}
		}
		; 文件 ====================
		Else If (iString := win_clip.iGetFiles()) {
			sections :=  Menus.GetSectionsF(SelectType := iGetFileType(iString))
			Loop,Parse,sections,`n
				If strlen(A_LoopField)
				{
					s := Menus.GetKeyValue(A_LoopField)
					sort,s
					config .= s
				}
			Config .= AddLine(s)
			s := Menus.GetKeyValue("AnyFile")
			sort,s
			Config .= s 
			Config .= AddLine(s)
		}
	}
	Else
	{
		; 窗口 ====================
		SelectType := "Class"
		sections := Menus.GetSectionsF(iString := iGetClass())
		Loop,Parse,sections,`n
			If strlen(A_LoopField)
				{
					s := Menus.GetKeyValue(A_LoopField)
					sort,s
					config .= s
				}
			Config .= AddLine(s)
			s := Menus.GetKeyValue("AnyClass")
			sort,s
			Config .= s 
			Config .= AddLine(s)
	}
	s := Menus.GetKeyValue("Any")
	sort,s
	Config .= s 
	Config .= AddLine(s)
	Return Config
}
AddLine(s) {
	If strlen(s)
		return "-`n"
}
GetMenuItemParams(Conf,Section){
	Global Menus
	params := []
	If Strlen(Tcolor := Conf.GetValue(Section,"Tcolor"))	
		params["tcolor"] := Tcolor 
	If Strlen(BGcolor := Conf.GetValue(Section,"BGcolor"))
		params["bgcolor"] :=  BGcolor
	If Strlen(bold := Conf.GetValue(Section,"bold"))
		params["bold"] := bold
	If Strlen(icon := Menus.ReplaceEnv(Conf.GetValue(Section,"icon")))
		params["icon"] :=  icon 
	If Strlen(break := Conf.GetValue(Section,"break")) 
		params["break"] := break
	return params
}
LoadMenuFromFunc(m,f){
	Global Items
	If not IsFunc(f)
		return
	for i , k in  %f%()
	{
		params := GetMenuItemParams(Items,k)
		params["name"] := k
		m.Add(params)
	}
	return m.handle
}
DynItemName(String){
    P1 := 1
    Loop
    {
        RString  := ""
        P2 := RegExMatch(String,"i)\{[^\{\}]*\}",Switch,P1)
        If P2
        {
;            RString := SaveSelect
            Loop
            {
                If RegExMatch(Switch,"i)^\{file:[^\{\}]*\}$") {
                    m := mFileGetAttrib(SaveSelect,"fn,n,s,tm")
                    mfile := New mfile(SaveSelect)
                    Attrib := SubStr(Switch,7,Strlen(Switch)-7)
                    If Attrib = name
                        RString := m["n"]
                    If Attrib = size
                        RString := m["s"]
                    If Attrib = mTime
                        RString := m["tm"]
                    Break
                }
                Else
                {
                    RString := Switch
                    Break
                }
            }
        }
        Else {
            If P1 > 1
                R .= Over
            Else
                R := String
            Break
        }

        Inter := Substr(String,P1,P2-P1)
        P1 := P2 + Strlen(Switch)
        Over  := Substr(String,P1)
        r .= Inter RString
    }
    return r
}
DynMenuSetParam(section,key,value){
	Global Items
	Items.Content[section "`n" key ] := value
}
/* ==============
 * 动态菜函数实例
 */
dynfunc(){
	obj := []
	obj[1] := "aaaaaa"
	obj[2] := "bbbbbb"
	obj[3] := "cccccc"
	DynMenuSetParam(obj[1],"string","hahahaha")
	DynMenuSetParam(obj[1],"BGcolor",121212)
	return obj
}
; 菜单有效性
Check_Mode(i) {
	Global Menus,Items,Mode,Mode_Sep
	ThisModes := Menus.ReplaceEnv(Items.GetValue(i,"Mode"))
	If Strlen(ThisModes) 
	{
		Loop,Parse,ThisModes,|
		{
			If A_LoopField = %Mode%
				return True
		}
		return False
	}
	Else
		Return Mode_Sep ? True : "Null"
}
Check_Class(i){
	Global Menus,Items,iClass
	method := Items.GetValue(i,"Classmethod")
	string := Menus.ReplaceEnv(Items.GetValue(i,"Class"))
	If not strlen(string)
		return True
	; method >> 0 或空时，为include，1是为exclude ，2是regexp
	If method = 2
		Return RegExMatch(iClass,string)
	Else
	{
		Loop,Parse,String,|
		{
			If A_LoopField = %iClass%
				return not method
		}
		return method
	}
}
Check_Control(i){
	Global Menus,Items,iControl
	method := Items.GetValue(i,"Controlmethod")
	string := Menus.ReplaceEnv(Items.GetValue(i,"Control"))
	;msgbox % method "`n" string
	If not strlen(string)
		return True
	; method >> 0 或空时，为include，1是为exclude ，2是regexp
	If method = 2
		Return RegExMatch(iControl,string)
	Else
	{
		Loop,Parse,String,|
		{
			If A_LoopField = %iControl%
				return not method
		}
		return method
	}
}
Check_Type(i){
	Global Menus,Items,SelectType
	method := Items.GetValue(i,"Typemethod")
	string := Menus.ReplaceEnv(Items.GetValue(i,"Type"))
	;msgbox % method "`n" string
	If not strlen(string)
		return True
	; method >> 0 或空时，为include，1是为exclude ，2是regexp
	If method = 2
		Return RegExMatch(SelectType,string)
	Else
	{
		Loop,Parse,String,|
		{
			If A_LoopField = %SelectType%
				return not method
		}
		return method
	}
}
Check_MultiType(i){
	Global Menus,Items,SelectType,iString
	If SelectType = MultiFiles
	{
		method := Items.GetValue(i,"MultiTypemethod")
		string := Menus.ReplaceEnv(Items.GetValue(i,"MultiType"))
		If not strlen(string)
			return True
		If not method
		{
			Loop,Parse,String,|
				iMatch .= "(" ToMatch(A_LoopField) ")|"
			iMatch := "i)" SubStr(iMatch,1,Strlen(iMatch)-1)
		}
		; method >> 0 或空时，为include，1是为exclude ，2是regexp
		Loop,Parse,iString,`n
		{
			If not Strlen(A_LoopField)
				continue
			iType := iGetFileType(A_LoopField)
			If not method And (not RegExMatch(iType,iMatch))
				return False
			If (method = 2) And not RegExMatch(iType,string)
				return False
			If RegExMatch(iType,SubStr(iMatch,1,Strlen(iMatch)-1)) And Strlen(iMatch)
				continue
			Else
			{
				m := ToMatch(iType)
				iMatch .= "(^" m "\|)|(\|" m "\|)|(\|" m "$)|(^" m "$)|"
			}
		}
		iMatch := "i)" SubStr(iMatch,1,Strlen(iMatch)-1)
		If method = 2 or method = 0
			return True
		Loop,Parse,String,|
		{
			If method ; exclude
			{
				If RegExMatch(A_LoopField,iMatch)
					return False
			}
		}
		return True
	}
	Else
		Return True
}
Check_Path(i) {
	Global Menus,Items,SelectType,iString
	method := Items.GetValue(i,"FilePathMethod")
	string := Menus.ReplaceEnv(Items.GetValue(i,"FilePath"))
	If ( not strlen(string))  or ( not RegExMatch(SelectType,"i)^(\..*)|(Multifiles)|(Folder)|(Drive)|(NoExt)$"))
		return True
	If SelectType = MultiFiles
	{
		Loop,Parse,iString,`n
		{
			aFile := A_LoopField
			break
		}
	}
	Else
		aFile := iString
	SplitPath,aFile,,dir
	dir .= "\"
	; method >> 0 或空时，为include，1是为exclude ，2是regexp
	If method = 2
		Return RegExMatch(dir,string)
	Else
	{
		Loop,Parse,String,|
		{
			If A_LoopField = %dir%
				return not method
		}
		return method
	}
}
Check_Name(i) {
	Global Menus,Items,SelectType,iString
	method := Items.GetValue(i,"FileNamemethod")
	string := Menus.ReplaceEnv(Items.GetValue(i,"FileName"))
	If not strlen(string) or RegExMatch(SelectType,"^\.")
		return True
	If SelectType = MultiFiles
	{
		If method = 2
		{
			Loop,Parse,String,|
					iMatch .= "(" ToMatch(A_LoopField) ")|"
			iMatch := "i)" SubStr(iMatch,1,Strlen(iMatch)-1)
		}
		Loop,Parse,iString,`n
		{
			If not Strlen(A_LoopField)
				continue
			SplitPath,A_LoopField,Name
			; method >> 0 或空时，为include，1是为exclude ，2是regexp
			If method = 2 
			{
				If RegExMatch(Name,string)
					continue
				Else
					return False
			}
			If method = 1
			{
				If RegExMatch(Name,iMatch)
					return False
				Else
					continue
			}
			If method = 0
			{
				If RegExMatch(Name,iMatch)
					continue
				Else
					return False
			}
		}
		return True
	}
	Else
	{
		SplitPath,iString,Name
		; method >> 0 或空时，为include，1是为exclude ，2是regexp
		If method = 2
			Return RegExMatch(Name,string)
		Else
		{
			Loop,Parse,String,|
			{
				If A_LoopField = %Name%
					return not method
			}
			return method
		}
	}
}

Check_If(i){
	Global Items
	IfFunc := Menus.ReplaceEnv(Items.GetValue(i,"if"))
	If strlen(IfFunc)
		return %IfFunc%()
	return True
}



; 获取菜单参数
GetMenuParams(Conf,Section){
	params := []
	If Strlen(Tcolor := Conf.GetValue(Section,"Tcolor"))	
		params["tcolor"] := Tcolor 
	If Strlen(BGcolor := Conf.GetValue(Section,"BGcolor"))
		params["bgcolor"] :=  BGcolor
	If Strlen(nocolors := Conf.GetValue(Section,"nocolors"))
		params["nocolors"] := nocolors
	If Strlen(noicons := Conf.GetValue(Section,"noicons"))
		params["noicons"] := noicons
	If Strlen(notext := Conf.GetValue(Section,"notext"))
		params["notext"] :=  notext
	If Strlen(IconsSize := Conf.GetValue(Section,"IconsSize"))
		params["IconsSize"] :=  IconsSize
	Else
		params["IconsSize"] :=  16
	If Strlen(textoffset := Conf.GetValue(Section,"textoffset")) 
		params["textoffset"] := textoffset
	If Strlen(maxheight := Conf.GetValue(Section,"maxheight"))
		params["maxheight"] := maxheight
	If Strlen(xmargin := Conf.GetValue(Section,"xmargin"))
		params["xmargin"] := xmargin
	If Strlen(ymargin := Conf.GetValue(Section,"ymargin"))
		params["ymargin"] := ymargin
	If Strlen(textMargin:= Conf.GetValue(Section,"textMargin")) 
		params["textMargin"] := textMargin
	return params
}
GetPUMParams(){
	Global Menus
	Params := { "SelMethod" : "fill"            
					,"oninit"      : "PUM_out"      
					,"onuninit"    : "PUM_out"     
					,"onselect"    : "PUM_out"     
					,"onrbutton"   : "PUM_out"   
					,"onmbutton"   : "PUM_out"  
					,"onrun"       : "PUM_out"      
					,"onshow"      : "PUM_out"      
					,"onclose"     : "Pum_out"}
	If Strlen(SelTcolo := Menus.GetValue("config","SelTcolor"))	
		params["Seltcolor"] := SelTcolor 
	If Strlen(SelBGcolor := Menus.GetValue("config","SelBGcolor"))
		params["Selbgcolor"] := SelBGcolor
	If Strlen(SelMethod := Menus.GetValue("config","SelMethod"))	
		params["SelMethod"] := SelMethod
	If Strlen(frameWidth := Menus.GetValue("config","frameWidth"))
		params["frameWidth"] := frameWidth
	return Params
}
iGetClass(){
	WinGetClass,c,A
	return c
}
iGetTextType(text){
	Global Menus
	regexps := Menus.GetKeys("TextType")
	Loop,Parse,regexps,`n
	{
		If not Strlen(A_LoopField)
			continue
		If RegExMatch(Text,Menus.GetValue("TextType",A_LoopField))
			return A_LoopField
	}
	Return "AnyText"
}
iGetFileType(file){
	If InStr(file,"`n") ;多文件
		Return "MultiFiles"
	Else
	{
		If RegExMatch(file,"[a-zA-Z]:\\$")
			Return "Drive"
		Else
		{
			Attrib := FileExist(file)
			If InStr(Attrib,"D")
				Return "Folder"
			Else
			{
				SplitPath,file,,,ext
				If strlen(ext)
					Return "." ext
				Else
					Return "NoExt"
			}
		}
	}
}
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
PUM_out(msg,obj) {
	Global iString,Items
	if msg = onselect
  	{
		;rect := obj.GetRECT()
		;tooltip,% "Selected: " obj.name,% rect.right,% rect.top
		tooltip
  	}
	If msg = onclose
	{
		Hotkey,IfWinActive
		Hotkey,space,MenuDefault,off
		Hotkey,tab,OpenConfigGUI,off
	}
	If msg = onshow
	{
		;tooltip % A_TimeSinceThisHotkey,10,10,10
	}
	If msg = onrbutton
	{
		;rect := obj.GetRECT()
		If RegExmatch(obj.uid,"i)^head")
		{
			MouseGetPos,mx,my
			Tooltip,% iString, % mx, % my
		}
	}
}
; 计时器
TickOn(){
	Global Tick
	Tick := A_TickCount
}
TickOff(){
	Global Tick
	Tick := A_TickCount - Tick
}
TickShow(){
	Global Tick
	msgbox % Tick
}

; 限制文本长度为Count,不够的话，补充空格
; AdjustString(String,Count) {{{2
AdjustString(String,Count){
    String := Trim(String)
    p := Count - Strlen(String)
    If p > 0
    {
        Loop,%p%
            String .= A_Space
    }
    Else
        String := SubStr(String,1,Count/2) "..." Substr(String,-(count/2)+1)
    Return String
}
Interpreter(obj,string){
	Global Items,iString,Menus
	RunString := Menus.ReplaceEnv(Trim(RegExReplace(String, ".*\n")))
	If obj.uid
	{
		; 修改选择的内容
	}
	P1 := 1
	Loop
	{
		Pos := RegExMatch(RunString,"\{[^\{\}]*\}",switch,P1)
		If Pos
		{
			RtString := switch
			If RegExMatch(switch,"i)\{file[^\{\}]*\}") And RegExMatch(iGetFileType(iString),"i)^(\..*)|(Multifiles)|(Folder)$")
				RtString := fileswitch(iString,switch)
			If RegExMatch(switch,"i)\{select[^\{\}]*\}")
				RtString := selectswitch(SaveSelect,switch)
			If RegExMatch(switch,"i)\{box[^\{\}]*\}",box)
				RtString := boxswitch(box)
			If RegExMatch(switch,"i)\{date[^\{\}]*\}",date)
				RtString := dateswitch(date)
			If RegExMatch(switch,"i)\{func:([^\{\}]*)\}",Func) And Isfunc(Func1)
				RtString := %func1%()
			/*
			{
				If Isfunc(Func1)
					RtString := %Func1%()
					msgbox % RtString
			;	Else
			;		RtString := CheckExtension(func)
			}
			*/



			P1 := Pos + Strlen(switch)
			RunString := SubStr(RunString,1,Pos-1) RtString Substr(RunString,P1)
			P1 := Pos + Strlen(RtString)
		}
		Else
			Break
			
	}
	;Msgbox % RunString
	Run, % RunString
}
; dateswitch(switch) {{{3
dateswitch(switch)
{
	If RegExMatch(switch,"\[(.*?)(?<!\\)\]",now)
	{
		If RegExMatch(now1,"^\w*$")
			now := now1
		Else
		{
			now := A_now
			If RegExMatch(now1,"i)d\s*\+\s*(\d+)",d)
				now += d1 , d
			If RegExMatch(now1,"i)m\s*\+\s*(\d+)",m)
				now += d1 , m
			If RegExMatch(now1,"i)s\s*\+\s*(\d+)",s)
				now += d1 , s
		}
	}
	Else
		now := A_Now
	If RegExMatch(switch,"i)^\{date\}$")
		FormatTime, time , %now% ,yyyyMMdd
	Else
	{
		format := RegExReplace(RegExReplace(switch,"i)(^\{date:)|(\}$)"),"\[(.*?)(?<!\\)\]")
		If RegExMatch(format,"\w*")
			FormatTime, time , %now% ,%format%
	}
	;If RegExMatch(switch,"
	return time
}

; boxswitch(switch) {{{3
; {box} 输入内容
boxswitch(switch)
{
	Global Languages,Lang
	GUI,boxswitch:Destroy
	GUI,boxswitch:Font ,s9 ,Microsoft YaHei
	GUI,boxswitch:Default
	GUI,boxswitch:+hwndboxhandle
	Exist := 0
	
	; {box:input} {{{4
	If RegExMatch(switch,"i)^\{box:input")
	{
		If RegExMatch(switch,"\[>(.*?)(?<!\\)\]",tips)
		{
			tips := RegExReplace(RegExReplace(tips1,"\\\]","]"),"i)\\n","`n")
			GUI,boxswitch:Add,Edit,x10 y10 w400 h60 ReadOnly, %Tips%
		}
		Exist++
		If RegExMatch(switch,"\[\*\]")
			opt := "Password"
		GUI,boxswitch:Add,Edit, x10 w400 h24 R1 %opt%
		GUIControl, Focus , Edit2
		GUI,boxswitch:Add,Button,x120 w120 h26 default gGUI_boxswitch_EditOK,% Languages.GetValue(Lang,"Button OK")
		GUI,boxswitch:Add,Button,x274 yp w120 h26 gGUI_boxswitch_Cancel,% Languages.GetValue(Lang,"Button Cancel")
		title := Languages.GetValue(Lang,"Box input")
	}
	; {box:list} {{{4
	If RegExMatch(switch,"i)^\{box:list")
	{
		If RegExMatch(switch,"\[>(.*?)(?<!\\)\]",tips)
		{
			tips := RegExReplace(RegExReplace(tips1,"\\\]","]"),"i)\\n","`n")
			GUI,boxswitch:Add,Edit,x10 y10 w300 h80 ReadOnly, %Tips%
		}
		Exist++
		GUI,boxswitch:Add,Listview,x10 w300 h200 gGUI_boxswitch_ListViewOK ,% Languages.GetValue(Lang,"Box list header")
		P1 := 1
		Loop
		{
			Pos := RegExMatch(switch,"\[[^>](.*?)(?<!\\)\]",opt,P1)
			If Pos
			{
				P1 := Pos + strlen(opt)
				LV_Add("",A_Index,RegExReplace(opt,"(^\[)|(\]$)"))
			}
			Else
				Break
		}
		GUI,boxswitch:Add,Button,x20 w120 h26 default gGUI_boxswitch_ListViewOK,% Languages.GetValue(Lang,"Button OK")
		GUI,boxswitch:Add,Button,x174 yp w120 h26 gGUI_boxswitch_Cancel,% Languages.GetValue(Lang,"Button Cancel")
		title :=  Languages.GetValue(Lang,"Box list")
	}
	If Exist
	{
		Temp := ""
		GUI,boxswitch:Show,xCenter yCenter,% title
		WinWaitClose,ahk_id %boxhandle%
		return Temp 
	}
	; {box:file} {{{4
	If RegExMatch(switch,"i)^\{box:file")
	{
		If RegExMatch(switch,"\[>(.*?)(?<!\\)\]",tips)
			tips := RegExReplace(RegExReplace(tips1,"\\\]","]"),"i)\\n","`n")
		Else
			tips := Languages.GetValue(Lang,"Box file")
		;opt := ""
		If RegExMatch(switch,"i)\[m\]")
			opt := "M35"
		Else If RegExMatch(switch,"i)\[s\]")
			opt := "S24"
		If RegExMatch(switch,"\[&(.*?)\*(?<!\\)\]",rootdir)
			rootdir := RegExReplace(rootdir1,"\\\]","]")
		Else
			SplitPath,SaveSelect,,rootdir
		If RegExMatch(switch,"\[#(.*?)(?<!\\)\]",filter)
			filter := RegExReplace(RegExReplace(filter1,"\\\]","]"),"i)\\n","`n")
		Else
			filter := ""
		FileSelectFile,Temp,%opt%,%rootdir%,%tips%,%filter%
		If opt = M35
		{
			Loop,Parse,Temp,`n
			{
				If not strlen(A_LoopField) 
					continue
				If A_Index = 1
					dir := A_LoopField
				Else
					newTemp .= dir "\" A_LoopField "`n"
			}
			If RegExMatch(switch,"i)\[(file:.*)\]",file)
			{
				file := "{" file1 "}"
				Temp := fileswitch(newTemp,file)
			}
			Else
				Temp := newTemp
		}
		return Temp
	}
	; {box:dir} {{{4
	If RegExMatch(switch,"i)^\{box:dir")
	{
		If RegExMatch(switch,"\[>(.*?)(?<!\\)\]",tips)
			tips := RegExReplace(RegExReplace(tips1,"\\\]","]"),"i)\\n","`n")
		Else
			tips := Languages.GetValue(Lang,"Box Dir")
		If RegExMatch(switch,"\[&(.*?)\*(?<!\\)\]",rootdir)
			rootdir := RegExReplace(rootdir1,"\\\]","]")
		Else
			SplitPath,SaveSelect,,rootdir
		FileSelectFolder, Temp , %rootdir%, 3, %tips%
	}
	GUI_boxswitch_EditOK:
		GuiControlGet, vis, Visible,Edit2
		If vis
			GUIControlGet,Temp,,Edit2
		Else
			GUIControlGet,Temp,,Edit1
		GUI,boxswitch:Destroy
	return
	GUI_boxswitch_ListViewOK:
		If A_GuiEvent = DoubleClick
		{
			If ( idx := LV_GetNext())
				LV_GetText(Temp,idx,2)
			GUI,boxswitch:Destroy
		}
	return
	GUI_boxswitch_Cancel:
		GUI,boxswitch:Destroy
	return
}


; {select} 选择内容
selectswitch(select,switch)
{
	Global iString
	RtString := iString
	If RegExMatch(switch,"\[@(.*?)(?<!\\)\]",RegExSelect)
	{
		RegExSelect:= RegExReplace(RegExSelect1,"\\\]","]")
		RegExMatch(select,RegExSelect,RtString)
	}
	If RegExMatch(switch,"\[(?<!@)(.*?)(?<!\\)\]",RegExEncode)
	{
		Encode := RegExReplace(RegExEncode1,"\\\]","]")
		RtString := SksSub_UrlEncode(RtString,Encode)
	}
	Return RtString
}

; SksSub_UrlEncode(string, enc="UTF-8") {{{2
; 来自万年书妖的Candy里的函数，用于转换编码。感谢！
SksSub_UrlEncode(string, enc="UTF-8")
{   ;url编码
    enc:=trim(enc)
    If enc=
        Return string
	If Strlen(String) > 200
		string := Substr(string,1,200)
    formatInteger := A_FormatInteger
    SetFormat, IntegerFast, H
    VarSetCapacity(buff, StrPut(string, enc))
    Loop % StrPut(string, &buff, enc) - 1
    {
        byte := NumGet(buff, A_Index-1, "UChar")
        encoded .= byte > 127 or byte <33 ? "%" Substr(byte, 3) : Chr(byte)
    }
    SetFormat, IntegerFast, %formatInteger%
    return encoded
}


fileSwitch(FileList,switch){
    If Strlen(switch) And RegExMatch(switch,"^\{.*\}$")
        Temp := switch
    Else
        Return switch
    If RegExMatch(switch,"i)^\{file\}$",m)
    OR RegExMatch(switch,"i)^\{file:((path)|(name)|(dir)|(ext)|(namenoext)|(drive)|(ver))\}$",m)
    OR RegExMatch(switch,"i)^\{file:size(\[[KMG]B\])?\}",m)
    OR RegExMatch(switch,"i)^\{file:time(\[[MAC]\])?\}",m)
;    OR RegExMatch(switch,"i)^\{file:content(\[((uft-(8|16)(-raw)?)|(cp\d{1,5}))\])?\}",m) {
    OR RegExMatch(switch,"i)^\{file:content(\[((cp\d{1,5})|(utf-(8|16))(-raw)?)\])?\}",m) {
        Loop,Parse,FileList,`n,`r
        {
            m := mFileGetAttrib(A_LoopField,"fn,fd,fe,fo,ff,v")
            If RegExMatch(switch,"i)^\{file\}$") Or RegExMatch(switch,"i)^\{file:path\}$"){
                nSwitch := A_LoopField
                break
            }
            If RegExMatch(switch,"i)^\{file:name\}$") {
                nSwitch := m["fn"]
                break
            }
            If RegExMatch(switch,"i)^\{file:dir\}$") {
                nSwitch := m["ff"]
                break
            }
            If RegExMatch(switch,"i)^\{file:ext\}$") {
                nSwitch := m["fe"]
                break
            }
            If RegExMatch(switch,"i)^\{file:namenoext\}$") {
                nSwitch := m["fo"]
                break
            }
            If RegExMatch(switch,"i)^\{file:drive\}$") {
                nSwitch := m["fd"]
                break
            }
            If RegExMatch(switch,"i)^\{file:ver\}$") {
                nSwitch := m["v"]
                break
            }
            If RegExMatch(switch,"i)^\{file:size(\[[kmg]b\])?\}$",m) {
                Units := SubStr(m,12,strlen(m)-14)
                m := mFileGetAttrib(A_LoopField,"sb,sk,sm,sg")
                If Strlen(Units) = 0
                    nSwitch := m["sb"]
                Else If InStr(Units,"k")
                    nSwitch := m["sk"]
                Else If InStr(Units,"m")
                    nSwitch := m["sm"]
                Else If InStr(Units,"g")
                    nSwitch := m["sg"]
                Else
                    nSwitch := m["sb"]
                break
            }
            If RegExMatch(switch,"i)^\{file:time(\[[mac]\])?\}$",m) {
                WhichTime := SubStr(m,12,strlen(m)-13)
                If Instr("mac",WhichTime) {
                    FileGetTime,nSwitch,%A_LoopField%,%WhichTime%
                    Break
                }
                FileGetTime,nSwitch,%A_LoopField%
                break
            }
            If RegExMatch(switch,"i)^\{file:content(\[[^\[\]]*\])?\}$",m) {
                If InStr(FileExist(A_LoopField),"D")
                    nSwitch := ""
                Else {
                    Encode := SubStr(m,15,strlen(m)-16)
                    SaveEncode := A_FileEncoding
                    FileEncoding, %Encode%
                    FileRead,nSwitch,%A_LoopField%
                    FileEncoding, %SaveEncode%
                }
                Break
            }
            Break
        }
        Return nSwitch
    }
    Else {
        co_Regex := ""
        co_Index := ""
        co_Equal := ""
        co_Unequ := ""
        co_Folder := ""
        co_File  := ""
        co_Char  := 0

        If RegExMatch(switch,"\[@.*?(?<!\\)\]",co_Regex) {
            Temp := RegexReplace(Temp,ToMatch(co_Regex))
            co_Regex := EscapeSwitch(SubStr(co_Regex,3,Strlen(co_Regex)-3))
        }
        If RegExMatch(switch,"\[<\d*\]",co_Char) {
            Temp := RegexReplace(Temp,ToMatch(co_Char))
            co_Char := Substr(co_Char,3,Strlen(co_Char)-3)
        }
        If RegExMatch(switch,"\[%[\d,-]*\]",idx) {
            ; [1,2-4,5,6,7]
            Temp := RegexReplace(Temp,ToMatch(idx))
            co_Index := ","
            idx := EscapeSwitch(SubStr(idx ,3,Strlen(idx)-3))
            If Instr(idx,",") OR InStr(idx,"-") {
                Loop,Parse,Idx,`,
                {
                    If RegExMatch(A_LoopField,"\d*-\d*",lidx)
                    {
                        N1 := Substr(lidx,1,Instr(A_LoopField,"-")-1)
                        N2 := SubStr(lidx, InStr(A_LoopField,"-")+1)
                        co_Index .= N1 ","
                        Loop % ( N2 - N1 )
                            co_Index .= (A_Index + N1 ) ","
                    }
                    Else
                        co_Index .= A_LoopField ","
                }
            }
            Else
                co_Index .= Idx ","
        }
        If RegExMatch(switch,"\[!.*?(?<!\\)\]",Ex) {
            Temp := RegexReplace(Temp,ToMatch(Ex))
            Ex := EscapeSwitch(SubStr(Ex,3,Strlen(Ex)-3))
            If Instr(Ex,"|") {
                Loop,Parse,Ex,|
                    co_Unequ .= "(" RegExReplace(RegExReplace(A_LoopField,"\s"),"\+|\?|\.|\*|\{|\}|\(|\)|\||\[|\]|\\","\$0") ")|"
                co_Unequ := "i)" SubStr(co_Unequ,1,Strlen(co_Unequ)-1)
            }
            Else
                co_Unequ := "i)" Ex
        }
        If RegExMatch(switch,"\[=.*?(?<!\\)\]",Ix) {
            Temp := RegexReplace(Temp,ToMatch(Ix))
            Ix := EscapeSwitch(SubStr(Ix,3,Strlen(Ix)-3))
            If InStr(Ix,"|") {
                Loop,Parse,Ix,|
                    co_Equal .= "(" RegExReplace(RegExReplace(A_LoopField,"\s"),"\+|\?|\.|\*|\{|\}|\(|\)|\||\[|\]|\\","\$0") ")|"
                co_Equal := "i)" SubStr(co_Equal,1,Strlen(co_Equal)-1)
            }
            Else
                co_Equal := "i)" Ix
        }
        If RegExMatch(switch,"i)\[OF\]") {
            Temp := RegexReplace(Temp,"i)\[OF\]")
            co_File := True
        }
        If RegExMatch(switch,"i)\[OD\]") {
            Temp := RegexReplace(Temp,"i)\[OD\]")
            co_Folder := True
        }
        LoopListCount := 0
        Loop,Parse,FileList,`n,`r
        {
            AddLine := True
            m := mFileGetAttrib(A_LoopField,"n")
            If co_Regex And (Not RegExMatch(A_LoopField,co_Regex) )
                AddLine := False
            If co_Index And (Not InStr(co_Index,"," A_Index ",") )
                AddLine := False
            If co_Equal And (Not RegExMatch(m["n"],co_Equal) )
                AddLine := False
            If co_Unequ And RegExMatch(m["n"],co_Unequ)
                AddLine := False
            If co_File  And InStr(FileExist(A_LoopField),"D")
                AddLine := False
            If co_Folder And (Not InStr(FileExist(A_LoopField),"D"))
                AddLine := False
            If AddLine {
                LoopList .= A_LoopField "`n"
                LoopListCount++
            }
        }
;        Msgbox % LoopList
        Temp := SubStr(Temp,7,Strlen(Temp)-7)
        Loop,Parse,LoopList,`n
        {
            If Strlen(A_LoopField) = 0
                Continue
            LoopListIndex := A_Index
            m := mFileGetAttrib(A_LoopField,"fn,ff,fe,fo,fd")
            r := ""
            P1 := 1
            Loop
            {
                P2 := RegExMatch(Temp,"\[((P)|(F)|(N)|(n)|(E)|(e)|(CR)|(TAB)|(I)|(II)|(C)|(D)|(d)|(M)|(m)|(Y)|(h)|(s)|(t)|(#.*?(?<!\\)))\]",s,P1)
                If P2
                {
                    Loop
                    {
                        If RegExMatch(s,"\[P\]") {
                            RString := A_LoopField
                            Break
                        }
                        If RegExMatch(s,"\[F\]") {
                            RString := m["ff"]
                            Break
                        }
                        If RegExMatch(s,"\[N\]") {
                            RString := m["fn"]
                            Break
                        }
                        If RegExMatch(s,"\[E\]") {
                            RString := m["fe"]
                            Break
                        }
                        If RegExMatch(s,"\[n\]") {
                            RString := m["fo"]
                            Break
                        }
                        If RegExMatch(s,"\[e\]") {
                            RString := m["fd"]
                            Break
                        }
                        If RegExMatch(s,"\[CR\]") {
                            RString := "`r`n"
                            Break
                        }
                        If RegExMatch(s,"\[Tab\]") {
                            RString := A_Tab
                            Break
                        }
                        If RegExMatch(s,"\[I\]") {
                            RString := LoopListIndex
                            Break
                        }
                        If RegExMatch(s,"\[II\]") {
                            Index := ""
                            If Strlen(LoopListIndex) < Strlen(LoopListCount) {
                                Loop, % strlen(LoopListCount) - strlen(LoopListIndex)
                                    Index .= "0"
                                Index .= LoopListIndex
                            }
                            Else
                                Index := LoopListIndex
                            RString := Index
                            Break
                        }
                        If RegExMatch(s,"\[C\]") {
                            RString := LoopListCount
                            Break
                        }

                        If RegExMatch(s,"\[d\]") {
                            RString := A_YYYY "-" A_MM "-" A_DD
                            Break
                        }

                        If RegExMatch(s,"\[t\]") {
                            RString := A_Hour A_Min A_Sec
                            Break
                        }

                        If RegExMatch(s,"\[Y\]") {
                            RString := A_YYYY
                            Break
                        }
                        If RegExMatch(s,"\[M\]") {
                            RString := A_MM
                            Break
                        }
                        If RegExMatch(s,"\[D\]") {
                            RString := A_DD
                            Break
                        }
                        If RegExMatch(s,"\[h\]") {
                            RString := A_Hour
                            Break
                        }
                        If RegExMatch(s,"\[m\]") {
                            RString := A_Min
                            Break
                        }
                        If RegExMatch(s,"\[s\]") {
                            RString := A_Sec
                            Break
                        }
                        If RegExMatch(s,"\[#.*?(?<!\\)\]",Exten) {
                            Exten := SubStr(Exten,3,Strlen(Exten)-3)
                            If Isfunc(Exten)
                                RString := %Exten%()
                            Else
                                RString := ""
                            Break
                        }

                    }
                }
                Else {
                    If P1 > 1
                        r .= Over
                    Else
                        r := Temp
                    Break
                }
                Inter := Substr(Temp,P1,P2-P1)
                P1 := P2 + Strlen(s)
                Over  := Substr(Temp,P1)
                r .= Inter RString
            }
            k .= r
        }
        co_Char := co_Char ? co_Char : 0
        return SubStr(k,1,Strlen(k) - co_Char)
    }
}
; 获取文件的属性
; type 的设置
; f 相当于Splitpath, 返回名称、目录、扩展名、驱动器
;   fn 文件名
;   ff 文件目录
;   fe 文件拓展名
;   fo 文件不事拓展名的名称
;   fd 驱动器
; a 相当于FileGetAttrib，返回字符串 "RASHNDOCT" 中部分字母组成的子集
; s 相当于FileGetSize ，返回文件大小 , s/sb sk sm 分别代表返回字节大小、返回千字节大小，返回兆字节大小，默认字节
; t 相当于FileGetTime , 返回文件的时间，t/tm tc ta 分别代表修改时间，创建时间，上次访问时间
; v 相当于FileGetVersion ，返回文件的版本
; l 相当于FileGetShortcut ，返回快捷方式的属性 ,
;   lt  用来存储快捷方式目标的变量名 (不包含它可能含有的任何参数). 例如: C:\WINDOWS\system32\notepad.exe
;   lf  用来保存快捷方式工作目录的变量名. 例如: C:\My Documents. 如果在字符串中存在像 %WinDir% 这样的环境变量, 那么解决它们的一种方法是使用
;   la  用来保存快捷方式参数的变量名 (如果没有则为空).
;   ld  用来保存快捷方式注释的变量名 (如果没有则为空).
;   li  用来保存快捷方式图标文件名的变量名 (如果没有则为空).
;   ln  用来保存快捷方式图标在图标文件中编号的变量名 (如果没有则为空). 这个值通常为 1, 表示首个图标.
;   lr  用来存储快捷方式初始运行方式的变量名, 其值为下列数字的其中一个: 1: 普通 3: 最大化 7: 最小化
; n 获取文件名/文件夹名
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
; EscapeSwitch(switch) {{{2
EscapeSwitch(switch){
    ;switch := RegExReplace(switch,"(^\{)|(\}$)")
    switch := RegExReplace(switch,"\\(?=[\[\]\{\}])")
    return switch
}



; ==============================================
; 界面代码
Config:
TV_SelectItem := ""
TV_SelectItemID := ""
ImageListID ? IL_Destroy(ImageListID)
ImageListID := IL_Create(10)
Menu,Tools,Add
Menu,Tools,Delete
Menu,Tools,Add,% Languages.GetValue(Lang,"Menu Debugger"),Debugger
Menu,Tools,Add,% Languages.GetValue(lang,"Menu variable tool"),Test
Menu,Tools,Add
Menu,Tools,Add,% Languages.GetValue(lang,"Menu class viewer"),Test
Menu,Tools,Add,% Languages.GetValue(lang,"Menu correlation type"),Test
Menu,Tools,Add,% Languages.GetValue(Lang,"Mneu color viewer"),Test
Menu,Help,Add
Menu,Help,Delete
Menu,Help,Add,% Languages.GetValue(lang,"Help topic"),Test
Menu,Help,Add,% Languages.GetValue(lang,"Open HomePage"),Test
Menu,Help,Add
Menu,Help,Add,% Languages.GetValue(Lang,"About"),Test
Menu,MenuTools,Add
Menu,MenuTools,Delete
Menu,MenuTools,Add,% Languages.GetValue(Lang,"Item viewer"),ItemViewer
Menu,MenuTools,Add
Menu,MenuTools,Add,% Languages.GetValue(Lang,"item export"),Test
Menu,MenuTools,Add,% Languages.GetValue(Lang,"item import"),Test
Menu,Config,Add,% Languages.GetValue(Lang,"Menu Option"),Test
Menu,Config,Add,% Languages.GetValue(Lang,"Menu Toolbar"),Test
Menu,Config,Add
Menu,Config,Add,% Languages.GetValue(Lang,"Menu Option Backup"),Test
Menu,Config,Add,% Languages.GetValue(Lang,"Menu Option Restore"),Test
Menu,MenuBar,Add
Menu,MenuBar,Delete
Menu,MenuBar,Add,% Languages.GetValue(Lang,"Menu Tools"),:Tools
Menu,MenuBar,Add,% Languages.GetValue(Lang,"Menu Item"),:MenuTools
Menu,MenuBar,Add,% Languages.GetValue(Lang,"Menu Config"),:Config
Menu,MenuBar,Add,% Languages.GetValue(Lang,"Menu Help"),:Help
GUI,Config:Destroy
GUI,Config:Default
GUI,Config:+Theme +hwndhGUI +LastFound 
GUI,Config:Font,s9 ,Microsoft YaHei
GUI,Config:Menu,MenuBar
;======
GUI,Config:Add,TreeView,x10 y38 w240 h447 altsubmit vMenuTreeView gSelectItem ImageList%ImageListID% hwndhTreeView
GUI,Config:Add,Edit,x261 y38 h24 w280
;GUI,Config:Add,Picture,x264 y35 h20 w20,e:\Program Files\MenuZ 2\Resource\Settings.ico
;====== 菜单有效性
GUI,Config:Add,GroupBox,x260 y205 w360 h240,% Languages.GetValue(Lang,"Menu availability")
;====== 类型
GUI,Config:Add,Text,x270 y233,% Languages.GetValue(Lang,"Type")
GUI,Config:Add,DropDownList,x317 y230 w70 h22 R3 Choose1 AltSubmit ,% Languages.GetValue(Lang,"Include") "|" Languages.GetValue(Lang,"Exclude") "|" Languages.GetValue(Lang,"Regexp") "|"
GUI,Config:Add,Edit,x397 y230 w210 h24
;====== 多类型
GUI,Config:Add,Text,x270 y263,% Languages.GetValue(Lang,"mType")
GUI,Config:Add,DropDownList,x317 y260 w70 h22 R3 Choose1 AltSubmit ,% Languages.GetValue(Lang,"Include") "|" Languages.GetValue(Lang,"Exclude") "|" Languages.GetValue(Lang,"Regexp") "|"
GUI,Config:Add,Edit,x397 y260 w210 h24
;====== 控件名
GUI,Config:Add,Text,x270 y293,% Languages.GetValue(Lang,"Class")
GUI,Config:Add,DropDownList,x317 y290 w70 h22 R3 Choose1 AltSubmit ,% Languages.GetValue(Lang,"Include") "|" Languages.GetValue(Lang,"Exclude") "|" Languages.GetValue(Lang,"Regexp") "|"
GUI,Config:Add,Edit,x397 y290 w210 h24
;====== 窗体类
GUI,Config:Add,Text,x270 y323,% Languages.GetValue(Lang,"Control")
GUI,Config:Add,DropDownList,x317 y320 w70 h22 R3 Choose1 AltSubmit ,% Languages.GetValue(Lang,"Include") "|" Languages.GetValue(Lang,"Exclude") "|" Languages.GetValue(Lang,"Regexp") "|"
GUI,Config:Add,Edit,x397 y320 w210 h24
;====== 路径
GUI,Config:Add,Text,x270 y353,% Languages.GetValue(Lang,"filepath")
GUI,Config:Add,DropDownList,x317 y350 w70 h22 R3 Choose1 AltSubmit ,% Languages.GetValue(Lang,"Include") "|" Languages.GetValue(Lang,"Exclude") "|" Languages.GetValue(Lang,"Regexp") "|"
GUI,Config:Add,Edit,x397 y350 w210 h24
;====== 文件名
GUI,Config:Add,Text,x270 y383,% Languages.GetValue(Lang,"filename")
GUI,Config:Add,DropDownList,x317 y380 w70 h22 R3 Choose1 AltSubmit ,% Languages.GetValue(Lang,"Include") "|" Languages.GetValue(Lang,"Exclude") "|" Languages.GetValue(Lang,"Regexp") "|"
GUI,Config:Add,Edit,x397 y380 w210 h24
;====== 函数
GUI,Config:Add,Text,x270 y413,% Languages.GetValue(Lang,"function")
GUI,Config:Add,ComboBox,x317 y410 w112 h22 R3  AltSubmit ,
;====== 模式
GUI,Config:Add,Text,x450 y413,% Languages.GetValue(Lang,"mode")
GUI,Config:Add,ComboBox,x494 y410 w112 h22 R3  AltSubmit ,
;====== 菜单有效性结束
GUI,Config:Add,Button,x260 y455 w90 h24,% Languages.GetValue(Lang,"Global Config")
GUI,Config:Add,Button,x390 y455 w70 h24,% Languages.GetValue(Lang,"Button OK")
GUI,Config:Add,Button,x470 y455 w70 h24,% Languages.GetValue(Lang,"Button Cancel")
GUI,Config:Add,Button,x550 y455 w70 h24 gConfigSave Disabled,% Languages.GetValue(Lang,"Button Save")
GUI,Config:Add,Button,x549 y70 h24 w70 gIconSelect, % Languages.GetValue(Lang,"Button Icon")
GUI,Config:Add,Button,x549 y38 h24 w70 gStyleSelect,% languages.GetValue(Lang,"Item Style")
GUI,Config:Add,Edit,x261 y70 h24 w280 ReadOnly
GUI,Config:Add,StatusBar
SB_SetParts(250)
sci.Add(hGui, 260, 102, 360, 90, A_ScriptDir "\Bin\SciLexer.dll")
sci.SetWrapMode(true) ; this removes the horizontal scrollbar
sci.SetMarginWidthN(1, 0) ; this removes the left margin
sci.StyleSetFont(STYLE_DEFAULT,"Microsoft YaHei")
sci.StyleSetSize(STYLE_DEFAULT, 10)
sci.StyleClearAll()
sci.notify := "Notify"
sci.StyleSetFore(SCE_AHKL_LABEL, 0xEE0000)
sci.SCI_STYLESETFONT(SCE_AHKL_LABEL,"Microsoft YaHei")
sci.SCI_STYLESETSIZE(SCE_AHKL_LABEL,10)
sci.STYLESETBOLD(SCE_AHKL_LABEL,1)
sci.StyleSetFore(SCE_AHKL_VAR, 0xAA2288)
sci.SCI_STYLESETFONT(SCE_AHKL_VAR,"Microsoft YaHei")
sci.SCI_STYLESETSIZE(SCE_AHKL_VAR,10)
sci.StyleSetFore(SCE_AHKL_BUILTINVAR, 0x0000EE)
sci.SCI_STYLESETFONT(SCE_AHKL_BUILTINVAR,"Microsoft YaHei")
sci.SCI_STYLESETSIZE(SCE_AHKL_BUILTINVAR,10)
SCI.CLEARCMDKEY(Asc("Q")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("S")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("W")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("G")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("F")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("H")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("H")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("E")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("R")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("O")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("P")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("K")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("N")+(SCMOD_CTRL<<16))
SCI.CLEARCMDKEY(Asc("B")+(SCMOD_CTRL<<16))
SCI.USEPOPUP(0)
IL_Destroy(hILSec)
hILSec := IL_Create(10,10,0)
IL_Add(hILSec,"dmdskres.dll",13)
IL_Add(hILSec,"shell32.dll" ,239)
IL_Add(hILSec,A_ScriptDir "\Resource\Itemdown.png",0)
IL_Add(hILSec,A_ScriptDir "\Resource\Itemup.png",0)
IL_Add(hILSec,A_ScriptDir "\Resource\SubItem.png",0)
IL_Add(hILSec,A_ScriptDir "\Resource\sep.png",0)
IL_Add(hILSec,A_ScriptDir "\Resource\moveup.png",0)
IL_Add(hILSec,A_ScriptDir "\Resource\movedown.png",0)
IL_Add(hILSec,A_ScriptDir "\Resource\Delete.png",0)
hToolbar := Toolbar_Add(hGUI,"ToolbarMenu","flat list tooltips",1,"x10 y5 w240 h30")
Toolbar_SetImageList(hToolbar,hILSec)
Toolbar_Insert(hToolbar,languages.GetValue(Lang,"Select Type") ",1,,,101")
Toolbar_Insert(hToolbar,languages.GetValue(Lang,"Item insert") ",2,,,102")
Toolbar_Insert(hToolbar,"-")
Toolbar_Insert(hToolbar,languages.GetValue(Lang,"Item up") ",4,,,103")
Toolbar_Insert(hToolbar,languages.GetValue(Lang,"Item Down") ",3,,,104")
Toolbar_Insert(hToolbar,languages.GetValue(Lang,"Item Sub") ",5,,,105")
Toolbar_Insert(hToolbar,languages.GetValue(Lang,"sep") ",6,,,106")
Toolbar_Insert(hToolbar,"-")
Toolbar_Insert(hToolbar,languages.GetValue(Lang,"move up") ",7,,,107")
Toolbar_Insert(hToolbar,languages.GetValue(Lang,"move down") ",8,,,108")
Toolbar_Insert(hToolbar,languages.GetValue(Lang,"Item Delete") ",9,,,109")
hToolbarItem := Toolbar_Add(hGUI,"ToolbarItem","flat list tooltips ",1,"x260 y2 w300 h32")
Toolbar_Insert(hToolbarItem,"file,1,,dropdown,201")
;Toolbar_Insert(hToolbarItem,"ssss,3,,dropdown")
GUi,Config:Show,w635 ,% "MenuZ " languages.GetValue(lang,"editor")
If not Strlen(SelectType)
	GoSub,SelectTypeGUI
;根据选择创建菜单项
return

;=============
Test:
return
;=============
ConfigSave:
	ConfigSave()
return
ConfigSave(){
	Global TV_SelectItem,TV_SelectItemID,Menus,Items,EditType,SCI
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	GUIControlGet,newstring,,Edit1
	Next := TV_GetChild(TV_SelectItemID)
	Loop
	{
		If Next = %TV_SelectItemID%
			idx := A_Index - 1
		next := TV_GetNext(next)
		If not next
			Break
		Else
			LastID := next
	}
	If TV_SelectItem <> %NewString%
	{
		LoopKeys := Items.GetKeys(TV_SelectItem)
		Loop,Parse,LoopKeys,`n
		{
			If not strlen(A_LoopField)
				continue
			Items.iniWrite(NewString,A_LoopField,Items.IniRead(TV_SelectItem,A_LoopField))
		}
		Items.IniDelete(TV_SelectItem)
		Menus.iniWrite(EditType,idx,NewString)
	}
	;GUIControlGet,icon,,Edit10
	;Items.iniWrite(NewString,"Icon",Icon)
	SCI.GetText(SCI.GetLength()+1,Txt)
	If SCI.GetLength()
		Items.iniWrite(NewString,"String",Txt)
	Else
		Items.iniWrite(NewString,"String","")
	If not RegExMatch(EditType,"i)^(\.[^\|]*$)|(Folder)|(Drive)|(NoExt)$"){
		GUIControlGet,Typemethod,,ComboBox1
		GUIControlGet,Type,,Edit2
		Items.iniWrite(NewString,"Typemethod",Typemethod-1)
		Items.iniWrite(NewString,"Type",Type)
		GuiControlGet,MultiTypemethod,,ComboBox2, 
		GUIControlGet,MultiType,,Edit3, 
		Items.iniWrite(NewString,"MultiTypemethod",MultiTypemethod-1)
		Items.iniWrite(NewString,"MultiType",MultiType)
	}
	GuiControlGet,Classmethod,,ComboBox3,
	GUIControlGet,class,,Edit4
	Items.iniWrite(NewString,"Classmethod",Classmethod-1)
	Items.iniWrite(NewString,"Class",Class)
	GuiControlGet,Controlmethod,,ComboBox4
	GUIControlGet,Control,,Edit5
	Items.iniWrite(NewString,"Controlmethod",Controlmethod-1)
	Items.iniWrite(NewString,"Control",Control)
	GuiControlGet,FilePathMethod,,ComboBox5
	GUIControlGet,FilePath,,Edit6
	Items.iniWrite(NewString,"FilePathMethod",FilePathMethod-1)
	Items.iniWrite(NewString,"FilePath",FilePath)
	GuiControlGet,FileNameMethod,,ComboBox6
	GUIControlGet,FileName,,Edit7
	Items.iniWrite(NewString,"FileNamemethod",FileNamemethod-1)
	Items.iniWrite(NewString,"FileName",FileName)
	;TV_Modify(TV_SelectItemID,"",NewString)
	Items.Read()
	Menus.Read()
	CreateTVItem()
	;msgbox % TV_SelectItem
	;msgbox % Menus.GetValue(EditType,idx)
}

ItemViewer:
GUI,ItemViewer:Destroy
GUI,ItemViewer:Default
GUI,ItemViewer:+theme 
GUI,ItemViewer:Font,s9,Microsoft YaHei
GUI,ItemViewer:Add,ListView,altsubmit x10 y10 w200 h400 gItemViewer_Select,Section
GUI,ItemViewer:Add,Edit,x220 y10 w300 h400 ReadOnly
GUI,ItemViewer:Show
GUI,ItemViewer:ListView,SysListView321
Sections := Items.GetSections()
Loop,Parse,Sections,`n
{
	If not strlen(A_LoopField)
		continue
	LV_Add("",A_LoopField)
}
return
ItemViewer_Select:
	If A_GuiEvent = Normal
	{
		GUI,ItemViewer:Default
		GUI,ItemViewer:ListView,SysListView321
		LV_GetText(i,A_EventInfo)
		options := Items.GetKeyValue(i)
		GUIControl,,Edit1,% options
	}
return

;=============
StyleSelect:
	If not strlen(TV_SelectItem){
		msgbox % Languages.GetValue(Lang,"Please Select Item")
		return
	}
	GUI,StyleGUI:Destroy
	GUI,StyleGUI:Default
	GUI,StyleGUI:+Theme +hwndhStyleGUI
	GUI,StyleGUI:Font,s9,Microsoft YaHei
	GUI,StyleGUI:Add,Text,   x10 y10 w300 h24 center hwndhStyle,% Languages.GetValue(Lang,"Select Style")
	GUI,StyleGUI:Add,Button, x90 y50 w100 h24 gStyleSelect_Text,% Languages.GetValue(Lang,"Button TextColor")
	GUI,StyleGUI:Add,Button, x200 y50 w100 h24 gStyleSelect_Background,% Languages.GetValue(Lang,"Button BackgroundColor")
	GUI,StyleGUI:Add,CheckBox, x15 y50 w70 h24 gStyleSelect_Bold,% Languages.GetValue(Lang,"Button Bold")
	GUI,StyleGUI:Add,Text,Border x10 y90 w300 h1
	GUI,StyleGUI:Add,Button, x90 y100 w100 h24 gStyleSelect_Default,% Languages.GetValue(Lang,"Button Default")
	;GUI,StyleGUI:Add,Button, x120 y100 w100 h24 ,% Languages.GetValue(Lang,"Button OK")
	GUI,StyleGUI:Add,Button, x200 y100 w100 h24 gStyleSelect_Close,% Languages.GetValue(Lang,"Button Close")
	GUI,StyleGUI:Show,,% Languages.GetValue(Lang,"Select Style")
	;CtlColors.Attach(hStyle,400000,"Red")
	CtlColors.Attach(hStyle,RegExReplace(Items.GetValue(TV_SelectItem,"BGcolor"),"i)^0x"),RegExReplace(Items.GetValue(TV_SelectItem,"Tcolor"),"i)^0x"))
	If Items.GetValue(TV_SelectItem,"Bold")
		GUIControl,,Button3,1
	Else
		GUIControl,,Button3,0
	iTextColor := ""
	iBackgroundColor := ""
return
StyleSelect_Text:
	TextColor := Rgb(Dlg_Color(auto.GetValue("color","Default"),hStyleGUI))
	iTextColor := SubStr("000000" . RegExReplace(TextColor,"i)^0x"),-5)
	CtlColors.Change(hStyle,iBackgroundColor,iTextColor)
	If strlen(TV_SelectItem)
		Items.IniWrite(TV_SelectItem,"TColor",TextColor)
return
StyleSelect_Background:
	BackgroundColor := Rgb(Dlg_Color(auto.GetValue("color","Default"),hStyleGUI))
	iBackgroundColor := SubStr("00000" . RegExReplace(BackgroundColor,"i)^0x"),-5)
	CtlColors.Change(hStyle,iBackgroundColor,iTextColor)
	If strlen(TV_SelectItem)
		Items.IniWrite(TV_SelectItem,"BGColor",BackgroundColor)
return
StyleSelect_Close:
	GUI,StyleGUI:Destroy
return
StyleSelect_Default:
	CtlColors.Detach(hStyle)
	If strlen(TV_SelectItem)
	{
		Items.IniWrite(TV_SelectItem,"TColor","")
		Items.IniWrite(TV_SelectItem,"BGColor","")
	}
return
StyleSelect_Bold:
	GUI,StyleGUI:Default
	GUIControlGet,bold,,Button3
	If Bold
	{
		GUI,StyleGUI:Font,s9 Bold,Microsoft YaHei
		GUIControl,Font,Static1
	}
	Else
	{
		GUI,StyleGUI:Font,s9 Normal,Microsoft YaHei
		GUIControl,Font,static1
	}
	If Strlen(TV_SelectItem)
		Items.IniWrite(TV_SelectItem,"Bold",Bold)
return
Dlg_Color(Color,hwnd){
;http://www.autohotkey.com/board/topic/94083-ahk-11-font-and-color-dialogs/
;Author => maestrith
	static
	global auto
	if !cc{
		VarSetCapacity(CUSTOM,16*A_PtrSize,0),cc:=1,size:=VarSetCapacity(CHOOSECOLOR,9*A_PtrSize,0)
		Loop,16{
			;IniRead,col,%inifile%,color,%A_Index%,0
			NumPut(auto.GetValue("color",A_Index),CUSTOM,(A_Index-1)*4,"UInt")
		}
	}
	NumPut(size,CHOOSECOLOR,0,"UInt"),NumPut(hwnd,CHOOSECOLOR,A_PtrSize,"UPtr")
	,NumPut(Color,CHOOSECOLOR,3*A_PtrSize,"UInt"),NumPut(3,CHOOSECOLOR,5*A_PtrSize,"UInt")
	,NumPut(&CUSTOM,CHOOSECOLOR,4*A_PtrSize,"UPtr")
	ret:=DllCall("comdlg32\ChooseColor","UPtr",&CHOOSECOLOR,"UInt")
	if !ret
	exit
	Loop,16
	auto.IniWrite("color",A_index,NumGet(custom,(A_Index-1)*4,"UInt"))
	rColor:=NumGet(CHOOSECOLOR,3*A_PtrSize,"UInt")
	auto.IniWrite("color","Default",rColor)
	;IniWrite,% NumGet(custom,(A_Index-1)*4,"UInt"),%INIFile%,color,%A_Index%
	;IniWrite,% ,%INIFile%,default,color
	return rColor
}
rgb(c){
	setformat,IntegerFast,H
	c:=(c&255)<<16|(c&65280)|(c>>16),c:=SubStr(c,1)
	SetFormat,IntegerFast,D
	return c
}
;=============
Debugger:
	GUI,Debugger:Destroy
	GUI,Debugger:Default
	GUI,Debugger:+theme +hwndhIconGUI
	GUI,Debugger:Font,s9,Microsoft YaHei
	GUI,Debugger:Add,Edit,x10 y10 w500 h100
	GUI,Debugger:Add,Text,x10 y120 w500 h24 Border
	GUI,Debugger:Show
return
;=============
IconSelect:
If not strlen(TV_SelectItem){
	msgbox % Languages.GetValue(Lang,"Please Select Item")
	return
}
GUI,IconSelect:Destroy
GUI,IconSelect:Default
GUI,IconSelect:+theme +Resize +hwndhIconGUI
GUI,IconSelect:Font,s9,Microsoft YaHei
GUI,IconSelect:Add,Radio   ,x10  y40   w90 h24 gGoReport checked,% Languages.GetValue(Lang,"Report view")
GUI,IconSelect:Add,Radio   ,x120 y40   w90 h24 gGoList,% Languages.GetValue(Lang,"List view") 
GUI,IconSelect:Add,Radio   ,x225 y40   w100 h24 gGoIcons,% Languages.GetValue(Lang,"Icons view")
GUI,IconSelect:Add,Radio   ,x340 y40   w100 h24 gGoSmallIcons,% Languages.GetValue(Lang,"smallicons view")
GUI,IconSelect:Add,ListView,x10  y70   w500 h300 nosort -Multi altsubmit gIconSelect_OK ,% Languages.GetValue(Lang,"Icon file")
GUI,IconSelect:Add,Edit,x10  y380  w500 r1, `%A_ScriptDir`%\Resource
GUI,IconSelect:Add,Radio   ,x10  y419  h24 gButton_Icon_Open, % Languages.GetValue(Lang,"button file")
GUI,IconSelect:Add,Radio   ,x80  y419  h24 checked gButton_Icon_Open, % Languages.GetValue(Lang,"button folder")
GUI,IconSelect:Add,Button  ,x165 y418  h24 w80 gButton_Icon_Search Default,% Languages.GetValue(Lang,"Button Search")
GUI,IconSelect:Add,Button  ,x431 y418  h24 w80 gButton_Icon_Cancel,% Languages.GetValue(lang,"Button Cancel")
GUI,IconSelect:Add,Button  ,x261 y418  h24 w80 gIconSelect_Clear,% Languages.GetValue(lang,"Button IconClear")
GUI,IconSelect:ListView,SysTreeView321
LV_ModifyCol(1,60)
;GUI,IconSelect:Add,Text    ,x10  y10   w400 h24 ,%  TV_SelectItem "  >>  " Languages.GetValue(Lang,"select icon") 
GUI,IconSelect:Add,Edit,    x40 y10 w465 h24 ReadOnly ,% TV_SelectItem
Pic :=  Items.GetValue(TV_SelectItem,"Icon")
idx := RegExReplace(pic,".*:")
ppath := RegExReplace(pic,":[-\d]*$")
GUI,IconSelect:Add,Picture, x16 y14 w16 h16 icon%idx%,% Menus.ReplaceEnv(ppath)
GUI,IconSelect:Show,w520 ,% Languages.GetValue(Lang,"select icon") 
WinMove,ahk_id %hIconGUI%,,,,535
Icon_Search_stop := False
Icon_Search()
return
IconGUI_Size(w,p){
	Global hIconGUI,hTypeGUI
	IfWinActive ahk_id %hIconGUI%
	{
		Anchor("SysListView321","wh")
		Anchor("Edit1","wy")
		Anchor("Button5","y")
		Anchor("Button6","y")
		Anchor("Button7","xy")
		Anchor("Button8","xy")
		Anchor("Button9","xy")
		GUI,IconSelect:Default
		GUI,IconSelect:ListView,SysListView321
		ControlGetPos , , , w, ,SysListView321,ahk_id %hIconGUI%
		LV_ModifyCol(2,w-60)
	}
	IfWinActive ahk_id %hTypeGUI%
	{
		Anchor("SysListView321","wh")
		Anchor("Button1","y")
		Anchor("Button2","y")
		Anchor("Button3","y")
		Anchor("Button4","xy")
	}
}
IconSelect_Clear:
	GUI,IconSelect:Default
	GUIControlGet,i,,static1
	i := RegExReplace(i,"\s\s>>\s.*$")
	Items.IniDelete(i,"Icon")
	Items.Read()
	GUI,IconSelect:Destroy
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	GUIControl,,Edit10
	TV_Modify(TV_SelectItemID,"Icon9999")
return
IconSelect_OK:
	If A_GuiEvent = DoubleClick
	{
		GUI,IconSelect:Default
		GUI,IconSelect:ListView,SysTreeView321
		LV_GetText(icon,A_EventInfo,2)
		GUIControlGet,ItemName,,Edit2
		Items.iniWrite(ItemName,"Icon",Icon)
		Gui,IconSelect:Destroy
		GUI,Config:Default
		GUI,Config:TreeView,SysTreeView321
		GUIControl,,Edit10,% Icon
		If Strlen(Icon){
			If RegExMatch(icon,":[-\d]*$"){
				pos := RegExMatch(Icon,":[^:]*$")
				file   := Menus.ReplaceEnv(SubStr(icon,1,pos-1))
				Number := (number := substr(icon,pos+1)) > 0 ? Number + 1 : Number
				ic := IL_Add(ImageListID,file,Number) 
			}
			Else
				ic := IL_Add(ImageListID,Menus.ReplaceEnv(icon))
			Options := "icon" ic 
			TV_Modify(TV_SelectItemID,options)
		}

		
	}
return
Button_Icon_Cancel:
	GUI,IconSelect:Destroy
return
Button_Icon_Open:
	Button_Icon_Open()
return
Button_Icon_Open()
{
	Global languages,Lang
	GUI,IconSelect:Default
	GUI,IconSelect:ListView,SysTreeView321
	GUIControlGet,type,,Button5
	If type
		FileSelectFile,file	,3,  , ,% Languages.GetValue(Lang,"Icon file") "(*.ICO; *.CUR; *.ANI; *.EXE; *.DLL; *.CPL; *.SCR; *.PNG)"
	Else
		FileSelectFolder,file ,*%A_ScriptDir%\Icons\,2, % languages.GetValue(Lang,"select icon folder")
	If not strlen(file)
		return
	GUIControl,,Edit1,%file%
	Icon_Search()
}
Button_Icon_Search:
	If Icon_Search_stop
		Settimer,Icon_Search,20
	Else
		Icon_Search_stop := True
return
Icon_Search:
	Settimer,Icon_Search,off
	Icon_Search_stop := False
	Icon_Search()
return
Icon_Search(){
	Global Languages,Lang,Icon_Search_stop,Menus
	GUI,IconSelect:Default
	GUI,IconSelect:ListView,SysTreeView321
	GUIControlGet,file,,Edit1
	GUIControlGet,type,,Button5
	GUIControl,,Button7,% Languages.GetValue(Lang,"Button Stop")
	file := Menus.ReplaceEnv(file)
	LV_Delete()
	iconfile := file
	iconfilerel := iRelativePath(file)
	IconListSmall := IL_Create(100,100,0)
	ListID   := LV_SetImageList(IconListSmall)
	If ListID
		IL_Destroy(ListID)
	IconListLarge := IL_Create(100,100,1)
	ListID   := LV_SetImageList(IconListLarge)
	If ListID
		IL_Destroy(ListID)
	If Type
	{
		m := 0
		Loop, 9999
		{
			If (id := IL_Add(IconListSmall,Iconfile,A_Index)) {
				IL_Add(IconListLarge,iconfile,A_Index)
				LV_Add("Icon"  id,A_Index,iconfilerel ":" A_Index-1)
				m++
			}
			Else
				Break
		}
		If (not m) And (id := IL_Add(IconListSmall,A_LoopFileFullPath)){
			IL_Add(IconListLarge,A_LoopFileFullPath,A_Index)
			LV_Add("Icon" id ,i,iRelativePath(A_LoopFileFullPath))
			i++
		}
	}
	Else
	{
		i := 1
		If InStr(FileExist(IconFile),"D")
		{
			If RegExMatch(IconFile,"\\$")
				IconFile := SubStr(IconFile,1,Strlen(IconFile)-1)
			Loop,%iconfile%\*.*,0,1
			{
				If RegExMatch(A_LoopFileFullPath,"i)(\.ICO)|(\.CUR)|(\.ANI)|(\.EXE)|(\.DLL)|(\.CPL)|(\.SCR)|(\.PNG)$")
				{
					m := 0
					Loop,9999
					{
						If (id := IL_Add(IconListSmall,A_LoopFileFullPath,A_Index)) {
							IL_Add(IconListLarge,A_LoopFileFullPath,A_Index)
							LV_Add("Icon" id ,i,iRelativePath(A_LoopFileFullPath)":" A_Index-1)
							i++
							m++
						}
						Else
							Break
						If Icon_Search_stop
							Break
					}
					If ( not m ) and (id := IL_Add(IconListSmall,A_LoopFileFullPath)){
						IL_Add(IconListLarge,A_LoopFileFullPath,A_Index)
						LV_Add("Icon" id ,i,iRelativePath(A_LoopFileFullPath))
						i++
					}
				}
				If Icon_Search_stop
					Break
			}
		}
		Icon_Search_stop := True
		If i = 0
			MsgBox % Languages.GetValue(Lang,"select icon folder error")
	}
	GUIControl,,Button7,% Languages.GetValue(Lang,"Button Search")
}
GoReport:
GUI,IconSelect:Default
GUI,IconSelect:ListView,SysListView321
GuiControl,+Report, SysListView321
Return

GoIcons:
GUI,IconSelect:Default
GUI,IconSelect:ListView,SysListView321
GuiControl,+Icon, SysListView321
Return

GoSmallIcons:
GUI,IconSelect:Default
GUI,IconSelect:ListView,SysListView321
GuiControl,+IconSmall,SysListView321
Return

GoList:
GUI,IconSelect:Default
GUI,IconSelect:ListView,SysListView321
GuiControl,+List, SysListView321
Return

iRelativePath(i){
	file := RegExReplace(i,"i)" ToMatch(A_ScriptDir),"%A_ScriptDir%")
	file := RegExReplace(file,"i)" ToMatch(A_WinDir),"%A_WinDir%")
	return file
}


SelectTypeGUI:
GUI,SelectTypeGUI:Destroy
GUI,SelectTypeGUI:Default
GUI,SelectTypeGUI:+theme +Owner%hGUI% +hwndhTypeGUI +Resize
GUI,SelectTypeGUI:Font,s9,Microsoft YaHei
GUI,SelectTypeGUI:Add,ListView,x10 y10 w460 h380 Grid gSelectTypeGUI_OK nosort altsubmit,% languages.GetValue(Lang,"Type|Describe")
GUI,SelectTypeGUI:Add,Button,x10 y400 w80 gSelectTypeGUI_Add,% Languages.GetValue(Lang,"Add Type")
GUI,SelectTypeGUI:Add,Button,x100 y400 w80 Disabled gSelectTypeGUI_Edit,% Languages.GetValue(Lang,"Edit Type")
GUI,SelectTypeGUI:Add,Button,x190 y400 w80 Disabled gSelectTypeGUI_Delete,% Languages.GetValue(Lang,"Delete Type")
;GUI,SelectTypeGUI:Add,Button,x300 y400 w80 ,% Languages.GetValue(Lang,"Button Ok")
GUI,SelectTypeGUI:Add,Button,x390 y400 w80 gSelectTypeGUI_Cancel,% Languages.GetValue(Lang,"Button Cancel")
GUI,SelectTypeGUI:show,w480 h440,% Languages.GetValue(Lang,"Select Type")
LV_ModifyCol(1,"150")
LV_ModifyCol(2,"304")
LoadSelectType()
WinMove,ahk_id %hTypeGUI%,,,,481
return

SelectTypeGUI_Delete:
GUI,SelectTypeGUI:Default
Number := LV_GetNext()
LV_GetText(SelectTypeGUI_Edit_Type,Number)
MsgBox, 4,, % languages.GetValue(Lang,"do you delete this type") " [ " SelectTypeGUI_Edit_Type " ] ?"
IfMsgBox No
	return
If LVIndex["other Type"] < Number
	Menus.IniDelete(SelectTypeGUI_Edit_Type)
Else If  LVIndex["Class Type"] < Number
{
	Menus.IniDelete("ClassType",SelectTypeGUI_Edit_Type)
	Menus.IniDelete(SelectTypeGUI_Edit_Type)
}
Else If  LVIndex["File Type"] < Number
{
	Menus.IniDelete("FileType",SelectTypeGUI_Edit_Type)
	Menus.IniDelete(SelectTypeGUI_Edit_Type)
}
;	msgbox 删除文件类型 %SelecTypeGUI_Edit_Type% ?
Else
{
	Menus.IniDelete("TextType",SelectTypeGUI_Edit_Type)
	Menus.IniDelete(SelectTypeGUI_Edit_Type)
}
Menus.Read()
LoadSelectType()
return


SelectTypeGUI_Add:
GUI,SelectTypeGUI_Add:Destroy
GUI,SelectTypeGUI_Add:Default
GUI,SelectTypeGUI_Add:Font,s9,Microsoft YaHei
GUI,SelectTypeGUI_Add:+theme +Owner%hTypeGUI%
GUI,SelectTypeGUI_Add:Add,Text,w280 x10 y10 h24,% languages.GetValue(lang,"Type") ":"
GUI,SelectTypeGUI_Add:Add,Edit,w280 x10 y30 h24 ,
GUI,SelectTypeGUI_Add:Add,Text,w280 x10 y60 h24,% languages.GetValue(lang,"Describe")  languages.GetValue(lang,"Describe2")":"
GUI,SelectTypeGUI_Add:Add,Edit,w280 x10 y78 h24 Disabled ,
GUI,SelectTypeGUI_Add:Add,DropDownList,w280 x10 y120 h24 Choose1 r4 altsubmit gSelectTypeGUI_Add_Other,% Languages.GetValue(lang,"other|TextType|FileType|ClassType")
GUI,SelectTypeGUI_Add:Add,Button,x10 y160  w135 h26 Default gSelectTypeGUI_Add_OK , % Languages.GetValue(Lang,"Button Ok")
GUI,SelectTypeGUI_Add:Add,Button,x156 y160 w135 h26 gSelectTypeGUI_Add_Cancel, % Languages.GetValue(Lang,"Button Cancel")
GUI,SelectTypeGUI_Add:show,w300 h200,% Languages.GetValue(Lang,"Add Type")
return
SelectTypeGUI_Add_Cancel:
GUI,SelectTypeGUI_Add:Destroy
return
SelectTypeGUI_Add_OK:
	SelectTypeGUI_Add_OK()
return
SelectTypeGUI_Add_OK(){
	Global Menus
	GUI,SelectTypeGUI_Add:Default
	GUIControLGet,Type,,Edit1
	GUIControLGet,Desc,,Edit2
	GUIControLGet,C,,ComboBox1
	Menus.iniWrite(Type,"","")
	If c = 2
		Menus.iniWrite("TextType",Type,Desc)
	If c = 3
		Menus.iniWrite("FileType",Type,Desc)
	If c = 4
		Menus.iniWrite("ClassType",Type,Desc)
	Menus.Read()
	LoadSelectType()
	GUI,SelectTypeGUI_Add:Destroy
}
SelectTypeGUI_Add_other:
GUI,SelectTypeGUI_Add:Default
GUIControlGet,C,,ComboBox1
If c = 1
	GUIControl,Disabled,edit2
Else
	GUIControl,Enable,edit2
return

SelectTypeGUI_Edit:
GUI,SelectTypeGUI:Default
Number := LV_GetNext()
LV_GetText(SelectTypeGUI_Edit_Type,Number)
LV_GetText(SelectTypeGUI_Edit_Desc,Number,2)
GUI,SelectTypeGUI_Edit:Destroy
GUI,SelectTypeGUI_Edit:Default
GUI,SelectTypeGUI_Edit:Font,s9,Microsoft YaHei
GUI,SelectTypeGUI_Edit:+theme +Owner%hTypeGUI%
GUI,SelectTypeGUI_Edit:Add,Text,w280 x10 y10 h24,% languages.GetValue(lang,"Type") ":"
GUI,SelectTypeGUI_Edit:Add,Edit,w280 x10 y30 h24 , % SelectTypeGUI_Edit_Type
GUI,SelectTypeGUI_Edit:Add,Text,w280 x10 y60 h24,% languages.GetValue(lang,"Describe") languages.GetValue(lang,"Describe2") ":"
GUI,SelectTypeGUI_Edit:Add,Edit,w280 x10 y78 h24 , % SelectTypeGUI_Edit_Desc
GUI,SelectTypeGUI_Edit:Add,DropDownList,w280 x10 y120 h24 r4 altsubmit gSelectTypeGUI_Edit_other,% Languages.GetValue(lang,"other|TextType|FileType|ClassType")
GUI,SelectTypeGUI_Edit:Add,Button,x10 y160  w135 h26 Default gSelectTypeGUI_Edit_OK , % Languages.GetValue(Lang,"Button Modity")
GUI,SelectTypeGUI_Edit:Add,Button,x156 y160 w135 h26 gSelectTypeGUI_Edit_Cancel, % Languages.GetValue(Lang,"Button Cancel")
GUI,SelectTypeGUI_Edit:show,w300 h200,% Languages.GetValue(Lang,"Edit Type")
If LVIndex["other Type"] < Number
{
	GUIControl,Choose,ComboBox1,1
	GUIControl,Disabled,edit2
	SelectTypeGUI_Edit_Choose  := 1
}
Else If  LVIndex["Class Type"] < Number
{
	GUIControl,Choose,ComboBox1,4
	SelectTypeGUI_Edit_Choose  := 4
}
Else If  LVIndex["File Type"] < Number
{
	GUIControl,Choose,ComboBox1,3
	SelectTypeGUI_Edit_Choose  := 3
}
Else
{
	GUIControl,Choose,ComboBox1,2
	SelectTypeGUI_Edit_Choose  := 2
}
return
;--- ----
SelectTypeGUI_Edit_other:
GUI,SelectTypeGUI_Edit:Default
GUIControlGet,C,,ComboBox1
If c = 1
	GUIControl,Disabled,edit2
Else
	GUIControl,Enable,edit2
return
SelectTypeGUI_Edit_OK:
GUI,SelectTypeGUI_Edit:Default
GUIControlGet,SelectTypeGUI_Edit_New_Type,,Edit1
GUIControlGet,SelectTypeGUI_Edit_New_Desc,,Edit2
GUIControlGet,SelectTypeGUI_Edit_New_Choose,,ComboBox1
;Msgbox % SelectTypeGUI_Edit_New_Type "`n" SelectTypeGUI_Edit_New_Desc "`n" SelectTypeGUI_Edit_New_Choose
If SelectTypeGUI_Edit_New_Choose = 1
{
	If SelectTypeGUI_Edit_Choose = 1
	{
		SelectTypeGUI_Edit_New_Desc := Menus.GetKeys(SelectTypeGUI_Edit_Type)
		Loop,Parse,SelectTypeGUI_Edit_New_Desc,`n
		{
			If not Strlen(A_LoopField)
				continue
			Menus.iniWrite(SelectTypeGUI_Edit_New_Type,A_LoopField,Menus.GetValue(SelectTypeGUI_Edit_Type,A_LoopField))
		}
		Menus.IniDelete(SelectTypeGUI_Edit_Type)
		GUI,SelectTypeGUI:Default
		GUI,SelectTypeGUI:ListView,SysTreeView321
		;LV_Modify(Number,"Select",SelectTypeGUI_Edit_New_Type)
	}
	If SelectTypeGUI_Edit_Choose = 2
		Menus.IniDelete("TextType",SelectTypeGUI_Edit_Type)
	If SelectTypeGUI_Edit_Choose = 3
		Menus.IniDelete("FileType",SelectTypeGUI_Edit_Type)
	If SelectTypeGUI_Edit_Choose = 4
		Menus.IniDelete("ClassType",SelectTypeGUI_Edit_Type)
}
If SelectTypeGUI_Edit_New_Choose = 2
{
	Menus.iniWrite("TextType",SelectTypeGUI_Edit_New_Type,SelectTypeGUI_Edit_New_Desc)
	If SelectTypeGUI_Edit_Choose = 3
		Menus.IniDelete("FileType",SelectTypeGUI_Edit_Type)
	If SelectTypeGUI_Edit_Choose = 4
		Menus.IniDelete("ClassType",SelectTypeGUI_Edit_Type)
}
If SelectTypeGUI_Edit_New_Choose = 3
{
	Menus.iniWrite("FileType",SelectTypeGUI_Edit_New_Type,SelectTypeGUI_Edit_New_Desc)
	If SelectTypeGUI_Edit_Choose = 2
		Menus.IniDelete("TextType",SelectTypeGUI_Edit_Type)
	If SelectTypeGUI_Edit_Choose = 4
		Menus.IniDelete("ClassType",SelectTypeGUI_Edit_Type)
}
If SelectTypeGUI_Edit_New_Choose = 4
{
	Menus.iniWrite("ClassType",SelectTypeGUI_Edit_New_Type,SelectTypeGUI_Edit_New_Desc)
	If SelectTypeGUI_Edit_Choose = 2
		Menus.IniDelete("TextType",SelectTypeGUI_Edit_Type)
	If SelectTypeGUI_Edit_Choose = 3
		Menus.IniDelete("FileType",SelectTypeGUI_Edit_Type)
}
Menus.Read()
GUI,SelectTypeGUI_Edit:Destroy
LoadSelectType()
return
SelectTypeGUI_Edit_Cancel:
GUI,SelectTypeGUI_Edit:Destroy
return

OpenConfigGUI:
	OpenConfigGUI()
return
OpenConfigGUI(Type=""){
	Global SelectType,EditType,iString,languages,Lang
	GoSub,Config
	If Strlen(Type)
		EditType := Type
	Else
	{
		If SelectType = Class
			EditType := iString
		Else
			EditType := SelectType
	}
	ConfigGUI_check(EditType)
	CreateTVItem(EditType)
}


LoadSelectType(){
	Global Menus,languages,Lang,LVIndex
	GUI,SelectTypeGUI:Default
	GUI,SelectTypeGUI:ListView,SysTreeView321
	LV_Delete()
	LVIndex := []
	LVIndex["InSide Type"] := LV_Add("","===" languages.GetValue(lang,"InSide Type") "===","==================")
	LV_Add("","Any",languages.GetValue(lang,"Any Desc"))
	LV_Add("","AnyText",languages.GetValue(lang,"AnyText Desc"))
	LV_Add("","AnyFile",languages.GetValue(lang,"AnyFile Desc"))
	LV_Add("","AnyClass",languages.GetValue(lang,"AnyClass Desc"))
	LV_Add("","Folder",languages.GetValue(lang,"Folder Desc"))
	LV_Add("","Drive",languages.GetValue(lang,"Drive Desc"))
	LV_Add("","MultiFiles",languages.GetValue(lang,"MultiFiles Desc"))
	LV_Add("","NoExt",languages.GetValue(lang,"NoExt Desc"))
	LV_Add("","Unknown",languages.GetValue(lang,"UnKnown Desc"))
	LV_Add("","UnknownClass",languages.GetValue(lang,"UnKnownClass Desc"))
	CustomText := Menus.GetKeys("TextType")
	LVIndex["Text Type"] := LV_Add("","==" languages.GetValue(lang,"Text Type") "==","==================")
	Loop,Parse,CustomText,`n
	{
		If not Strlen(A_LoopField)
			continue
		LV_Add("",A_LoopField,Menus.GetValue("TextType",A_LoopField))
	}
	CustomFile := Menus.GetKeys("FileType")
	LVIndex["File Type"] := LV_Add("","==" languages.GetValue(lang,"File Type") "==","==================")
	Loop,Parse,CustomFile,`n
	{
		If not Strlen(A_LoopField)
			continue
		LV_Add("",A_LoopField,Menus.GetValue("FileType",A_LoopField))
	}
	CustomClass := Menus.GetKeys("ClassType")
	LVIndex["Class Type"] := LV_Add("","==" languages.GetValue(lang,"Class Type") "==","==================")
	Loop,Parse,CustomClass,`n
	{
		If not Strlen(A_LoopField)
			continue
		LV_Add("",A_LoopField,Menus.GetValue("ClassType",A_LoopField))
	}
	LVIndex["other Type"] := LV_Add("","=" languages.GetValue(lang,"Other Type") "=","==================")
	Section := Menus.GetSections()
	Loop,Parse,Section,`n
	{
		If not Strlen(A_LoopField)
			continue
		If RegExMatch(A_LoopField,"i)^(config)|(hotkey)|(TextType)|(FileType)|(ClassType)|(env)|(any)|(anyfile)|(anytext)|(anyclass)|(unknown)|(unknownclass)|(folder)|(Drive)|(NoExt)$")
			continue
		Else
		{
			RowNumber  := 0
			Iscontinue := False
			iMatch := "i)^" ToMatch(A_LoopField) "$"
			Loop % LV_GetCount()
			{
				LV_GetText(Text,A_Index)
				If RegExMatch(Text,iMatch){
					Iscontinue := True
					Break
				}
			}
			If Iscontinue
				continue
			Else
				LV_Add("",A_LoopField)
		}
	}
}
SelectTypeGUI_OK:
	SelectTypeGUI_OK()
return
SelectTypeGUI_OK(){
	Global EditType,LVIndex
	GUI,SelectTypeGUI:Default
	GUI,SelectTypeGUI:ListView,SysTreeView321
	If A_GuiEvent = DoubleClick
	{
		for i , k in LVIndex
			If A_EventInfo = %k%
				return
		LV_GetText(EditType,A_EventInfo)
		GUI,SelectTypeGUI:Destroy
		GUI,Config:+LastFound
		WinActivate
		ConfigGUI_check(EditType)	
		CreateTVItem(EditType)
	}
	If A_GuiEvent = Normal
	{
		i := LVIndex["Text Type"]
		If A_EventInfo > %i%
		{
			GUIControl,Enable,Button3
			GUIControl,Enable,Button2
			for i , k in LVIndex
				If A_EventInfo = %k%
				{
					GUIControl,Disable,Button3
					GUIControl,Disable,Button2
				}
		}
		Else
		{
			GUIControl,Disable,Button3
			GUIControl,Disable,Button2
		}
	}
}

ConfigGUI_check(EditType){
	Global Menus
	GUI,Config:Default
	If RegExMatch(EditType,"i)^(\.[^\|]*$)|(Folder)|(Drive)|(NoExt)$"){
		GUIControl,Disable,ComboBox1
		GUIControl,Disable,Edit2
		GUIControl,Disable,ComboBox2
		GUIControl,Disable,Edit3
	}
	Else
	{
		GUIControl,Enable,ComboBox1
		GUIControl,Enable,Edit2
		GUIControl,Enable,ComboBox2
		GUIControl,Enable,Edit3
	}
	IfWinExist,ahk_class %EditType%
	{
		GUIControl,Disable,ComboBox3
		GUIControl,Disable,Edit4
	}
	Else If strlen(Menus.GetValue("ClassType",EditType))
	{
		GUIControl,Disable,ComboBox3
		GUIControl,Disable,Edit4
	}
	Else
	{
		GUIControl,Enable,ComboBox3
		GUIControl,Enable,Edit4
	}

}
SelectTypeGUI_Cancel:
GUI,SelectTypeGUI:Destroy
GUI,Config:+LastFound
WinActivate
return


ToolbarItem(hCtrl,Event,Txt,Pos,ID){
	;Toolbar_DeleteButton(hCtrl)
		Global SCI
	If Event = Click
	{
		If ID = 201
		{
			tips := Languages.GetValue(Lang,"Box file")
			FileSelectFile,file,s3,%rootdir%,%tips%
			SCI.INSERTTEXT(SCI.GETCURRENTPOS(),file)
			;SCI.GoToPos(SCI.GETCURRENTPOS()+1)
		}
	}
	If Event = Menu
	{
	}
	
}
ToolbarMenu(hCtrl,Event,Txt,Pos,ID){
	If Event = Click
	{
		If ID = 101
			GoSub,SelectTypeGUI
		;ConfigGUI_AddMenuItem(Direct=1,Sub=0)
		;If ID = 102
		;	CreateTVItem()
		If ID = 103
			ConfigGUI_AddMenuItem(Direct:=0,Sub:=0)
		If ID = 104
			ConfigGUI_AddMenuItem(Direct:=1,Sub:=0)
		If ID = 105
			ConfigGUI_AddMenuItem(Direct:=0,Sub:=1)
		If ID = 106
			ConfigGUI_AddMenuItem(1,0,1)
		If ID = 109
			ConfigGUI_DeleteMenu()
	}
}

SelectItem:
	SelectItem()
return
SelectItem(id=0) {
	Global Items,Menus,sci,TV_SelectItem,TV_SelectItemID
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	If A_GuiEvent = RightClick
		GoSub,SelectTypeGUI
	If A_GuiEvent = DoubleClick
	{
		TV_GetText(i,A_EventInfo)
		TV_SelectItem := i
		TV_SelectItemID := A_EventInfo
		GUIControl,,Edit1, % i
		GUIControl,,Edit10, % Items.GetValue(i,"Icon")
		string := Items.GetValue(i,"String")
		strlen(string) ? SCI.SetText(unused,string) : SCI.CLEARALL()
		Typemethod := strlen(Typemethod := Items.GetValue(i,"Typemethod")) ? Typemethod + 1 : 1
		type := Items.GetValue(i,"Type")
		MultiTypemethod := strlen(MultiTypemethod := Items.GetValue(i,"MultiTypemethod")) ? MultiTypemethod + 1 : 1
		MultiType := Items.GetValue(i,"MultiType")
		Classmethod := strlen(Classmethod := Items.GetValue(i,"Classmethod")) ? Classmethod + 1 : 1
		class := Items.GetValue(i,"Class")
		Controlmethod := strlen(Controlmethod := Items.GetValue(i,"Controlmethod")) ? Controlmethod + 1 : 1
		control := Items.GetValue(i,"control")
		FilePathMethod := strlen(FilePathMethod := Items.GetValue(i,"FilePathMethod") ) ? FilePathmethod + 1 : 1
		FilePath := Items.GetValue(i,"FilePath")
		FileNamemethod := strlen(FileNamemethod := Items.GetValue(i,"FileNamemethod")) ? FileNamemethod + 1 : 1 
		FileName := Items.GetValue(i,"FileName")
		GuiControl, Choose, ComboBox1, % Typemethod
		GUIControl, ,Edit2,% type
		GuiControl, Choose, ComboBox2, % MultiTypemethod
		GUIControl, ,Edit3,% MultiType
		GuiControl, Choose, ComboBox3, % Classmethod
		GUIControl, ,Edit4,% class
		GuiControl, Choose, ComboBox4, % Controlmethod
		GUIControl, ,Edit5,% control 
		GuiControl, Choose, ComboBox5, % FilePathMethod
		GUIControl, ,Edit6,% FilePath 
		GuiControl, Choose, ComboBox6, % FileNameMethod
		GUIControl, ,Edit7,% FileName
		GUIControl,Enable,Button5,
		;type := Items.GetValue(i,"type")
	}
	/*
	*/
	If A_GuiEvent = Normal
	{
		If A_EventInfo = %TV_SelectItemID%
			GUIControl,Enable,Button5,
		Else
			GUIControl,Disable,Button5,
		;TV_GetText(i,A_EventInfo)
	}
}
; =========================================
; 添加菜单项
ConfigGUI_AddMenuItem(Direct=1,Sub=0,sep=0)
{
	Global Items,Menus,EditType,ppp,languages,Lang
	GuiControl, -Redraw, MenuTreeView 
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	If Not EditType
	{
		GoSub,SelectTypeGUI
		return
	}
	;GUIControlGet,NewItem,,Edit1
	Random,r,10000,99999
	If sep
		NewItem := "-"
	Else
		NewItem := languages.GetValue(Lang,"Menu") " - " r " - " 
	; 获取当前选择的菜单所在级别的所有菜单
	If not TV_GetSelection()
	{
		Loop
		{
			ItemID := TV_GetNext(ItemID)
			If not ItemID
				Break
			LoopID := ItemID
		}
		TV_Modify(LoopID,"Select")
	}
	If ( ParentID := TV_GetParent(SelectID := TV_GetSelection()))
		TV_GetText(Section,ParentID)
	Else
		Section := EditType
	If ( s := Items.GetValue(Section,"sub")) ;如果是Sub的话，还需要转换为Menu.ini中的内容
		Section := s
	; ==========================================
	; 获取当前选择的位置
	Next := TV_GetChild(ParentID)
	Loop
	{
		If Next = %SelectID%
			idx := A_Index
		next := TV_GetNext(next)
		If not next
			Break
		Else
			LastID := next
	}
	MaxIdx := Menus.content[Section "`nnumber"]
	; MaxIdx代表当前级别菜单的总数
	If MaxIdx
	{
		If Sub
		{
			TV_GetText(ParentText,SelectID)
			Items.iniWrite(ParentText,"Sub",ParentText)
			Items.iniWrite(NewItem,"String",NewString)
			Menus.iniWrite(ParentText,1,NewItem)
			TV_Add(NewItem,SelectID,"Select icon9999")
		}
		Else If Direct ; down
		{
			Loop,% Maxidx - idx
				Menus.iniWrite(Section,Maxidx - A_Index + 2,Menus.GetValue(Section,Maxidx-A_Index+1))
			Menus.iniWrite(Section,idx+1,NewItem)
			Items.iniWrite(NewItem,"String",NewString)
			TV_Add(NewItem,ParentID,SelectID " Select icon9999")
		}
		Else ; up
		{
			Loop,% Maxidx - idx + 1
				Menus.iniWrite(Section,Maxidx - A_Index + 2,Menus.GetValue(Section,Maxidx-A_Index+1))
			Menus.iniWrite(Section,idx,NewItem)
			Items.iniWrite(NewItem,"String",NewString)
			If ( PrevID := TV_GetPrev(SelectID) )
				TV_Add(NewItem,ParentID,PrevID " Select icon9999")
			Else
				TV_Add(NewItem,ParentID,"First Select icon9999")
		}
	}
	Else
	{
		Menus.iniWrite(Section,1,NewItem)
		Items.iniWrite(NewItem,"String",NewString)
		TV_Add(NewItem,ParentID,SelectID " Select icon9999")
	}
	Menus.Read()
	If strlen(Content := Items.GetKeys(NewItem))
		GUIControl,,Edit3,% Content
	Else
	{
		Content := Menus.GetKeys(NewItem)
		GUIControl,,Edit3,% Content
	}
	GuiControl, +Redraw, MenuTreeView 
}
ConfigGUI_DeleteMenu(){
	Global Menus,Items,EditType
	;Items.IniDelete("子菜单2","Sub")
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321

	If ( ParentID := TV_GetParent(SelectID := TV_GetSelection()))
		TV_GetText(Section,ParentID)
	Else
		Section := EditType

	If ( s := Items.GetValue(Section,"sub")) ;如果是Sub的话，还需要转换为Menu.ini中的内容
		Section := s
	If ( ParentID := TV_GetParent(SelectID := TV_GetSelection()))
		TV_GetText(Section,ParentID)
	Else
		Section := EditType
	If ( s := Items.GetValue(Section,"sub")) ;如果是Sub的话，还需要转换为Menu.ini中的内容
		Section := s

	TV_GetText(Item,SelectID)
	MaxIdx := Menus.content[Section "`nnumber"]
	; ==========================================
	; 获取当前选择的位置
	Next := TV_GetChild(ParentID)
	Loop
	{
		If Next = %SelectID%
			idx := A_Index
		next := TV_GetNext(next)
		If not next
			Break
		Else
			LastID := next
	}
	Loop,% MaxIdx - Idx
		Menus.iniWrite(Section,idx + A_Index - 1,Menus.GetValue(Section,Idx + A_Index))
	Menus.content[Section "`nnumber"] := MaxIdx - 1
	Menus.IniDelete(Section,MaxIdx)
	Menus.IniDelete(Items.GetValue(Item,"sub"))
	Items.IniDelete(Item)
	If not ( Menus.content[Section "`nnumber"] )
	{
		TV_GetText(ParentText,TV_GetParent(SelectID))
		mi := Items.GetValue(ParentText,"sub")
		Menus.IniDelete(mi)
		Items.IniDelete(ParentText,"Sub")
	}
	If not ( Menus.Content[EditType "`nnumber"] )
		Menus.IniDelete(EditType)
	Menus.Read()
	TV_Delete(SelectID)
	GUIControl,,Edit3,% Content
}
CreateTVItem(item="")
{
	Global languages,Lang,EditType,Menus
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	;GUIControl,,Edit1,%EditType%
	SB_SetText(" " languages.GetValue(Lang,"Type") " : " EditType)
	desc :=  languages.GetValue(lang,EditType " Desc")
	If not Strlen(desc)
		desc := Menus.GetValue("TextType",EditType)
	If not Strlen(desc)
		desc := Menus.GetValue("FileType",EditType)
	If not Strlen(desc)
		desc := Menus.GetValue("ClassType",EditType)
	SB_SetText( " " languages.GetValue(Lang,"Describe")  " : " Desc ,2)
	TV_Delete()
	return CreateTVItemSub(EditType,0)
}

CreateTVItemSub(item,id)
{
	Global Items,Menus,MenuTreeView,ImageListID,hTreeView
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	GuiControl, -Redraw, MenuTreeView 
	Menus_Keys := Menus.GetKeys(Item)
	Loop,Parse,Menus_Keys,`n
	{ 
		If Strlen(A_LoopField){
			i := Menus.GetValue(item,A_Index)
			icon := Items.GetValue(i,"icon")
			If Strlen(icon){
				If RegExMatch(icon,":[-\d]*$"){
					pos := RegExMatch(Icon,":[^:]*$")
					file   := Menus.ReplaceEnv(SubStr(icon,1,pos-1))
					Number := (number := substr(icon,pos+1)) > 0 ? Number + 1 : Number
					ic := IL_Add(ImageListID,file,Number)
					Options := "icon" ic " Expand"
				}
				Else
				{
					file   := Menus.ReplaceEnv(icon)
					ic := IL_Add(ImageListID,file)
					Options := "icon" ic " Expand"
				}
			
			}
			Else
				Options := "icon9999 Expand"
			Options := Items.GetValue(i,"Bold") ? Options " bold" : Options
			mid := TV_Add(i,id,Options)
;			TV_SetColor(mid,"121212","787878")
			If ( sub := Items.GetValue(Menus.GetValue(item,A_Index),"sub")) 
				If RegExMatch(Item,ToMatch(sub))
					Items.IniDelete(Item,"Sub")
				Else
					CreateTVItemSub(sub,mid)
		}
	}
	GuiControl, +Redraw, MenuTreeView 
	return mid
}

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

GetType(s){
	Global SelectType
	return SelectType
}



DefaultConfig:
dc =
(
[Any]
[AnyText]
[AnyFile]
[AnyClass]
[Folder]
[Drive]
[NoExt]
[UnKnow]
[UnKnowClass]
[MultiFiles]
)
return

#Include %A_ScriptDir%\lib\INI.ahk
#Include %A_ScriptDir%\Plugins\Plugins.ahk
