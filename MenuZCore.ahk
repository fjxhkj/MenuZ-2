SetMouseDelay,-1
DetectHiddenWindows, On
Menu,Tray,Icon,%A_ScriptDir%\Icons\Menuz.ico
#Include %A_ScriptDir%\lib\WinClipAPI.ahk
#Include %A_ScriptDir%\lib\WinClip.ahk
#Include %A_ScriptDir%\lib\PUM_API.ahk
#Include %A_ScriptDir%\lib\PUM.ahk
#Include %A_ScriptDir%\lib\Toolbar.ahk
#Include %A_ScriptDir%\lib\sci.ahk
#Include %A_ScriptDir%\lib\TT.ahk
#Include %A_ScriptDir%\lib\Struct.ahk
#Include %A_ScriptDir%\lib\sizeof.ahk
#Include %A_ScriptDir%\lib\Anchor.ahk
#Include %A_ScriptDir%\lib\Class_CtlColors.ahk
#Include %A_ScriptDir%\lib\Acc.ahk
#Include %A_ScriptDir%\lib\WatchDirectory.ahk
#Include %A_ScriptDir%\lib\GDIP.ahk
;WatchDirectory(A_ScriptDir  "\Plugins\*|.ahk" , "ReportChange")
; ==============================================
; 加载配置文件
Menus     := new ini(A_ScriptDir "\Config\MenuZ.ini")
items     := new ini(A_ScriptDir "\config\item.ini")
Auto      := new ini(A_ScriptDir "\config\auto.ini")
Languages := new ini(A_ScriptDir "\lib\language.ini")
SCI       := new scintilla
win_clip  := new WinClip
win_PUM   := new PUM
win_PUM.SetParams(GetPUMParams())
Lang      := "zh-cn"
SelectType := ""
Mode := ""
Mode_Sep := 0

Menu,Tray,Add,% Languages.GetValue(Lang,"Menu Config"),config
Menu,Tray,Default,% Languages.GetValue(Lang,"Menu Config")
;Menu,Tray,Click, 1

Gui,MZC:Add,Text,,__MZC
GUI,MZC:Add,Edit,gMZCRead

mzLoadHotkey()
mzSetCopyFunction("vim_copy","vim")
mzSetCopyFunction("tc_copy","TTOTAL_CMD")
GoSub,LoadGUIMenu
GoSub LoadCUR

return

MZCRead:
GUI,MZC:Default
GuiControlGet,s,,Edit1
If Strlen(s)
	mzInit(0.8,0)
return

!z::
reload
return

; API 列表{{{1
; ====================================================
; mzInit(time=0,Method=0) {{{2
; 激活MZ获取选择的内容并出菜单
mzInit(time=0,Method=0){
	Global MenuConfig
	Menuconfig := CreateMenuConfig(time,method)
	CreateMenuHead()
	CreateMenuBody(MenuConfig)
	CreateMenuTail()
}
; mzReload() {{{2
mzReload(){
	Reload
}
; mzDefault() {{{2
; 执行默认动作
mzDefault:
	mzDefault()
return
mzDefault(){
	Global MenuConfig,Items,iString,win_PUM
	msgbox default "`n" %iString%
	Loop,Parse,MenuConfig,`n,`r
	{
		If RegExMatch(A_LoopField,"^[\d\s]*=\s*(.*)",s)
			If Items.GetValue(s1,"Default")
			{
				ToExec(Engine(iString,Items.GetValue(s1,"String")))
				return
			}
	}
	ToExec(Engine(iString,"{save:clip} {run:none}"))
}
; mzLoadHotkey() {{{2
; 加载热键
; {win:TTOTAL_CMD} 设置某个窗口类下有效
mzLoadHotkey(){
	Global Menus
	k := Menus.GetKeys("Hotkey")
	Loop,Parse,k,`n
	{
		If not strlen(A_LoopField)
			continue
		RegExMatch(Menus.GetValue("Hotkey",A_LoopField),"i)\{win:([^\{\}]*)\}",class)
		mzSetHotkey(A_LoopField,class1)
	}
}
; mzDoHotkey() {{{2
; 执行热键
; {mode} 模式
; {sep}  独立模式，与全局模式分离
; {time:0.8} 时间
; {method:1} 方式 >> 当为1时，使用ctrl+insert来复制
; {fast} 快速模式
; {prefunc:xxxx} 在复制前就运行某函数功能
mzDoHotkey:
	mzDoHotkey()
return
mzDoHotkey(){
	Global Mode,Mode_Sep,Menus,mzHotkey
	s := Menus.GetValue("Hotkey",A_ThisHotkey)
	Mode_Sep := RegExMatch(s,"i)\{sep\}")
	If RegExMatch(s,"i)\{prefunc:([^\{\}]*)\}",m)
	{
		If IsFunc(m1)
			%m1%()
	}
	If RegExMatch(s,"i)\{mode:([^\{\}]*)\}",m)
		mode := m1
	If RegExMatch(s,"i)\{fast\}")
		mzInit(0,0)
	If RegExMatch(s,"i)\{time:([\d\.]*)\}",time)
	{
		If RegExMatch(s,"i)\{method:1\}")
			mzInit(time1,1)
		Else
			mzInit(time1,0)
	}
}

; mzSetHotkey(key,string) {{{2
; 设置热键与对应字符串
mzSetHotkey(key,class=""){
	Global mzHotkey
	If Not IsObject(mzHotkey)
		mzHotkey := []
	If Strlen(class)
	{
		mzHotkey[Key] := class
		Hotkey,IfWinActive,ahk_class %class%
	}
	Else
	{
		mzHotkey[key] := true
		Hotkey,IfWinActive
	}
	Hotkey,%key%,mzDoHotkey,on
}
; mzGetHotkey(key) {{{2
; 获取热键与对应字符串
mzGetHotkey(key){
	Global mzHotkey
	return mzHotkey[key]
}
; mzDelHotkey() {{{2
; 删除所有热键
mzDelHotkey(){
	Global mzHotkey
	for key , class in mzHotkey
	{
		If (class <> 1)
			Hotkey,IfWinActive,ahk_class %class%
		Else
			Hotkey,IfWinActive
		Hotkey,%key%,off
	}
	mzHotkey := []
}

; mzGetType() {{{2
;获取选择的类型(0 是文本 , 1 是文件 , 2 是窗口(class))
mzGetType(){
	Global IsFile,SelectType
	Return IsFile ? 1 : RegExMatch(SelectType,"^Class$") ? 2 : 0
}
; mzGetTypeString() {{{2
;获取选择的类型描述( 文本会返回
mzGetTypeString(){
	Global SelectType
	s := mzGetType() 
	If s = 2
		return mzGetSelect()
	return SelectType
}
; mzGetSelect() {{{2
; 获取选择的内容
mzGetSelect(){
	Global iString
	return iString
}
; mzGetText() {{{2
; 获取选择的文本
mzGetText(){
	return iGetText()
}
; mzGetFiles() {{{2
; 获取选择的文件列表 
mzGetFiles(){
	return iGetFiles()
}
; mzSetText(text) {{{2
; 用自定义文本替换选择的内容
mzSetText(text){
	Global IsFile,SaveString
	Isfile := False
	SaveString := Text
}

; mzSetFiles(files) {{{2
; 用自定义的文件列表替换选择的内容
mzSetFiles(files){
	Global IsFile,SaveString
	Isfile := True
	SaveString := Files
}
; mzSetCopyFunction(mFunc,mClass) {{{2
; 设置特定类中使用特定的复制方法
mzSetCopyFunction(mFunc,mClass=""){
	Global mzCopyFunc,mzCopyFuncGlobal
	If Not IsObject(mzCopyFunc)	
		mzCopyFunc := []
	If strlen(mClass)
		mzCopyFunc[mClass] := mFunc
	Else
		mzCopyFuncGlobal := mFunc
}

; mzClipbackup() {{{2
mzClipbackup(){
	Global win_Clip,ClipBak
	win_clip.Snap(ClipBak)
}
; mzClipRestore() {{{2
mzClipRestore(){
	Global win_Clip,ClipBak
	win_clip.Restore(clipBak)
}
; mzClipClear() {{{2
mzClipClear(){
	Global win_Clip
	win_clip.Clear()
}
; mzSetCheckFunction(mFunc) {{{2
; 忘记这个函数要干什么了
mzSetCheckFunction(mFunc){
	Global cFunc
	If Not IsObject(cFunc)
		cFunc := []
	cFunc[mFunc] := True
}
; mzGetCheckFunction(mFunc) {{{2
; 忘记这个函数要干什么了
mzGetCheckFunction(mFunc){
	Global cFunc
	return cFunc[mFunc]
}

; Main Functions 主体函数 {{{1

; ReportChange(f,t) {{{2
; 监听Plugins目录的改变，随时加载插件
ReportChange(f,t){
	;msgbox % f "`n" t 
}
; iCopy(timeout=1,method=1) {{{2
; 使用常规的方法复制，获取内容
iCopy(timeout=1,method=1){
	Global win_clip,IsFile,SaveString
	SaveString := ""
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
; iGetText() {{{2
; 获取选择的文本
iGetText(){
	Global IsFile,SaveString
	If (IsFile = 0 ) And strlen(SaveString)
		return SaveString
}
; iGetFiles() {{{2
; 获取选择的文件列表 
iGetFiles(){
	Global IsFile,SaveString
	If (IsFile = 1) And Strlen(SaveString)
		return SaveString
}
; iGetClass() {{{2
iGetClass(){
	WinGetClass,c,A
	return c
}
; iGetTextType(text) {{{2
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
; iGetFileType(file) {{{2
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
; CreateMenuConfig(time,method) {{{2
; 获取内容后生成菜单配置
CreateMenuConfig(time,method) {
	Global win_clip,iString,Menus,Items,SelectType,iClass,iControl,mzCopyFunc,mzCopyFuncGlobal
	Menus.CheckTime()
	MouseGetPos,,,WinID,iControl
	WinGetClass,iClass,ahk_id %WinID%
	If Time {
		IsFunc(cf := mzCopyFunc[iClass]) ? %cf%(time,method) : IsFunc(mzCopyFuncGlobal) ? %mzCopyFuncGlobal%(time,method) : iCopy(time,method)
		; 文本 ====================
		If (iString := iGetText()) {
			sections := Menus.GetSectionsF(SelectType := iGetTextType(iString))
			Loop,Parse,sections,`n
				If strlen(A_LoopField)
				{
					s := Menus.GetKeyValue(A_LoopField)
					Config .= AddLine(s)
					config .= iSort(s)
				}
			If SelectType <> AnyText
			{
				s := Menus.GetKeyValue("AnyText")
				Config .= AddLine(s)
				config .= iSort(s)
			}
		}
		; 文件 ====================
		Else If (iString := iGetFiles()) {
			sections :=  Menus.GetSectionsF(SelectType := iGetFileType(iString))
			Loop,Parse,sections,`n
				If strlen(A_LoopField)
				{
					s := Menus.GetKeyValue(A_LoopField)
					Config .= AddLine(s)
					config .= iSort(s)
				}
			s := Menus.GetKeyValue("AnyFile")
			Config .= AddLine(s)
			config .= iSort(s)
		}
		Else If Menus.GetValue("Config","noClass")
		{
			SelectType := ""
			iString := ""
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
						Config .= AddLine(s)
						config .= iSort(s)
					}
				s := Menus.GetKeyValue("AnyClass")
				Config .= AddLine(s)
				config .= iSort(s)
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
					Config .= AddLine(s)
					config .= iSort(s)
			}
			s := Menus.GetKeyValue("AnyClass")
			Config .= AddLine(s)
			config .= iSort(s)
	}
	s := Menus.GetKeyValue("Any")
	Config .= AddLine(s)
	config .= iSort(s)
	Return Config
}
; iSort(string) {{{3
; 用于菜单配置的顺序
iSort(string){
	Sort,string , F NumberSort
	return String
}
; NumberSort(n,m) {{{3
; 配套isort使用
NumberSort(n,m){
	a1 := RegExReplace(n,"[^\d]*=.*")
	a2 := RegExReplace(m,"[^\d]*=.*")
	return a1 > a2 ? 1 : a1 < a2 ? -1 : 0 
}
; AddLine(s) {{{3
; 添加分割符
AddLine(s) {
	If strlen(s)
		return "-`n"
}
Is_Menu_Add_Line(){
	Global Menu_Add_Line
}
; CreateMenuBody(string,IsSub=false,config="",type="") {{{2
; 按照CreateMenuConfig获取的配置生成菜单
; 一般只需要用到string 和 IsSub 两个参数即可，后面的 config 和type 为CreateMenuHead()所用
CreateMenuBody(string,IsSub=false,config="",type=""){
	Global win_PUM,iString,Menus,Items,SelectType,win_Menu,Mode,Last_Item
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
		{
			If This_Menu.Prev_Item_Not_Separator()
				This_Menu.Add()
		}
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
			{
				If This_Menu.Prev_Item_Not_Separator()
					This_Menu.Add()
			}
			Else
			{
				params := GetMenuItemParams(Items,ItemName)
				params["name"] := DynItemName(ItemName)
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
				Last_Item := This_Item :=  This_Menu.Add(params)
				If ( Menus.GetKeyValue(Items.GetValue(ItemName,"Sub")) )
					This_Item.SetParams({"submenu":CreateMenuBody(ItemName,Sub:=True)})
			}
		}
	}
	Return This_Menu.handle
}
; Check_Mode(i) {{{3
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
; Check_Class(i) {{{3
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
; Check_Control(i) {{{3
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
; Check_Type(i) {{{3
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
; Check_MultiType(i) {{{3
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
; Check_Path(i) {{{3
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
; Check_Name(i) {{{3
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

; Check_If(i) {{{3
Check_If(i){
	Global Items
	IfFunc := Menus.ReplaceEnv(Items.GetValue(i,"if"))
	If strlen(IfFunc)
		return %IfFunc%()
	return True
}

; LoadMenuFromFunc(m,f) {{{3
; 从函数加载菜单，m为菜单对象，f为函数
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
; DynItemName(String) {{{3
; 动态菜单名
DynItemName(String){
	Global Menus
	String := Menus.ReplaceEnv(String)
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
; DynMenuSetParam(section,key,value) {{{3
; 设置动态菜单参数
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
; CreateMenuHead(config) {{{2
; 创建菜单的首菜单项
CreateMenuHead() {
	Global win_PUM,iString,Menus,Items,SelectType,win_Menu,Lang,languages
	win_Menu := win_PUM.CreateMenu(GetMenuParams(Menus,"Config"))
	If (Not Strlen(SelectType)) And ( Not Strlen(iString))
	{
        GetExtIcon("AnyText",mIconFile,mIconNumber)
		mIcon := mIconFile ":" mIconNumber := mIconNumber > 0 ? mIconNumber - 1 : mIconNumber
		win_Menu.Add({"name":Languages.GetValue(Lang,"None Select"),"uid":"head","icon":mIcon})
		Return
	}

	ItemName := iString
	If SelectType = Class
	{
		WinGet,mIconFile,ProcessPath,ahk_class %iString%
		ItemName := Menus.GetValue("ClassType",iString)
		If Not Strlen(ItemName)
			ItemName := iString
	}
	Else If SelectType = MultiFiles
	{
		;多文件
		ExtObj := []
		iFolderCount := 0
		iFileCount   := 0
		Loop,Parse,iString,`n,`r
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
		If win_Menu.Prev_Item_Not_Separator()
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
		If win_Menu.Prev_Item_Not_Separator()
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
}
; LoadMenuFromOther(m) {{{3
; 选择多文件时进行分类，将获取的分类生成菜单
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
		params["uid"] := type
		m.Add(params)
	}
	return m.handle
}
; Otherinit() {{{3
; 初始化分类列表
Otherinit(){
	Global otherObj,otherObj_Index
	Otherobj := ""
	otherObj_Index := ""
	otherObj_type := ""
}
; OtherFiles() {{{3
OtherFiles(){
	Global otherObj
	return otherObj
}
; otherAdd(i,count) {{{3
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
; CreateMenuTail(config) {{{2
; 创建最后的菜单并显示菜单
CreateMenuTail() {
	Global win_PUM,iString,Menus,Items,SelectType,win_Menu,Languages,Lang,Last_Item
	Hotkey,IfWinActive
	Hotkey,space,mzDefault,on
	Hotkey,tab,OpenConfigGUI,on
	If Menus.GetValue("Config","ShowConfig"){
		If win_Menu.Prev_Item_Not_Separator()
			win_Menu.Add()
			win_Menu.Add({"name":Languages.GetValue(Lang,"Open Config GUI"),"icon":A_ScriptDir "\icons\settings.ico:0","uid":"config"})
	}
	Coordmode,Mouse,Screen
	MouseGetPos,mx,my
	obj := win_Menu.Show(mx,my,"animll")
	If RegExMatch(obj.uid,"i)^Head")
		GoSub,mzDefault
	If RegExMatch(obj.uid,"i)^config")
		GoSub,OpenConfigGUI
	Else
	{
		If SelectType = Multifiles
		{
			Loop,Parse,iString,`n,`r
			{
				If Strlen(A_LoopField){
					If iGetFileType(A_LoopField) == obj.uid
						NewString .= A_LoopField "`n" 
				}
			}
			iString := NewString
		}
		ToExec(Engine(iString,Items.GetValue(obj.name,"string")))
	}
	SelectType := ""
}

; GetMenuItemParams(Conf,Section) {{{2
; 从配置ini文件中获取菜单项的参数
GetMenuItemParams(Conf,Section){
	Global Menus
	params := []
	If Strlen(Tcolor := Conf.GetValue(Section,"Tcolor"))	
		params["tcolor"] :=  Tcolor
	Else
		params["tcolor"] := Strlen(t:=Menus.GetValue("config","Tcolor")) ? t : 0x00
	If Strlen(BGcolor := Conf.GetValue(Section,"BGcolor"))
		params["bgcolor"] :=  BGcolor
	Else
		params["bgcolor"] :=  Strlen(b:=Menus.GetValue("config","BGcolor")) ? b : 0xffffff
	If Strlen(bold := Conf.GetValue(Section,"bold"))
		params["bold"] := bold
	If Strlen(icon := Menus.ReplaceEnv(Conf.GetValue(Section,"icon")))
	{
		If RegExMatch(Icon,"i).png$")
			params["icon"] := icon
		Else
			params["icon"] := RegExMatch(icon,":\d*$") ? icon  : icon ":0"
	}
	If Strlen(break := Conf.GetValue(Section,"break")) 
		params["break"] := break
	return params
}


; GetMenuParams(Conf,Section) {{{2
; 获取菜单参数
GetMenuParams(Conf,Section){
	Global Menus
	params := []
	params["tcolor"]  := Strlen(t:=Menus.GetValue("config","Tcolor")) ? t : 0x00
	params["bgcolor"] :=  Strlen(b:=Menus.GetValue("config","BGcolor")) ? b : 0xffffff
	If Strlen(nocolors := Conf.GetValue(Section,"nocolors"))
		params["nocolors"] := nocolors
	Else
		params["nocolors"] := Menus.GetValue("Config","nocolors")
	If Strlen(noicons := Conf.GetValue(Section,"noicons"))
		params["noicons"] := noicons
	Else
		params["noicons"] := Menus.GetValue("Config","noicons")
	If Strlen(notext := Conf.GetValue(Section,"notext"))
		params["notext"] :=  notext
	Else
		params["notext"] := Menus.GetValue("Config","notext")
	If Strlen(IconsSize := Conf.GetValue(Section,"IconsSize"))
		params["IconsSize"] :=  IconsSize
	Else
		params["IconsSize"] :=  Strlen(s:=Menus.GetValue("Config","IconsSize"))?s:16
	If Strlen(textoffset := Conf.GetValue(Section,"textoffset")) 
		params["textoffset"] := textoffset
	If Strlen(maxheight := Conf.GetValue(Section,"maxheight"))
		params["maxheight"] := maxheight
	;Else
	;	params["maxheight"] := Menus.GetValue("Config","maxheight")
	If Strlen(xmargin := Conf.GetValue(Section,"xmargin"))
		params["xmargin"] := xmargin
	;Else
	;	params["xmargin"] := Menus.GetValue("Config","xmargin")
	If Strlen(ymargin := Conf.GetValue(Section,"ymargin"))
		params["ymargin"] := ymargin
	;Else
	;	params["ymargin"] := Menus.GetValue("Config","ymargin")
	If Strlen(textMargin:= Conf.GetValue(Section,"textMargin")) 
		params["textMargin"] := textMargin
	;Else
	;	params["textMargin"] := Menus.GetValue("Config","textMargin")
	return params
}
; GetPUMParams() {{{2
; 获取全局菜单设置
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
	If Strlen(SelTcolor := Menus.GetValue("config","SelTcolor"))	
		params["Seltcolor"] := SelTcolor 
	If Strlen(SelBGcolor := Menus.GetValue("config","SelBGcolor"))
		params["Selbgcolor"] := SelBGcolor
	If Strlen(SelMethod := Menus.GetValue("config","SelMethod"))	
		params["SelMethod"] := SelMethod
	If Strlen(frameWidth := Menus.GetValue("config","frameWidth"))
		params["frameWidth"] := frameWidth
	return Params
}

; GetExtIcon(ext,ByRef IconFile,ByRef IconNumber) {{{2
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
; PUM_OUT(msg,obj) {{{2
PUM_OUT(msg,obj) {
	Global iString,Items
	if msg = onselect
  	{
		;rect := obj.GetRECT()
		;tooltip,% "Selected: " obj.name,% rect.right,% rect.top
		tooltip
  	}
	If msg = onclose
	{
		tooltip
		Hotkey,IfWinActive
		Hotkey,space,mzDefault,off
		Hotkey,tab,OpenConfigGUI,off
	}
	If msg = onshow
	{
		;tooltip % A_TimeSinceThisHotkey,10,10,10
	}
	If msg = onrbutton
	{
		;rect := obj.GetRECT()
		MouseGetPos,mx,my
		If RegExmatch(obj.uid,"i)^config$")
			Tooltip,打开配置
		Else If RegExmatch(obj.uid,"i)^head$")
			Tooltip,% iString, % mx, % my
		Else
			Tooltip, % Say(Engine(iString,Items.GetValue(Obj.name,"String")))
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
	Tooltip % Tick
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
; ToExec(obj) {{{2
/*
  obj.Method
	method = none       >>> 无操作
	method = macro 		>>> 执行宏
	method = multirun	>>> 多次运行
	method = run  		>>> 单次运行
  obj.String
	run模式下		>>> 直接执行此变量 obj.string = "Notepad.exe"
	multirun模式下	>>> 此变量保存多次运行的行数,如 obj.String = 3
	macro模式下     >>> 此变量不使用
  obj.mode := max	>>> 最大化运行
  obj.mode := min	>>> 最小化运行
  obj.mode := hide	>>> 隐藏运行
  obj.wait := ture  >>> 运行等待
  obj.workingdir  >>> 工作目录
  obj.String1  >>> 配合多次运行使用
  obj.String3  >>> 同上
  obj.ini 	   >>> 配合执行宏使用，保存宏对应的ini文件
  obj.section  >>> 配合执行宏使用，保存宏对应的section段
  obj.clip     >>> 保存内容到剪切板中
*/
ToExec(obj){
	If obj.clip
			ClipBoard := obj.ClipString
	If InStr(obj.Method,"none")
		return
	If InStr(obj.Method,"run")
	{
		Target 	:= obj.String
		Workingdir := obj.Workingdir
		mode := obj.mode " UseErrorLevel"
		If obj.Wait
			RunWait,%Target%,%Workingdir%,%mode%
		Else
			Run,%Target%,%Workingdir%,%mode%
	}
}
; Say(obj) {{{2
Say(obj){
	If not Strlen(Obj.String)
		return "无内容"
	t .= "原语句是:`n" obj.Save "`n"
	If InStr(obj.Method,"run")
	{
		t .= "将会"
		If InStr(obj.mode,"max")
			t .= "以""最大化""的方式"
		If InStr(obj.mode,"min")
			t .= "以""最小化""的方式"
		If InStr(obj.mode,"hide")
			t .= "以""隐藏""的方式"
		t .= "运行以下字符串`n"
	}
	t .= Obj.String
	return t
}
; Engine(string) {{{2
Engine(content,string){
	Global Items,Menus,iString
	ToExecObj := []
	ToExecObj.Method := "run"
	ToExecObj.Save := String
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
			If RegExMatch(switch,"i)\{file[^\{\}]*\}") 
			{
				If RegExMatch(iGetFileType(Content),"i)^(\..*)|(Multifiles)|(Folder)$")
					RtString := fileswitch(content,switch)
				Else
					RtString := ""
			}
			If RegExMatch(switch,"i)\{select[^\{\}]*\}")
				RtString := selectswitch(content,switch)
			If RegExMatch(switch,"i)\{box[^\{\}]*\}",box)
				RtString := boxswitch(box)
			If RegExMatch(switch,"i)\{date[^\{\}]*\}",date)
				RtString := dateswitch(date)
			If RegExMatch(switch,"i)\{func:([^\{\}]*)\}",Func)
			{
				If Isfunc(Func1)
					RtString := %Func1%()
			;	Else
			;		RtString := CheckExtension(func)
			}
			If RegExMatch(switch,"i)\{run:([^\{\}]*)\}",r)
			{
				If RegExMatch(r1,"i)^(max)|(min)|(hide)$")
					ToExecObj.mode := r1
				If RegExMatch(r1,"i)^wait$")
					ToExecObj.wait := True
				If RegExMatch(r1,"i)^none$")
					ToExecObj.method := "none"
				If RegExMatch(r1,"i)^dir=(.*)",d)
					ToExecObj.Workingdir := d1
				RtString := ""
			}
			If RegExMatch(switch,"i)\{save:([^\{\}]*)\}",s)
			{
				If RegExMatch(s1,"^clip$")
				{
					ToExecObj.Clip := True
					ToExecObj.clipString := iString
				}
				RtString := ""
			}
			P1 := Pos + Strlen(switch)
			RunString := SubStr(RunString,1,Pos-1) RtString Substr(RunString,P1)
			P1 := Pos + Strlen(RtString)
		}
		Else
			Break
	}
	ToExecObj.String := RunString
	return ToExecObj
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

; selectswitch(select,switch) {{{3
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

; SksSub_UrlEncode(string, enc="UTF-8") {{{4
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


; fileSwitch(FileList,switch) {{{3
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
; 界面代码 {{{1

LoadGUIMenu:
Menu,Tools,Add
Menu,Tools,Delete
Menu,Tools,Add,% Languages.GetValue(Lang,"Menu Debugger"),Debugger
Menu,Tools,Add,% Languages.GetValue(lang,"Menu class viewer"),Test
Menu,Tools,Add,% Languages.GetValue(lang,"Menu correlation type"),Test
Menu,Help,Add
Menu,Help,Delete
Menu,Help,Add,% Languages.GetValue(lang,"Help topic"),Test
Menu,Help,Add,% Languages.GetValue(lang,"Open HomePage"),Test
Menu,Help,Add
Menu,Help,Add,% Languages.GetValue(Lang,"About"),Test
;Menu,MenuTools,Add
;Menu,MenuTools,Delete
;Menu,MenuTools,Add,% Languages.GetValue(Lang,"Item viewer"),ItemViewer
;Menu,MenuTools,Add
;Menu,MenuTools,Add,% Languages.GetValue(Lang,"item export"),Test
;Menu,MenuTools,Add,% Languages.GetValue(Lang,"item import"),Test
Menu,Config,Add,% Languages.GetValue(Lang,"Menu Option"),OptionGUI
;Menu,Config,Add,% Languages.GetValue(Lang,"Menu Toolbar"),Test
Menu,Config,Add
Menu,Config,Add,% Languages.GetValue(Lang,"Menu Option Backup"),Test
Menu,Config,Add,% Languages.GetValue(Lang,"Menu Option Restore"),Test
Menu,MenuBar,Add
Menu,MenuBar,Delete
Menu,MenuBar,Add,% Languages.GetValue(Lang,"Menu Config"),:Config
Menu,MenuBar,Add,% Languages.GetValue(Lang,"Menu Tools"),:Tools
;Menu,MenuBar,Add,% Languages.GetValue(Lang,"Menu Item"),:MenuTools
Menu,MenuBar,Add,% Languages.GetValue(Lang,"Menu Help"),:Help
return
; Config: {{{2
Config:
Critical
TV_SelectItem := ""
TV_SelectItemID := ""
ImageListID ? IL_Destroy(ImageListID)
ImageListID := IL_Create(10)

GUI,Config:Destroy
GUI,Config:Default
GUI,Config:+Theme +hwndhGUI +LastFound 
GUI,Config:Font,s9 ,Microsoft YaHei
GUI,Config:Menu,MenuBar
;======
GUI,Config:Add,TreeView,x10 y38 w240 h447 altsubmit vMenuTreeView gSelectItem ImageList%ImageListID% hwndhTreeView
mzTreeView := new TreeView(hTreeView)
GUI,Config:Add,Edit,x261 y38 h24 w280
;GUI,Config:Add,Picture,x264 y35 h20 w20,e:\Program Files\MenuZ 2\Icons\Settings.ico
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
;====== 窗体类
GUI,Config:Add,Text,x270 y293,% Languages.GetValue(Lang,"Class")
GUI,Config:Add,DropDownList,x317 y290 w70 h22 R3 Choose1 AltSubmit ,% Languages.GetValue(Lang,"Include") "|" Languages.GetValue(Lang,"Exclude") "|" Languages.GetValue(Lang,"Regexp") "|"
GUI,Config:Add,Edit,x397 y290 w180 h24
;====== 控件名
GUI,Config:Add,Text,x270 y323,% Languages.GetValue(Lang,"Control")
GUI,Config:Add,DropDownList,x317 y320 w70 h22 R3 Choose1 AltSubmit ,% Languages.GetValue(Lang,"Include") "|" Languages.GetValue(Lang,"Exclude") "|" Languages.GetValue(Lang,"Regexp") "|"
GUI,Config:Add,Edit,x397 y320 w180 h24
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
GUI,Config:Add,Button,x390 y455 w70 h24 Hide,% Languages.GetValue(Lang,"Button OK")
GUI,Config:Add,Button,x470 y455 w70 h24 gConfigClose,% Languages.GetValue(Lang,"Button Close")
GUI,Config:Add,Button,x550 y455 w70 h24 gConfigSave Disabled,% Languages.GetValue(Lang,"Button Save")
GUI,Config:Add,Button,x549 y70 h24 w70 gIconSelect, % Languages.GetValue(Lang,"Button Icon")
GUI,Config:Add,Button,x549 y38 h24 w70 gStyleSelect,% languages.GetValue(Lang,"Item Style")
GUI,Config:Add,Edit,x261 y70 h24 w280 ReadOnly
GUI,Config:Add,StatusBar
;====== 其它辅助
GUI,Config:Add,Picture,x577 y285 w36 h36 gSetico2,%Cross_CUR_File%
GUI,Config:Add,Picture,x577 y315 w36 h36 gSetico3,%Cross_CUR_File%
SB_SetParts(250)
sci.Add(hGui, 260, 102, 360, 90, A_ScriptDir "\Lib\SciLexer.dll")
sci.SetWrapMode(true) ; this removes the horizontal scrollbar
sci.SetMarginWidthN(1, 0) ; this removes the left margin
sci.StyleSetFont(STYLE_DEFAULT,"Microsoft YaHei")
sci.StyleSetSize(STYLE_DEFAULT, 10)
sci.StyleClearAll()
sci.notify := "Notify"
sci.StyleSetFore(SCE_AHKL_LABEL, 0xEE0000)
sci.SetCodePage(936)
;sci.STYLESETFONT(SCE_AHKL_LABEL,"Microsoft YaHei")
;sci.STYLESETSIZE(SCE_AHKL_LABEL,10)
sci.STYLESETBOLD(SCE_AHKL_LABEL,1)
sci.StyleSetFore(SCE_AHKL_VAR, 0xAA2288)
;sci.STYLESETFONT(SCE_AHKL_VAR,"Microsoft YaHei")
;sci.STYLESETSIZE(SCE_AHKL_VAR,10)
sci.StyleSetFore(SCE_AHKL_BUILTINVAR, 0x0000EE)
;sci.STYLESETFONT(SCE_AHKL_BUILTINVAR,"Microsoft YaHei")
;sci.STYLESETSIZE(SCE_AHKL_BUILTINVAR,10)
sci.AUTOCSETSEPARATOR(10)
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
IL_Add(hILSec,A_ScriptDir "\Icons\Itemdown.png",0)
IL_Add(hILSec,A_ScriptDir "\Icons\Itemup.png",0)
IL_Add(hILSec,A_ScriptDir "\Icons\SubItem.png",0)
IL_Add(hILSec,A_ScriptDir "\Icons\sep.png",0)
IL_Add(hILSec,A_ScriptDir "\Icons\moveup.png",0)
IL_Add(hILSec,A_ScriptDir "\Icons\movedown.png",0)
IL_Add(hILSec,A_ScriptDir "\Icons\Delete.png",0)
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
Toolbar_Insert(hToolbarItem,Languages.GetValue(Lang,"insert content") ",1,,dropdown,201")
;Toolbar_Insert(hToolbarItem,"ssss,3,,dropdown")
GUi,Config:Show,w630 ,% "MenuZ " languages.GetValue(lang,"editor")
If not Strlen(SelectType)
	GoSub,SelectTypeGUI
;根据选择创建菜单项
Critical,off
return

;=============
Test:
return
;=============
; ConfigClose: {{{2
ConfigClose:
	GUI,Config:Destroy
return

;=============
; ConfigSave: {{{2
ConfigSave:
	ConfigSave()
return
ConfigSave(){
	Global TV_SelectItem,TV_SelectItemID,Menus,Items,EditType,SCI,TextColor,BackgroundColor,Bold,IconsSize
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	GUIControlGet,newstring,,Edit1
	If not strlen(NewString)
		return
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
	If not strlen(idx)
		idx := 1
	;If TV_SelectItem <> %NewString%
	;{
		;If not Items.GetKeys(NewString)
		;{
			LoopKeys := Items.GetKeys(TV_SelectItem)
			Loop,Parse,LoopKeys,`n
			{
			If not strlen(A_LoopField)
				continue
			Items.iniWrite(NewString,A_LoopField,Items.IniRead(TV_SelectItem,A_LoopField))
			}
			;GUIControlGet,icon,,Edit10
			;Items.iniWrite(NewString,"icon",icon)
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
		;}
	;}
	Menus.iniWrite(EditType,idx,NewString)
	CreateTVItemSub(NewString,nid:=TV_Modify(TV_SelectItemID,TV_GetItemIcon(NewString),NewString))
	TV_SetItemStyle(nid,NewString)
	;Items.Read()
	;Menus.Read()
	;msgbox % TV_SelectItem
	;msgbox % Menus.GetValue(EditType,idx)
}
; OptionGUI: {{{2
OptionGUI:
GUI,OptionGUI:Destroy
GUI,OptionGUI:Default
GUI,OptionGUI:+theme 
GUI,OptionGUI:Font,s9,Microsoft YaHei
GUI,OptionGUI:Add,Button, x425 y375 w80 h26 gOptionGUI_Destroy Default,% Languages.GetValue(Lang,"Button Close")
GUI,OptionGUI:Add,Button, x290 y375 w120 h26 gOptionGUI_Reload ,% Languages.GetValue(Lang,"Button Reload")
GUI,OptionGUI:Add,Tab,x5 y5 w500 h360 , % Languages.GetValue(Lang,"OptionGUI Tab")
GUI,OptionGUI:Tab,2
GUI,OptionGUI:Add,ListView,x15 y40 w480 h150 gOptionGUI_SetHotkey AltSubmit,% Languages.GetValue(Lang,"Hotkey") "|" Languages.GetValue(Lang,"Action")
GUI,OptionGUI:Add,Text,x20 y202 w30 h24, % Languages.GetValue(Lang,"Hotkey")
GUI,OptionGUI:Add,Hotkey,x55 y200 w140 h24
GUI,OptionGUI:Add,GroupBox,x15 y235 w480 h120,% Languages.GetValue(Lang,"Action")
GUI,OptionGUI:Add,Text,x25 y262 w70 h24, % Languages.GetValue(Lang,"Outtime")
GUI,OptionGUI:Add,Text,x160 y262 w15 h24, % Languages.GetValue(Lang,"second")
GUI,OptionGUI:Add,Edit,x100 y258 w50 h24 
GUI,OptionGUI:Add,CheckBox,x25 y290 h24 gOptionGUI_SetHotkey_FastMode,% Languages.GetValue(Lang,"FastMode")
GUI,OptionGUI:Add,Radio,x25 y320 w70 h24 checked,Ct&rl+C
GUI,OptionGUI:Add,Radio,x100 y320 w80 h24,Ctrl+&Insert
GUI,OptionGUI:Add,Text,x200 y260 w90 h24,% Languages.GetValue(Lang,"Menu Mode")
GUI,OptionGUI:Add,Edit,x275 y258 w95 h24
GUI,OptionGUI:Add,Button,x376 y258 w30 h24,+
GUI,OptionGUI:Add,CheckBox,x410 y258 w82 h24,% Languages.GetValue(Lang,"Menu Mode Sep")
GUI,OptionGUI:Add,Text,x200 y322 w100 h24,% Languages.GetValue(Lang,"Prefunc")
GUI,OptionGUI:Add,Edit,x275 y318 w180 h24
GUI,OptionGUI:Add,Button,x459 y318 w30 h24,+
GUI,OptionGUI:Add,Button,x320 y200 w80 h24 gOptionGUI_SetHotkey_Save,% Languages.GetValue(Lang,"Button Save")
GUI,OptionGUI:Add,Button,x415 y200 w80 h24 gOptionGUI_SetHotkey_Delete,% Languages.GetValue(Lang,"Button Delete")
GUI,OptionGUI:Add,Text,x200 y292 w90 h24,% Languages.GetValue(Lang,"win class")
GUI,OptionGUI:Add,Edit,x275 y288 w180 h24
GUI,OptionGUI:Add,Picture,x459 y284 w36 h36 gSetico,%Cross_CUR_File%
; 常规选项
GUI,OptionGUI:Tab,1
GUI,OptionGUI:Add,GroupBox,x15 y40 w480 h90 , % Languages.GetValue(Lang,"Menu")
GUI,OptionGUI:Add,CheckBox,x30 y60 w400 h26 gOptionGUI_ShowConfig, % Languages.GetValue(Lang,"ShowConfig")
GUI,OptionGUI:Add,CheckBox,x30 y90 w400 h26 gOptionGUI_NoClass, % Languages.GetValue(Lang,"noclass")
GUI,OptionGUI:Add,GroupBox,x15 y140 w480 h90 ,% Languages.GetValue(Lang,"Icon")
GUI,OptionGUI:Add,CheckBox,x30 y160 w400 h26 gOptionGUI_NoIcons, % Languages.GetValue(Lang,"noicons")

GUI,OptionGUI:Add,Text,x30 y194 w400 h26 , % Languages.GetValue(Lang,"IconsSize") " :"
GUI,OptionGUI:Add,DropDownList,x110 y190 w100 h26 R5 AltSubmit gOptionGUI_IconsSize, 16x16|32x32|48x48|64x64

GUI,OptionGUI:Add,GroupBox,x15 y240 w480 h115,% Languages.GetValue(Lang,"color")

GUI,OptionGUI:Add,Text,x30 y270 w80 h26,% Languages.GetValue(Lang,"Tcolor") " :"
GUI,OptionGUI:Add,Text,x90 y268 w100 h22 Border center gOptionGUI_RemoveColor_T,
GUI,OptionGUI:Add,Button,x194 y267 w30 h24 gOptionGUI_ChangeColor_T,>>

GUI,OptionGUI:Add,Text,x260 y270 w80 h26,% Languages.GetValue(Lang,"BGcolor") " :"
GUI,OptionGUI:Add,Text,x320 y268 w100 h22 Border center gOptionGUI_RemoveColor_BG,
GUI,OptionGUI:Add,Button,x424 y267 w30 h24 gOptionGUI_ChangeColor_BG,>>


GUI,OptionGUI:Add,Text,x30 y300 w80 h26,% Languages.GetValue(Lang,"SelTcolor") " :"
GUI,OptionGUI:Add,Text,x90 y298 w100 h22 Border center gOptionGUI_RemoveColor_SelT,
GUI,OptionGUI:Add,Button,x194 y297 w30 h24 gOptionGUI_ChangeColor_SelT,>>

GUI,OptionGUI:Add,Text,x260 y300 w80 h26,% Languages.GetValue(Lang,"SelBGcolor") " :"
GUI,OptionGUI:Add,Text,x320 y298 w100 h22 Border center gOptionGUI_RemoveColor_SelBG,
GUI,OptionGUI:Add,Button,x424 y297 w30 h24 gOptionGUI_ChangeColor_SelBG,>>

GUI,OptionGUI:Add,Text,x30 y330 w400 h22 ,% Languages.GetValue(lang,"needReload")
GUI,OptionGUI:Show,w510 h414
OptionGUI_LoadGeneral()
OptionGUI_LoadHotkey()
return

OptionGUI_Reload:
	mzReload()
return
OptionGUI_Destroy:
	GUI,OptionGUI:Destroy
return
; OptionGUI_ShowConfig() {{{4
OptionGUI_ShowConfig:
	OptionGUI_ShowConfig()
Return
OptionGUI_ShowConfig(){
	Global Menus
	GUI,OptionGUI:Default
	GUIControlGet,v,,Button13
	Menus.iniWrite("Config","ShowConfig",v)
}
; OptionGUI_NoClass() {{{4
OptionGUI_NoClass:
	OptionGUI_NoClass()
return
OptionGUI_NoClass(){
	Global Menus
	GUI,OptionGUI:Default
	GUIControlGet,v,,Button14
	Menus.iniWrite("Config","noclass",v)
}

; OptionGUI_Noicons() {{{4
OptionGUI_NoIcons:
	OptionGUI_NoIcons()
return
OptionGUI_Noicons(){
	Global Menus
	GUI,OptionGUI:Default
	GUIControlGet,v,,Button16
	Menus.iniWrite("Config","noicons",v)
}

; OptionGUI_IconsSize() {{{4
OptionGUI_IconsSize:
	OptionGUI_IconsSize()
return
OptionGUI_IconsSize(){
	Global Menus
	GUI,OptionGUI:Default
	GUIControlGet,v,,ComboBox1
	Menus.iniWrite("Config","IconsSize",16*v)
}

; OptionGUI_ChangeColor() {{{4
OptionGUI_ChangeColor_T:
	OptionGUI_ChangeColor(10)
return
OptionGUI_ChangeColor_BG:
	OptionGUI_ChangeColor(12)
return
OptionGUI_ChangeColor_SelT:
	OptionGUI_ChangeColor(14)
return
OptionGUI_ChangeColor_SelBG:
	OptionGUI_ChangeColor(16)
return
OptionGUI_ChangeColor(idx){
	Global Menus
	GUI,OptionGUI:Default
	GUI,OptionGUI:+hwndh
	If idx = 10
		Menus.iniWrite("Config","Tcolor",BackgroundColor := Rgb(Dlg_Color(auto.GetValue("color","Default"),h)))
	If idx = 12
		Menus.iniWrite("Config","BGcolor",BackgroundColor := Rgb(Dlg_Color(auto.GetValue("color","Default"),h)))
	If idx = 14
		Menus.iniWrite("Config","SelTcolor",BackgroundColor := Rgb(Dlg_Color(auto.GetValue("color","Default"),h)))
	If idx = 16
		Menus.iniWrite("Config","SelBGcolor",BackgroundColor := Rgb(Dlg_Color(auto.GetValue("color","Default"),h)))
	GUIControlGet,h,Hwnd,Static%idx%
	iBackgroundColor := strlen(BackgroundColor) ? SubStr("000000" . RegExReplace(BackgroundColor,"i)^0x"),-5) : ""
	If iBackgroundColor
	{
		GUIControl,,Static%idx%
		CtlColors.Change(h,iBackgroundColor,iTextColor)
	}
	Else
		GUIControl,,Static%idx%,% Languages.GetValue(Lang,"nullcolor")
}

; OptionGUI_RemoveColor(idx) {{{4
OptionGUI_RemoveColor_T:
	OptionGUI_RemoveColor(10)
return
OptionGUI_RemoveColor_BG:
	OptionGUI_RemoveColor(12)
return
OptionGUI_RemoveColor_SelT:
	OptionGUI_RemoveColor(14)
return
OptionGUI_RemoveColor_SelBG:
	OptionGUI_RemoveColor(16)
return
OptionGUI_RemoveColor(idx){
	Global Menus,Languages,Lang
	GUI,OptionGUI:Default
	If idx = 10
		Menus.iniWrite("Config","Tcolor","")
	If idx = 12
		Menus.iniWrite("Config","BGcolor","")
	If idx = 14
		Menus.iniWrite("Config","SelTcolor","")
	If idx = 16
		Menus.iniWrite("Config","SelBGcolor","")
	GUIControlGet,h,Hwnd,Static%idx%
	GUIControl,,Static%idx%,% Languages.GetValue(Lang,"nullcolor")
	CtlColors.Detach(h)
}

; OptionGUI_LoadGeneral() {{{4
OptionGUI_LoadGeneral(){
	Global Menus,Languages,Lang
	GUI,OptionGUI:Default
	GUIControl,,Button13,% Strlen(v:=Menus.GetValue("config","ShowConfig"))?v:0
	GUIControl,,Button14,% Strlen(v:=Menus.GetValue("config","noclass"))?v:0
	GUIControl,,Button16,% Strlen(v:=Menus.GetValue("config","noicons"))?v:0
	If (Mod(k:=Menus.GetValue("config","IconsSize"),16)) or (not k)
		GUIControl,Choose,ComboBox1,1
	Else
		GUIControl,Choose,ComboBox1,% Round(k/16)
	GUIControlGet,h,Hwnd,Static10
	BackgroundColor := Menus.GetValue("Config","Tcolor")
	iBackgroundColor := strlen(BackgroundColor) ? SubStr("000000" . RegExReplace(BackgroundColor,"i)^0x"),-5) : ""
	If iBackgroundColor
	{
		GUIControl,,Static10
		CtlColors.Attach(h,iBackgroundColor,iTextColor)
	}
	Else
		GUIControl,,Static10,% Languages.GetValue(Lang,"nullcolor")
	GUIControlGet,h,Hwnd,Static12
	BackgroundColor := Menus.GetValue("Config","BGcolor")
	iBackgroundColor := strlen(BackgroundColor) ? SubStr("000000" . RegExReplace(BackgroundColor,"i)^0x"),-5) : ""
	If iBackgroundColor
	{
		GUIControl,,Static12
		CtlColors.Attach(h,iBackgroundColor,iTextColor)
	}
	Else
		GUIControl,,Static12,% Languages.GetValue(Lang,"nullcolor")

	GUIControlGet,h,Hwnd,Static14
	BackgroundColor := Menus.GetValue("Config","SelTcolor")
	iBackgroundColor := strlen(BackgroundColor) ? SubStr("000000" . RegExReplace(BackgroundColor,"i)^0x"),-5) : ""
	If iBackgroundColor
	{
		GUIControl,,Static14
		CtlColors.Attach(h,iBackgroundColor,iTextColor)
	}
	Else
		GUIControl,,Static14,% Languages.GetValue(Lang,"nullcolor")
	GUIControlGet,h,Hwnd,Static16
	BackgroundColor := Menus.GetValue("Config","SelBGcolor")
	iBackgroundColor := strlen(BackgroundColor) ? SubStr("000000" . RegExReplace(BackgroundColor,"i)^0x"),-5) : ""
	If iBackgroundColor
	{
		GUIControl,,Static16
		CtlColors.Attach(h,iBackgroundColor,iTextColor)
	}
	Else
		GUIControl,,Static16,% Languages.GetValue(Lang,"nullcolor")
}
; OptionGUI_LoadHotkey() {{{4
OptionGUI_LoadHotkey(){
	Global Menus
	GUI,OptionGUI:Default
	GUI,OptionGUI:ListView,SysListView321
	LV_ModifyCol(1,80)
	Keys := Menus.GetKeys("Hotkey")
	Loop,Parse,Keys,`n
	{
		If not strlen(A_LoopField)
			continue
		LV_Add("",A_LoopField,Menus.GetValue("Hotkey",A_LoopField))
	}
}
; OptionGUI_SetHotkey {{{4
OptionGUI_SetHotkey:
	OptionGUI_SetHotkey(A_GuiEvent,A_EventInfo)
return
OptionGUI_SetHotkey(GuiEvent,EventInfo){
	Global Hotkey_Selected
	GUI,OptionGUI:Default
	GUI,OptionGUI:ListView,SysListView321
	If GuiEvent = Normal
	{
		Hotkey_Selected := EventInfo
		LV_GetText(key,EventInfo,1)
		GuiControl,,msctls_hotkey321,% Key
		LV_GetText(s,EventInfo,2)
		Mode_Sep := RegExMatch(s,"i)\{sep\}")
		If RegExMatch(s,"i)\{prefunc:([^\{\}]*)\}",m)
			GuiControl,,Edit3,% m1
		Else
			GuiControl,,Edit3,
		If RegExMatch(s,"i)\{mode:([^\{\}]*)\}",m)
			GuiControl,,Edit2,% m1
		Else
			GuiControl,,Edit2
		If RegExMatch(s,"i)\{fast\}"){
			GuiControl,,Button4,1
			GuiControl,,Edit1
			GuiControl,Disable,Edit1
		}
		Else {
			GuiControl,,Button4,0
			GuiControl,Enable,Edit1
		}
		If RegExMatch(s,"i)\{time:([\d\.]*)\}",time)
			GuiControl,,Edit1,% time1
		Else
			GuiControl,,Edit1
		If RegExMatch(s,"i)\{method:1\}")
			GuiControl,,Button6,1
		Else
			GuiControl,,Button5,1
		If RegExMatch(s,"i)\{win:([^\{\}]*)\}",class)
			GuiControl,,Edit4,% class1
		Else
			GuiControl,,Edit4
	}
}
; OptionGUI_SetHotkey_fastMode {{{4
OptionGUI_SetHotkey_fastMode:
	OptionGUI_SetHotkey_fastMode()
return
OptionGUI_SetHotkey_fastMode(){
	GUI,OptionGUI:Default
	GUIControlGet,v,,Button4
	If v
	{
		GuiControl,,Edit1
		GuiControl,Disable,Edit1
	}
	Else
		GuiControl,Enable,Edit1
}
; OptionGUI_SetHotkey_Delete {{{4
OptionGUI_SetHotkey_Delete:
	OptionGUI_SetHotkey_Delete()
return
OptionGUI_SetHotkey_Delete(){
	Global Hotkey_Selected,Menus
	GUI,OptionGUI:Default
	GUI,OptionGUI:ListView,SysListView321
	LV_GetText(key,Hotkey_Selected,1)
	If LV_Delete(Hotkey_Selected)
		Menus.IniDelete("Hotkey",Key)
	mzDelHotkey()
	mzLoadHotkey()
}
; OptionGUI_SetHotkey_Save {{{4
OptionGUI_SetHotkey_Save:
	OptionGUI_SetHotkey_Save()
return
OptionGUI_SetHotkey_Save(){
	Global Hotkey_Selected,Menus
	GUI,OptionGUI:Default
	GUI,OptionGUI:ListView,SysListView321
	LV_GetText(key,Hotkey_Selected,1)
	GuiControlGet,newKey,,msctls_hotkey321
	GuiControlGet,second,,Edit1
	GuiControlGet,fastmode,,Button4
	GuiControlGet,method,,Button6
	GuiControlGet,menu_mode,,Edit2
	GuiControlGet,menu_mode_sep,,Button8
	GuiControlGet,prefunc,,Edit3
	GuiControlGet,class,,Edit4
	If fastmode
		string .= "{fast} "
	Else
	{
		If second
			string .= "{time:" second "} "
	}
	If method
		string .= "{method:1} "
	If strlen(menu_mode)
		string .= "{mode:" menu_mode "} "
	If menu_mode_sep
		string .= "{sep} "
	If strlen(prefunc)
		string .= "{prefunc:" prefunc "} "
	If Strlen(class)
		string .= "{win:" class "} "
	If not strlen(string)
		string := "{time:1}"
	If ( key == newKey )
		LV_Modify(Hotkey_Selected,"",key,string)
	Else
		LV_Add("",newkey,string)
	Menus.IniWrite("Hotkey",newkey,string)
	mzDelHotkey()
	mzLoadHotkey()
}

; ItemViewer: {{{2
ItemViewer:
Critical
GUI,ItemViewer:Destroy
GUI,ItemViewer:Default
GUI,ItemViewer:+theme 
GUI,ItemViewer:Font,s9,Microsoft YaHei
GUI,ItemViewer:Add,ListView,altsubmit NoSort x5 y10 w140 h405 gItemViewer_SelectINIFile,% Languages.GetValue(Lang,"Config file")
GUI,ItemViewer:Add,ListView,altsubmit x150 y10 w215 h370 gItemViewer_SelectSection Checked,% Languages.GetValue(Lang,"Config Section")
GUI,ItemViewer:Add,ListView,altsubmit x370 y200 w360 h180 gItemViewer_ModifyEnv,% Languages.GetValue(Lang,"Config Env")
GUI,ItemViewer:Add,Edit,x370 y10 w360 R1 h24 ReadOnly
GUI,ItemViewer:Add,Picture,x370 y40 w24 h24 border
GUI,ItemViewer:Add,Picture,x374 y44 w16 h16
GUI,ItemViewer:Add,Edit,x400 y40 w330 R1 h24 ReadOnly
GUI,ItemViewer:Add,Edit,x370 y70 w360 h125 ReadOnly
GUI,ItemViewer:Add,Button,x150 y387 w100 h24 gItemViewer_Clean,% Languages.GetValue(Lang,"Button Clean")
GUI,ItemViewer:Add,Button,x370 y387 w80 h24 gItemViewer_Delete,% Languages.GetValue(Lang,"Button Delete")
GUI,ItemViewer:Add,Button,x460 y387 w80 h24 gItemViewer_InsertItem,% Languages.GetValue(Lang,"Button Insert Menu")
GUI,ItemViewer:Add,Button,x550 y387 w90 h24 gItemViewer_InsertSubItem,% Languages.GetValue(Lang,"Button Insert Sub Menu")
GUI,ItemViewer:Add,Button,x650 y387 w80 h24 gItemViewer_Destroy,% Languages.GetValue(Lang,"Button Close")
GUI,ItemViewer:Show,h420 w735,% Languages.GetValue(Lang,"Menu Library")
GUI,ItemViewer:ListView,SysListView321
LV_Add("",Languages.GetValue(Lang,"Custom Config"))
Loop, % A_ScriptDir "\Config\Library\*.*"
{
	If RegExMatch(A_LoopFileName,"i)\.ini$")
		LV_Add("",RegExReplace(A_LoopFileName,"i)\.ini$"))
}

GUI,ItemViewer:ListView,SysListView322
Sections := Items.GetSections()
Loop,Parse,Sections,`n
{
	If not strlen(A_LoopField)
		continue
	LV_Add("",A_LoopField)
}
GUI,ItemViewer:ListView,SysListView323
LV_ModifyCol(1,120)
LV_ModifyCol(2,400)
LV_ModifyCol(3,50)
ItemViewer_Icons := IL_Create(10)  ; 创建加载 10 个小图标的图像列表.
LV_SetImageList(ItemViewer_Icons)
IL_Add(ItemViewer_Icons,"shell32.dll",220)
IL_Add(ItemViewer_Icons,"shell32.dll",297)
Critical,off
return
; ItemViewer_InsertItem: {{{3
ItemViewer_InsertItem:
	ItemViewer_InsertItem()
return
; ItemViewer_InsertSubItem: {{{3
ItemViewer_InsertSubItem:
	ItemViewer_InsertItem(sub:=True)
return
ItemViewer_InsertItem(sub=false){
	Global TV_SelectItemID,Items,Menus
	If sub ;插入子级菜单
		id := TV_SelectItemID
	Else ;插入同级菜单
		id := TV_GetParent(TV_SelectItemID)
	GUI,ItemViewer:Default
	GUI,Config:ListView,SysTreeView323
	If (cnt := LV_GetCount())
	{
		Loop,%cnt%
		{
			LV_GetText(k,A_Index,1)
			LV_GetText(v,A_Index,2)
			LV_GetText(s,A_Index,3)
			If s
				Menus.iniWrite("Env",k,v)
		}
	}
	GUI,ItemViewer:Default
	GUI,ItemViewer:ListView,SysListView321
	lib_idx := LV_GetNext(0,"focused")
	If lib_idx > 1
	{
		LV_GetText(ThisLib,Lib_idx)
		libINI := new ini(A_ScriptDir "\Config\Library\" ThisLib ".ini")
		
	}
	GUI,ItemViewer:Default
	GUI,ItemViewer:ListView,SysListView322
	idx := 0
	Loop
	{
		idx := LV_GetNext(idx,"Checked")
		If not idx
			break
		LV_GetText(text,idx)
		CheckText .= Text "`n"
	}
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	Loop,Parse,CheckText,`n
	{
		If not Strlen(A_LoopField)
			continue
		If lib_idx > 1
		{
			;Msgbox % libINI.GetKeyValue(A_LoopField)
			s := A_LoopField
			keys := LibINI.GetKeys(s)
			Loop,Parse,Keys,`n
			{
				If not Strlen(A_LoopField)
					continue
				Items.iniWrite(s,A_LoopField,LibINI.GetValue(s,A_LoopField))
			}
			Items.Read()
		}
		CreateTVItemSub(A_LoopField,nid := TV_Add(A_LoopField,id,TV_GetItemIcon(A_LoopField) " Expand"))
		TV_SetItemStyle(nid,A_LoopField)
	}
	If sub
		TV_WriteMenu(TV_GetSection(),id,IsSub:=true)
	Else
		TV_WriteMenu(TV_GetSection(),id,IsSub:=False)
	
}
; ItemViewer_SelectINIFile: {{{3
ItemViewer_SelectINIFile:
	Critical
	If A_GuiEvent = I
	{
		GUI,ItemViewer:Default
		GUI,ItemViewer:ListView,SysListView321
		LV_GetText(i,A_EventInfo)
		;If RegExMatch(i,"^" ToMatch(Languages.GetValue(Lang,"Custom Config")) "$")
		If A_EventInfo < 2
		{
			GUI,ItemViewer:ListView,SysListView322
			LV_Delete()
			Sections := Items.GetSections()
			Loop,Parse,Sections,`n
			{
				If not strlen(A_LoopField)
					continue
				LV_Add("",A_LoopField)
			}
			GUI,ItemViewer:ListView,SysListView323
			LV_Delete()
		}
		Else
		{
			GUI,ItemViewer:ListView,SysListView322
			LV_Delete()
			Library := new INI(A_ScriptDir "\Config\Library\" i ".ini")
			Sections := Library.GetSections()
			Loop,Parse,Sections,`n
			{
				If (Not strlen(A_LoopField)) OR ( RegExMatch(A_LoopField,"i)setting"))
					continue
				LV_Add("",A_LoopField)
			}
			GUI,ItemViewer:ListView,SysListView323
			LV_Delete()
			envs := Library.GetKeys("Setting")
			Loop,Parse,envs,`n
			{
				If not Strlen(A_LoopField)
					continue
				If Strlen(v:=Menus.GetValue("Env",A_LoopField))
				{
					LV_Add("icon2",A_LoopField,v,1)
					continue
				}
				envs_obj := Library.GetValue("Setting",A_LoopField)
				If RegExMatch(envs_obj,"i)^((file)|(folder)):")
					LV_Add("icon1",A_LoopField,RegExReplace(envs_obj,"i)^((file)|(folder)):"),0)
				Else
					LV_Add("icon2",A_LoopField,envs_obj,1)
			}
		}
	ItemViewer_CheckButtonDelete()
	ItemViewer_CheckButtonInsert()
	}
	Critical,off
return
; ItemViewer_SelectSection: {{{3
ItemViewer_SelectSection:
	Critical
	If A_GuiEvent = I
	{
		Change := ErrorLevel
		file := Number := ""
		GUI,ItemViewer:Default
		GUI,ItemViewer:ListView,SysListView321
		idx := LV_GetNext()
		LV_GetText(i,idx?idx:1)
		GUI,ItemViewer:ListView,SysListView322
		LV_GetText(s,A_EventInfo)
		If  idx < 2
		{
			options := Items.GetValue(s,"string")
			icon := Items.GetValue(s,"icon")
		}
		Else
		{
			Library := new INI(A_ScriptDir "\Config\Library\" i ".ini")
			options := Library.GetValue(s,"string")
			icon := Library.GetValue(s,"icon")
		}
		If Strlen(icon){
			If RegExMatch(icon,":[-\d]*$"){
				pos := RegExMatch(Icon,":[^:]*$")
				file   := Menus.ReplaceEnv(SubStr(icon,1,pos-1))
				Number := (number := substr(icon,pos+1)) > 0 ? Number + 1 : Number
			}
			Else
				file := Menus.ReplaceEnv(icon)
		}
		GUIControl,,Static2, *icon%Number%  *w16 *h16 %file%
		GUIControl,,Edit1,% s
		GUIControl,,Edit2,% icon
		GUIControl,,Edit3,% options
		If Not RegExMatch(Change,"i)C")
			return
	ItemViewer_CheckButtonDelete()
	ItemViewer_CheckButtonInsert()
	}
	Critical,off
return

; ItemViewer_Delete {{{3
ItemViewer_Delete:
	ItemViewer_Delete()
return
ItemViewer_Delete(){
	Critical
	Global Items
	GUI,ItemViewer:Default
	GUI,ItemViewer:ListView,SysListView321
	idx := LV_GetNext("Focused")
	If idx < 2
	{
		GUI,ItemViewer:ListView,SysListView322
		i := LV_GetNext("Focused")	
		LV_GetText(v,i)
		Items.iniDelete(v)
		LV_Delete(i)
	}
	Else
		return
	Critical,off
}
; ItemViewer_CheckButtonInsert() {{{3
ItemViewer_CheckButtonInsert(){
	Critical
	GUI,ItemViewer:Default
	GUI,ItemViewer:ListView,SysListView322
	If LV_GetNext(0,"Checked")
	{
		GUI,ItemViewer:ListView,SysListView323
		If (cnt := LV_GetCount())
		{
			Loop,%cnt%
			{
				LV_GetText(v,A_Index,3)
				If not v
				{
					GUIControl,Disable,Button3
					GUIControl,Disable,Button4
					return
				}
			}
		}
		GUIControl,Enable,Button3
		GUIControl,Enable,Button4
		Return
	}
	GUIControl,Disable,Button3
	GUIControl,Disable,Button4
	Critical,off
}
; ItemViewer_CheckButtonDelete() {{{3
ItemViewer_CheckButtonDelete(){
	GUI,ItemViewer:Default
	GUI,ItemViewer:ListView,SysListView321
	idx := LV_GetNext("Focused")
	If idx < 2
		GUIControl,Enable ,Button2
	Else
		GUIControl,Disable,Button2
}
; ItemViewer_Clean {{{3
ItemViewer_Clean:
	ItemViewer_Clean()
return
ItemViewer_Clean(){
	Global Items,Menus
	vaild := []
	m := Menus.GetSections()
	Loop,Parse,m,`n
	{
		If not strlen(A_LoopField)
			continue
		k := Menus.GetKeys(s:=A_LoopField)
		If RegExMatch(s,"i)^((config)|(env)|(texttype)|(classtype)|(filetype)|(hotkey))$")
			continue
		Loop,Parse,k,`n
		{
			If not strlen(A_LoopField)
				continue
			vaild[Menus.GetValue(s,A_LoopField)] := true
		}
	}
	i := Items.GetSections()
	Loop,Parse,i,`n
	{
		If not strlen(A_LoopField)
			continue
		If not strlen(Items.GetValue(A_LoopField,"String"))
		{
			Items.iniDelete(A_LoopField)
			msgbox % A_LoopField
		}
	}
	GUI,ItemViewer:Default
	GUI,ItemViewer:ListView,SysListView322
	Sections := Items.GetSections()
	LV_Delete()
	Loop,Parse,Sections,`n
	{
		If not strlen(A_LoopField)
			continue
		LV_Add("",A_LoopField)
	}
}

; ItemViewer_ModifyEnv {{{3
ItemViewer_ModifyEnv:
	ItemViewer_ModifyEnv(A_GuiEvent,A_EventInfo)
return
ItemViewer_ModifyEnv(GuiEvent,EventInfo){
	Critical
	If GuiEvent = DoubleClick
	{
		GUI,ItemViewer:Default
		GUI,ItemViewer:ListView,SysListView321
		LV_GetText(i,LV_GetNext("Focused"))
		Library := new INI(A_ScriptDir "\Config\Library\" i ".ini")
		GUI,ItemViewer:ListView,SysListView323
		LV_GetText(s,EventInfo)
		v := Library.GetValue("Setting",s)
		If RegExMatch(v,"i)^file:")
		{
			FileSelectFile,f,S3,,% RegExReplace(v,"i)^file:")
			If FileExist(f)
			LV_Modify(EventInfo,"icon2",s,f,1)
		}
		Else If RegExMatch(v,"i)^Folder:")
		{
			FileSelectFolder,f,,0,% RegExReplace(v,"i)^folder:")
			If FileExist(f)
			LV_Modify(EventInfo,"icon2",s,f,1)
		}
		Else 
			return
		ItemViewer_CheckButtonInsert()
	}
	Critical,off
}
; ItemViewer_Destroy {{{3
ItemViewer_Destroy:
	GUI,ItemViewer:Destroy
return
;=============
; StyleSelect: {{{2
StyleSelect:
	If not strlen(TV_SelectItem){
		msgbox % Languages.GetValue(Lang,"Please Select Item")
		return
	}
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	TV_GetSelection()
	GUI,StyleGUI:Destroy
	GUI,StyleGUI:Default
	GUI,StyleGUI:+Theme +hwndhStyleGUI
	GUI,StyleGUI:Font,s9,Microsoft YaHei
	GUI,StyleGUI:Add,Text,   x10 y10 w320 h24 center hwndhStyle border,% Languages.GetValue(Lang,"Select Style")
	GUI,StyleGUI:Add,Button, x10 y56 w100 h30 gStyleSelect_Text,% Languages.GetValue(Lang,"Button TextColor")
	GUI,StyleGUI:Add,Button, x120 y56 w100 h30 gStyleSelect_Background,% Languages.GetValue(Lang,"Button BackgroundColor")
	GUI,StyleGUI:Add,CheckBox, x250 y60 w70 h24 gStyleSelect_Bold,% Languages.GetValue(Lang,"Button Bold")
	GUI,StyleGUI:Add,Text,Border x10 y100 w320 h1
	GUI,StyleGUI:Add,Button, x10 y110 w100 h24 gStyleSelect_Default,% Languages.GetValue(Lang,"Button Default")
	GUI,StyleGUI:Add,Button, x120 y110 w100 h24 gStyleSelect_OK ,% Languages.GetValue(Lang,"Button OK")
	GUI,StyleGUI:Add,Button, x230 y110 w100 h24 gStyleSelect_Close,% Languages.GetValue(Lang,"Button Close")
	;GUI,StyleGUI:Add,Radio,x30 y70 w70 Disabled,16x16(&L)
	;GUI,StyleGUI:Add,Radio,x30 y95 w70 Disabled,32x32(&K)
	If Items.GetValue(TV_SelectItem,"Bold")
	{
		GUI,StyleGUI:Font,s9 Bold,Microsoft YaHei
		GUIControl,,Button3,1
		GuiControl,Font,Static1
	}
	Else
		GUIControl,,Button3,0
	If Strlen(Items.GetValue(TV_SelectItem,"Sub"))
	{
		GUIControl,Enable,Button7
		GUIControl,Enable,Button8
		IconsSize := Items.GetValue(TV_SelectItem,"IconsSize")
		If IconsSize = 32
			GUIControl,,Button8,1
		Else
			GUIControl,,Button7,1
	}
	GUI,StyleGUI:Show,,% Languages.GetValue(Lang,"Select Style")
	TextColor :=  Items.GetValue(TV_SelectItem,"Tcolor")
	iTextColor := strlen(TextColor) ? SubStr("000000" . RegExReplace(TextColor,"i)^0x"),-5) : ""
	BackgroundColor := Items.GetValue(TV_SelectItem,"BGcolor")
	iBackgroundColor := strlen(BackgroundColor) ? SubStr("000000" . RegExReplace(BackgroundColor,"i)^0x"),-5) : ""
	CtlColors.Attach(hStyle,iBackgroundColor,iTextColor)
return
; StyleSelect_Text: {{{3
StyleSelect_Text:
	TextColor := Rgb(Dlg_Color(auto.GetValue("color","Default"),hStyleGUI))
	iTextColor := SubStr("000000" . RegExReplace(TextColor,"i)^0x"),-5)
	CtlColors.Change(hStyle,iBackgroundColor,iTextColor)
	If strlen(TV_SelectItem)
		Items.IniWrite(TV_SelectItem,"TColor",TextColor)
return
; StyleSelect_Background: {{{3
StyleSelect_Background:
	BackgroundColor := Rgb(Dlg_Color(auto.GetValue("color","Default"),hStyleGUI))
	iBackgroundColor := SubStr("00000" . RegExReplace(BackgroundColor,"i)^0x"),-5)
	CtlColors.Change(hStyle,iBackgroundColor,iTextColor)
	If strlen(TV_SelectItem)
		Items.IniWrite(TV_SelectItem,"BGColor",BackgroundColor)
return
; StyleSelect_OK: {{{3
StyleSelect_OK:
	GUI,StyleGUI:Default
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	If Items.GetValue(TV_SelectItem,"Bold")
		TV_Modify(TV_SelectItemID,"Bold")
	Else
		TV_Modify(TV_SelectItemID,"-Bold")
	If strlen(TextColor) or Strlen(BackgroundColor)
		mzTreeView.TV_Color({hwnd:TV_SelectItemID,fore:rgb_bgr_swap(TextColor),Back:rgb_bgr_swap(BackgroundColor)})
	Else
		mzTreeView.TV_Color({hwnd:TV_SelectItemID})
	GUI,StyleGUI:Destroy
	GuiControl, +Redraw, MenuTreeView 
return
; StyleSelect_Close: {{{3
StyleSelect_Close:
	GUI,StyleGUI:Destroy
return
; StyleSelect_Default: {{{3
StyleSelect_Default:
	GUI,StyleGUI:Default
	GUI,StyleGUI:Font,s9 Normal,Microsoft YaHei
	GUIControl,Font,Static1
	GUIControl,,Button3,0
	GUIControl,,Button7,1
	CtlColors.Detach(hStyle)
	TextColor := BackgroundColor := ""
	Bold := 0
	Items.IniWrite(TV_SelectItem,"TColor","")
	Items.IniWrite(TV_SelectItem,"BGColor","")
	Items.IniWrite(TV_SelectItem,"Bold","")
return
; StyleSelect_Bold: {{{3
StyleSelect_Bold:
	GUI,StyleGUI:Default
	GUIControlGet,bold,,Button3
	If Bold
	{
		GUI,StyleGUI:Font,s9 Bold,Microsoft YaHei
		GUIControl,Font,Static1
		Items.INIWrite(TV_SelectItem,"Bold",1)
	}
	Else
	{
		GUI,StyleGUI:Font,s9 Normal,Microsoft YaHei
		GUIControl,Font,static1
		Items.INIWrite(TV_SelectItem,"Bold",0)
	}
return
; Dlg_Color(Color,hwnd) {{{2
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
; Debugger: {{{2
Debugger:
	GUI,Debugger:Destroy
	GUI,Debugger:Default
	GUI,Debugger:+theme +hwndhDebuggerGUI
	GUI,Debugger:Font,s9,Microsoft YaHei
	GUI,Debugger:Add,Edit,x10 y10 w500 h100
	GUI,Debugger:Add,Button,x100 y118 w100 gDebugger_Open_Clipboard,% Languages.GetValue(lang,"Button ClipBoard")
	GUI,Debugger:Add,Button,x220 y118 w100 gDebugger_Test,% Languages.GetValue(Lang,"Button Debug")
	GUI,Debugger:Add,Edit,x10 y260 w500 h300 ReadOnly
	DebugSCI := new scintilla
	DebugSCI.Add(hDebuggerGUI,10,154,500,90, A_ScriptDir "\Lib\SciLexer.dll")
	Debugsci.SetWrapMode(true) ; this removes the horizontal scrollbar
	Debugsci.SetMarginWidthN(1, 0) ; this removes the left margin
	Debugsci.StyleSetFont(STYLE_DEFAULT,"Microsoft YaHei")
	Debugsci.StyleSetSize(STYLE_DEFAULT, 10)
	Debugsci.StyleClearAll()
	Debugsci.notify := "Notify"
	Debugsci.StyleSetFore(SCE_AHKL_LABEL, 0xEE0000)
	Debugsci.SCI_STYLESETFONT(SCE_AHKL_LABEL,"Microsoft YaHei")
	Debugsci.SCI_STYLESETSIZE(SCE_AHKL_LABEL,10)
	Debugsci.STYLESETBOLD(SCE_AHKL_LABEL,1)
	Debugsci.StyleSetFore(SCE_AHKL_VAR, 0xAA2288)
	Debugsci.SCI_STYLESETFONT(SCE_AHKL_VAR,"Microsoft YaHei")
	Debugsci.SCI_STYLESETSIZE(SCE_AHKL_VAR,10)
	Debugsci.StyleSetFore(SCE_AHKL_BUILTINVAR, 0x0000EE)
	Debugsci.SCI_STYLESETFONT(SCE_AHKL_BUILTINVAR,"Microsoft YaHei")
	Debugsci.SCI_STYLESETSIZE(SCE_AHKL_BUILTINVAR,10)
	DebugSCI.CLEARCMDKEY(Asc("Q")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("S")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("W")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("G")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("F")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("H")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("H")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("E")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("R")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("O")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("P")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("K")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("N")+(SCMOD_CTRL<<16))
	DebugSCI.CLEARCMDKEY(Asc("B")+(SCMOD_CTRL<<16))
	DebugSCI.USEPOPUP(0)
	GUI,Debugger:Show
return
Debugger_Open_Clipboard:
	GUI,Debugger:Default
	GUIControl,,Edit1,%ClipBoard%
return
Debugger_Test:
	GUI,Debugger:Default
	GUIControlGet,content,,Edit1
	DebugSCI.GetText(DebugSCI.GetLength()+1,string)
	t := Engine(content,String)
	GUIControl,,Edit2,% t.String
return
;=============
; IconSelect: {{{2
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
GUI,IconSelect:Add,Edit,x10  y380  w500 r1, `%ICONS`%
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
Icon_Search_stop := False
Icon_Search()
return
; GUISize: {{{3
IconSelectGUISize:
SelectTypeGUIGUISize:
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
		GUI,SelectTypeGUI:Default
		GUI,SelectTypeGUI:ListView,SysListView321
		ControlGetPos , , , w, ,SysListView321,ahk_id %hTypeGUI%
		LV_ModifyCol(1,w/2-100)
		LV_ModifyCol(2,w/2+100)
	}
return
; IconSelect_Clear: {{{3
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
; IconSelect_OK: {{{3
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
; Button_Icon_Cancel: {{{3
Button_Icon_Cancel:
	GUI,IconSelect:Destroy
return
; Button_Icon_Open: {{{3
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
; Button_Icon_Search: {{{3
Button_Icon_Search:
	If Icon_Search_stop
		Settimer,Icon_Search,20
	Else
		Icon_Search_stop := True
return
; Icon_Search: {{{3
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
; GoReport: {{{3
GoReport:
GUI,IconSelect:Default
GUI,IconSelect:ListView,SysListView321
GuiControl,+Report, SysListView321
Return

; GoIcons: {{{3
GoIcons:
GUI,IconSelect:Default
GUI,IconSelect:ListView,SysListView321
GuiControl,+Icon, SysListView321
Return

; GoSmallIcons: {{{3
GoSmallIcons:
GUI,IconSelect:Default
GUI,IconSelect:ListView,SysListView321
GuiControl,+IconSmall,SysListView321
Return

; GoList: {{{3
GoList:
GUI,IconSelect:Default
GUI,IconSelect:ListView,SysListView321
GuiControl,+List, SysListView321
Return
; SelectTypeGUI: {{{2
SelectTypeGUI:
GUI,SelectTypeGUI:Destroy
GUI,SelectTypeGUI:Default
GUI,SelectTypeGUI:+theme +hwndhTypeGUI +Resize
GUI,SelectTypeGUI:Font,s9,Microsoft YaHei
GUI,SelectTypeGUI:Add,ListView,x5 y10 w460 h380 Grid gSelectTypeGUI_OK nosort altsubmit,% languages.GetValue(Lang,"Type|Describe")
GUI,SelectTypeGUI:Add,Button,x5 y400 w80 gSelectTypeGUI_Add,% Languages.GetValue(Lang,"Add Type")
GUI,SelectTypeGUI:Add,Button,x95 y400 w80 Disabled gSelectTypeGUI_Edit,% Languages.GetValue(Lang,"Edit Type")
GUI,SelectTypeGUI:Add,Button,x185 y400 w80 Disabled gSelectTypeGUI_Delete,% Languages.GetValue(Lang,"Delete Type")
;GUI,SelectTypeGUI:Add,Button,x300 y400 w80 ,% Languages.GetValue(Lang,"Button Ok")
GUI,SelectTypeGUI:Add,Button,x385 y400 w80 gSelectTypeGUI_Cancel,% Languages.GetValue(Lang,"Button Cancel")
GUI,SelectTypeGUI:show,w470 h440,% Languages.GetValue(Lang,"Select Type")
LV_ModifyCol(1,"150")
LV_ModifyCol(2,"304")
LoadSelectType()
;WinMove,ahk_id %hTypeGUI%,,,,481
return

; SelectTypeGUI_Delete: {{{3
SelectTypeGUI_Delete:
GUI,SelectTypeGUI:Default
Number := LV_GetNext()
LV_GetText(SelectTypeGUI_Edit_Type,Number)
MsgBox, 36,, % languages.GetValue(Lang,"do you delete this type") " [ " SelectTypeGUI_Edit_Type " ] ?"
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
LV_Delete(Number)
return


; SelectTypeGUI_Add: {{{3
SelectTypeGUI_Add:
GUI,SelectTypeGUI:Default
GUI,SelectTypeGUI:ListView,SysListView321
Number := LV_GetNext()
LVIndex := GetLVIndex()
GUI,SelectTypeGUI_Add:Destroy
GUI,SelectTypeGUI_Add:Default
GUI,SelectTypeGUI_Add:Font,s9,Microsoft YaHei
GUI,SelectTypeGUI_Add:+theme +Owner%hTypeGUI%
GUI,SelectTypeGUI_Add:Add,Text,w280 x10 y10 h24,% languages.GetValue(lang,"Type") ":"
GUI,SelectTypeGUI_Add:Add,Edit,w280 x10 y30 h24
GUI,SelectTypeGUI_Add:Add,Text,w280 x10 y60 h24,% languages.GetValue(lang,"Describe")  languages.GetValue(lang,"Describe2")":"
GUI,SelectTypeGUI_Add:Add,Edit,w280 x10 y78 h24
GUI,SelectTypeGUI_Add:Add,DropDownList,w280 x10 y120 h24 Choose1 r4 altsubmit gSelectTypeGUI_Add_Other,% Languages.GetValue(lang,"other|FileType|TextType|ClassType")
GUI,SelectTypeGUI_Add:Add,Button,x10 y160  w135 h26 Default gSelectTypeGUI_Add_OK , % Languages.GetValue(Lang,"Button Ok")
GUI,SelectTypeGUI_Add:Add,Button,x156 y160 w135 h26 gSelectTypeGUI_Add_Cancel, % Languages.GetValue(Lang,"Button Cancel")
GUI,SelectTypeGUI_Add:show,w300 h200,% Languages.GetValue(Lang,"Add Type")
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
SelectTypeGUI_Add_Cancel:
	GUI,SelectTypeGUI_Add:Destroy
return
; SelectTypeGUI_Add_OK: {{{3
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
	GUI,SelectTypeGUI_Add:Destroy
	idx := GetLVIndex()
	If c = 1
		LV_Add("Select vis Focus",type,desc)
	Else If c = 2
		LV_Insert(idx["File Type"],"Select vis Focus",type,desc)
	Else If c = 3
		LV_Insert(idx["Class Type"],"Select vis Focus",type,desc)
	Else If c = 4
		LV_Insert(idx["other Type"],"Select vis Focus",type,desc)
}
; SelectTypeGUI_Add_other: {{{3
SelectTypeGUI_Add_other:
GUI,SelectTypeGUI_Add:Default
GUIControlGet,C,,ComboBox1
If c = 1
{
	GUIControl,,Edit2
	GUIControl,Disabled,edit2
}
Else
	GUIControl,Enable,edit2
return

; SelectTypeGUI_Edit: {{{3
SelectTypeGUI_Edit:
GUI,SelectTypeGUI:Default
GUI,SelectTypeGUI:ListView,SysListView321
Number := LV_GetNext()
LV_GetText(SelectTypeGUI_Edit_Type,Number)
LV_GetText(SelectTypeGUI_Edit_Desc,Number,2)
LVIndex := GetLVIndex()
GUI,SelectTypeGUI_Edit:Destroy
GUI,SelectTypeGUI_Edit:Default
GUI,SelectTypeGUI_Edit:Font,s9,Microsoft YaHei
GUI,SelectTypeGUI_Edit:+theme +Owner%hTypeGUI%
GUI,SelectTypeGUI_Edit:Add,Text,w280 x10 y10 h24,% languages.GetValue(lang,"Type") ":"
GUI,SelectTypeGUI_Edit:Add,Edit,w280 x10 y30 h24 , % SelectTypeGUI_Edit_Type
GUI,SelectTypeGUI_Edit:Add,Text,w280 x10 y60 h24,% languages.GetValue(lang,"Describe") languages.GetValue(lang,"Describe2") ":"
GUI,SelectTypeGUI_Edit:Add,Edit,w280 x10 y78 h24 , % SelectTypeGUI_Edit_Desc
GUI,SelectTypeGUI_Edit:Add,DropDownList,w280 x10 y120 h24 r4 altsubmit gSelectTypeGUI_Edit_other,% Languages.GetValue(lang,"other|FileType|TextType|ClassType")
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
; SelectTypeGUI_Edit_other: {{{3
SelectTypeGUI_Edit_other:
GUI,SelectTypeGUI_Edit:Default
GUIControlGet,C,,ComboBox1
If c = 1
{
	GUIControl,,Edit2
	GUIControl,Disabled,edit2
}
Else
	GUIControl,Enable,edit2
return
; SelectTypeGUI_Edit_OK: {{{3
SelectTypeGUI_Edit_OK:
GUI,SelectTypeGUI_Edit:Default
GUIControlGet,SelectTypeGUI_Edit_New_Type,,Edit1
GUIControlGet,SelectTypeGUI_Edit_New_Desc,,Edit2
GUIControlGet,SelectTypeGUI_Edit_New_Choose,,ComboBox1
; choose = 1 >> 未定义类型 
; choose = 2 >> 文本类型  texttype
; choose = 3 >> 文件类型  filetype
; choose = 4 >> 窗口类型  classtype

; 如果新旧两个类型名不同，则迁移旧类型到新类型中
If SelectTypeGUI_Edit_New_Type <> %SelectTypeGUI_Edit_Type%
{
	old := Menus.GetKeys(SelectTypeGUI_Edit_Type)
	Loop,Parse,old,`n
	{
		If not Strlen(A_LoopField)
			continue
		Menus.iniWrite(SelectTypeGUI_Edit_New_Type,A_LoopField,Menus.GetValue(SelectTypeGUI_Edit_Type,A_LoopField))
	}
	Menus.IniDelete(SelectTypeGUI_Edit_Type)
}
; 先把原先的类型删除
If SelectTypeGUI_Edit_Choose = 2
	Menus.IniDelete("TextType",SelectTypeGUI_Edit_Type)
Else If SelectTypeGUI_Edit_Choose = 3
	Menus.IniDelete("FileType",SelectTypeGUI_Edit_Type)
Else If SelectTypeGUI_Edit_Choose = 4
	Menus.IniDelete("ClassType",SelectTypeGUI_Edit_Type)
;添加新的类型
If SelectTypeGUI_Edit_New_Choose = 2
	Menus.iniWrite("TextType",SelectTypeGUI_Edit_New_Type,SelectTypeGUI_Edit_New_Desc)
Else If SelectTypeGUI_Edit_New_Choose = 3
	Menus.iniWrite("FileType",SelectTypeGUI_Edit_New_Type,SelectTypeGUI_Edit_New_Desc)
Else If SelectTypeGUI_Edit_New_Choose = 4
	Menus.iniWrite("ClassType",SelectTypeGUI_Edit_New_Type,SelectTypeGUI_Edit_New_Desc)
; 修改原先的类型
GUI,SelectTypeGUI_Edit:Destroy
GUI,SelectTypeGUI:Default
GUI,SelectTypeGUI:ListView,SysTreeView321
If SelectTypeGUI_Edit_New_Choose <> %SelectTypeGUI_Edit_Choose%
{
	LV_Delete(Number)
	idx := GetLVIndex()
	If SelectTypeGUI_Edit_New_Choose = 1
		LV_Add("Select vis Focus",SelectTypeGUI_Edit_New_Type,SelectTypeGUI_Edit_New_Desc)
	Else If SelectTypeGUI_Edit_New_Choose = 2
		LV_Insert(idx["File Type"],"Select vis Focus",SelectTypeGUI_Edit_New_Type,SelectTypeGUI_Edit_New_Desc)
	Else If SelectTypeGUI_Edit_New_Choose = 3
		LV_Insert(idx["Class Type"],"Select vis Focus",SelectTypeGUI_Edit_New_Type,SelectTypeGUI_Edit_New_Desc)
	Else If SelectTypeGUI_Edit_New_Choose = 4
		LV_Insert(idx["other Type"],"Select vis Focus",SelectTypeGUI_Edit_New_Type,SelectTypeGUI_Edit_New_Desc)
}
Else
	LV_Modify(Number,"Select",SelectTypeGUI_Edit_New_Type,SelectTypeGUI_Edit_New_Desc)
return

SelectTypeGUI_Edit_Cancel:
GUI,SelectTypeGUI_Edit:Destroy
return


; SelectTypeGUI_OK: {{{3
SelectTypeGUI_OK:
	SelectTypeGUI_OK()
return
SelectTypeGUI_OK(){
	Global EditType,LVIndex
	LVIndex := GetLVIndex()
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
	If A_GuiEvent = I
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

; ConfigGUI_check(EditType) {{{3
ConfigGUI_check(EditType){
	Global Menus
	GUI,Config:Default
	If RegExMatch(EditType,"^(Any)|(AnyFile)|(AnyText)|(AnyClass)$"){
		GUIControl,Enable,ComboBox1
		GUIControl,Enable,Edit2
		GUIControl,Enable,ComboBox2
		GUIControl,Enable,Edit3
		GUIControl,Enable,ComboBox3
		GUIControl,Enable,Edit4
		GUIControl,Enable,ComboBox4
		GUIControl,Enable,Edit5
		GUIControl,Enable,ComboBox5
		GUIControl,Enable,Edit6
		GUIControl,Enable,ComboBox6
		GUIControl,Enable,Edit7
	}
	Else If RegExMatch(EditType,"i)^(\.[\w\|\.]*$)|(Folder)|(Drive)|(NoExt)|(UnKnown)$"){
		GUIControl,Disable,ComboBox1
		GUIControl,Disable,Edit2
		GUIControl,Disable,ComboBox2
		GUIControl,Disable,Edit3
		GUIControl,Enable,ComboBox3
		GUIControl,Enable,Edit4
		GUIControl,Enable,ComboBox4
		GUIControl,Enable,Edit5
		GUIControl,Enable,ComboBox5
		GUIControl,Enable,Edit6
		GUIControl,Enable,ComboBox6
		GUIControl,Enable,Edit7
	}
	Else If RegExMatch(EditType,"i)^MultiFiles$")
	{
		GUIControl,Disable,ComboBox1
		GUIControl,Disable,Edit2
		GUIControl,Enable,ComboBox2
		GUIControl,Enable,Edit3
		GUIControl,Enable,ComboBox3
		GUIControl,Enable,Edit4
		GUIControl,Enable,ComboBox4
		GUIControl,Enable,Edit5
		GUIControl,Enable,ComboBox5
		GUIControl,Enable,Edit6
		GUIControl,Disable,ComboBox6
		GUIControl,Disable,Edit7
	}
	Else
	{
		GUIControl,Disable,ComboBox1
		GUIControl,Disable,Edit2
		GUIControl,Disable,ComboBox2
		GUIControl,Disable,Edit3
		GUIControl,Enable,ComboBox3
		GUIControl,Enable,Edit4
		GUIControl,Enable,ComboBox4
		GUIControl,Enable,Edit5
		GUIControl,Enable,ComboBox5
		GUIControl,Enable,Edit6
		GUIControl,Enable,ComboBox6
		GUIControl,Enable,Edit7
	}
	IfWinExist,ahk_class %EditType%
	{
		GUIControl,Disable,ComboBox1
		GUIControl,Disable,Edit2
		GUIControl,Disable,ComboBox2
		GUIControl,Disable,Edit3
		GUIControl,Disable,ComboBox3
		GUIControl,Disable,Edit4
		GUIControl,Enable,ComboBox4
		GUIControl,Enable,Edit5
		GUIControl,Disable,ComboBox5
		GUIControl,Disable,Edit6
		GUIControl,Disable,ComboBox6
		GUIControl,Disable,Edit7
	}
	Else If strlen(Menus.GetValue("ClassType",EditType)) Or EditType = UnKnownClass
	{
		GUIControl,Disable,ComboBox1
		GUIControl,Disable,Edit2
		GUIControl,Disable,ComboBox2
		GUIControl,Disable,Edit3
		GUIControl,Disable,ComboBox3
		GUIControl,Disable,Edit4
		GUIControl,Enable,ComboBox4
		GUIControl,Enable,Edit5
		GUIControl,Disable,ComboBox5
		GUIControl,Disable,Edit6
		GUIControl,Disable,ComboBox6
		GUIControl,Disable,Edit7
	}

}
; SelectTypeGUI_Cancel: {{{3
SelectTypeGUI_Cancel:
GUI,SelectTypeGUI:Destroy
GUI,Config:+LastFound
WinActivate
return

; OpenConfigGUI: {{{2
OpenConfigGUI:
	OpenConfigGUI()
return
OpenConfigGUI(Type=""){
	Global SelectType,EditType,iString,languages,Lang,hGUI
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


; LoadSelectType(){ {{{2
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
; GetLVIndex() {{{3
GetLVIndex(){
	idx := []
	idx["InSide Type"] := 1
	idx["Text Type"] := 12
	GUI,SelectTypeGUI:Default
	GUI,SelectTypeGUI:ListView,SysTreeView321
	Loop % LV_GetCount() - 12
	{
		LV_GetText(type,A_Index+12)
		LV_GetText(Desc,A_Index+12,2)
		If RegExMatch(Type,"i)^=.*file.*=$") And RegExMatch(Desc,"^=*$")
			idx["File Type"] := A_index+12
		If RegExMatch(Type,"i)^=.*class.*=$") And RegExMatch(Desc,"^=*$")
			idx["class Type"] := A_index+12
		If idx["File Type"] And idx["class Type"] And RegExMatch(Type,"i)^=.*=$") And RegExMatch(Desc,"^=*$")
			idx["other Type"] := A_index+12
	}
	return idx
}

; ToolbarItem(hCtrl,Event,Txt,Pos,ID) {{{2
ToolbarItem(hCtrl,Event,Txt,Pos,ID){
	Global Languages,Lang
	;Toolbar_DeleteButton(hCtrl)
	Global SCI
	If Event = Click
	{
		If ID = 201
		{
			GoSub,ToolbarItem_InsertEnv	
		}
	}
	If Event = Menu
	{
		If ID = 201
		{
			controlGetPos,x,y,,,,ahk_id %hCtrl%
			rect := Toolbar_GetRect(hCtrl,Pos)
			Loop,Parse,Rect,%A_Space%
			{
				If A_Index = 1
					x := x + A_LoopField
				If A_Index = 4
					y := y + A_LoopField
			}
			Menu,ToolbarItem_Insert,Add
			Menu,ToolbarItem_Insert,DeleteAll
			Menu,ToolbarItem_Insert,Add,% Languages.GetValue(Lang,"insert env"), ToolbarItem_InsertEnv
			Menu,ToolbarItem_Insert,Add,% Languages.GetValue(Lang,"insert file"), ToolbarItem_InsertFile
			Menu,ToolbarItem_Insert,Add,% Languages.GetValue(Lang,"insert folder"), ToolbarItem_InsertFolder
			Menu,ToolbarItem_Insert,Show,%x%,%y%
		}
	}
	
}

; ToolbarItem_InsertEnv {{{3
ToolbarItem_InsertEnv:
	ToolbarItem_InsertEnv()
return
ToolbarItem_InsertEnv(){
	Global Languages,Menus,Lang
	GUI,Env:Destroy
	GUI,Env:Default
	GUI,Env:+Theme +hwndhGUI +LastFound 
	GUI,Env:Font,s9 ,Microsoft YaHei
	GUI,Env:Add,ListView,x5 y10 w500 h400 AltSubmit gToolbarItem_InsertEnv_Event,% Languages.GetValue(Lang,"Env Title")
	GUI,Env:Add,Edit,x5 y420 w410 h24
	GUI,Env:Add,Edit,x5 y455 w410 r3
	GUI,Env:Add,Button,x425 y420 w80 h26,% Languages.GetValue(Lang,"Button Open")
	GUI,Env:Add,Button,x425 y455 w80 h26,% Languages.GetValue(Lang,"Button Save")
	GUI,Env:Add,Button,x425 y488 w80 h26,% Languages.GetValue(Lang,"Button Close")
	GUI,Env:Listview,SysListView321
	LV_ModifyCol(1,"100")
	envs := Menus.GetKeys("Env")
	Loop,Parse,envs,`n
	{
		If not strlen(A_LoopField)
			continue
		LV_Add("",A_LoopField,Menus.GetValue("Env",A_LoopField))
	}
	GUI,Env:Show,w510,% Languages.GetValue(Lang,"insert env")
}
ToolbarItem_InsertEnv_Event:
	If A_GuiEvent = normal
	{
		GUI,Env:Default
		GUI,Env:Listview,SysListView321
		LV_GetText(title,A_EventInfo,1)
		LV_GetText(content,A_EventInfo,2)
		GUIControl,,Edit1,%title%
		GUIControl,,Edit2,%content%
	}
	If A_GuiEvent = DoubleClick
	{
		GUI,Env:Default
		GUI,Env:Listview,SysListView321
		LV_GetText(title,A_EventInfo,1)
		GUI,Env:Destroy
		title := "`%" title "`%"
		SCI.INSERTTEXT(SCI.GETCURRENTPOS(),title)
	}
return
; ToolbarItem_InsertFile {{{3
ToolbarItem_InsertFile:
	tips := Languages.GetValue(Lang,"Box file")
	FileSelectFile,file,s3,%rootdir%,%tips%
	SCI.INSERTTEXT(SCI.GETCURRENTPOS(),file)
return
; ToolbarItem_InsertFolder {{{3
ToolbarItem_InsertFolder:
	FileSelectFolder,folder
	SCI.INSERTTEXT(SCI.GETCURRENTPOS(),folder)
return






; ToolbarMenu(hCtrl,Event,Txt,Pos,ID) {{{2
ToolbarMenu(hCtrl,Event,Txt,Pos,ID){
	If Event = Click
	{
		If ID = 101
			GoSub,SelectTypeGUI
		If ID = 102
			GoSub,ItemViewer
		If ID = 103
			ConfigGUI_AddMenuItem(Direct:=0,Sub:=0)
		If ID = 104
			ConfigGUI_AddMenuItem(Direct:=1,Sub:=0)
		If ID = 105
			ConfigGUI_AddMenuItem(Direct:=0,Sub:=1)
		If ID = 106
			ConfigGUI_AddMenuItem(1,0,1)
		If ID = 107
			ConfigGUI_MoveMenuItem(Up:=False)
		If ID = 108
			ConfigGUI_MoveMenuItem(Down:=True)
		If ID = 109
			ConfigGUI_DeleteMenu()
	}
}

; SelectItem {{{2
SelectItem:
	SelectItem()
return
SelectItem(id=0) {
	Global Items,Menus,sci,TV_SelectItem,TV_SelectItemID
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	If A_GuiEvent = RightClick
		GoSub,SelectTypeGUI
	If A_GuiEvent = Normal
	{
		TV_Modify(A_EventInfo,"select")
		TV_GetText(item,A_EventInfo)
		If Item = -
			return
		SelectItem_Load(A_EventInfo)
	}
}
; SelectItem_Load(id) {{{3
; 加载选择的菜单项
SelectItem_Load(id){
	Global Items,Menus,sci,TV_SelectItem,TV_SelectItemID,TextColor,BackgroundColor,Bold
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	TV_GetText(i,id)
	TV_SelectItem := i
	TV_SelectItemID := id
	GUIControl,,Edit1, % i
	GUIControl,,Edit10, % Items.GetValue(i,"Icon")
	string := Items.GetValue(i,"String")
	TextColor := Items.GetValue(i,"tcolor")
	Bold := Items.GetValue(i,"Bold")
	BackgroundColor := Items.GetValue(i,"bgcolor")
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
}
StrPutVar(string, ByRef var, encoding)
{
    ; 确定容量.
    VarSetCapacity( var, StrPut(string, encoding)
        ; StrPut 返回字符数, 但 VarSetCapacity 需要字节数.
        * ((encoding="utf-16"||encoding="cp1200") ? 2 : 1) )
    ; 复制或转换字符串.
    return StrPut(string, &var, encoding)
}
; =========================================
; 添加菜单项
; ConfigGUI_AddMenuItem(Direct=1,Sub=0,sep=0) {{{2
ConfigGUI_AddMenuItem(Direct=1,Sub=0,sep=0)
{
	Global Items,Menus,EditType,ppp,languages,Lang,mzTreeView,TV_SelectItemID,TV_SelectItem,MenuTreeView
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	GuiControl, -Redraw, MenuTreeView 
	If Not EditType
	{
		GoSub,SelectTypeGUI
		return
	}
	;GUIControlGet,NewItem,,Edit1
	;Random,r,10000,99999
	If sep
		NewItem := "-"
	Else
		;NewItem := languages.GetValue(Lang,"Menu") " - " r " - " 
		InputBox,NewItem
	If not strlen(NewItem)
		return
	; 获取当前选择的菜单所在级别的所有菜单
	If not TV_GetSelection() ; 如果当前没有选择，自动获取最后一行
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

	If ( s := Items.GetValue(Section,"sub")) ;如果当前选择的是子菜单的话，还需要转换为MenuZ.ini中的内容
		Section := s

	; ==========================================
	; 获取当前级别菜单的总数 : MaxIdx
	; 添加子菜单
	If Sub
	{
		ParentID := SelectID
		TV_GetText(Section,SelectID)
		If Section = -
		{
			GuiControl, +Redraw, MenuTreeView 
			return
		}
		Items.iniWrite(Section,"Sub",Section)
		nid := TV_Add(NewItem,SelectID,"Select icon9999")
	}
	Else
	{
		If Menus.GetKeyCount(Section)
		{
			If Direct ; 向下添加菜单
				nid := TV_Add(NewItem,ParentID,SelectID " Select " TV_GetItemIcon(NewItem) " Expand")
			Else ; 向上添加菜单
			{
				If ( PrevID := TV_GetPrev(SelectID) )
					nid := TV_Add(NewItem,ParentID,PrevID " Select " TV_GetItemIcon(NewItem) " Expand")
				Else
					nid := TV_Add(NewItem,ParentID,"First Select" TV_GetItemIcon(NewItem) " Expand")
			}
		}
		Else
			nid := TV_Add(NewItem,ParentID,SelectID " Select " TV_GetItemIcon(NewItem) " Expand")
	}
	Menus.iniDelete(Section)
	Next := TV_GetChild(ParentID)
	If next
	{
		Loop
		{
			LastID := A_Index
			TV_GetText(value,Next)
			Menus.iniWrite(Section,A_Index,value)
			next := TV_GetNext(next)
			If not next
				Break
		}
	}
	CreateTVItemSub(NewItem,nid)
	TV_SetItemStyle(nid,NewItem)
	SelectItem_Load(nid)
	GuiControl, +Redraw, MenuTreeView 
}
; ConfigGUI_MoveMenuItem(Direct) {{{2
ConfigGUI_MoveMenuItem(Direct=0){
	Global Menus,Items,EditType,MenuTreeView
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	GuiControl, -Redraw, MenuTreeView 
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
	If Direct ; 向下移动
	{
		NextID := TV_GetNext(SelectID)
		TV_GetText(NextItem,NextID)
		If not NextID
			return
		; 删除两个Item的子项
		TV_ClearSubItem(SelectID)
		TV_ClearSubItem(NextID)
		CreateTVItemSub(NextItem,nid := TV_Modify(SelectID,TV_GetItemIcon(NextItem) Items.GetValue(NextItem,"bold")?" Bold":" -Bold",NextItem))
		TV_SetItemStyle(nid,NextItem)
		CreateTVItemSub(Item,nid := TV_Modify(NextID,TV_GetItemIcon(Item) Items.GetValue(Item,"bold")?" Bold":" -Bold",Item))
		TV_SetItemStyle(nid,Item)
		TV_Modify(nid,"Select")
	}
	Else ;向上移动
	{
		PrevID := TV_GetPrev(SelectID)
		TV_GetText(PrevItem,PrevID)
		If not PrevID
			return
		; 删除两个Item的子项
		TV_ClearSubItem(SelectID)
		TV_ClearSubItem(PrevID)
		CreateTVItemSub(PrevItem,nid := TV_Modify(SelectID,TV_GetItemIcon(PrevItem) Items.GetValue(PrevItem,"bold")?" Bold":" -Bold",PrevItem))
		TV_SetItemStyle(nid,PrevItem)
		CreateTVItemSub(Item,nid := TV_Modify(PrevID,TV_GetItemIcon(Item) Items.GetValue(Item,"bold")?" Bold":" -Bold",Item))
		TV_SetItemStyle(nid,Item)
		TV_Modify(nid,"Select")
	}
	Menus.iniDelete(Section)
	Next := TV_GetChild(ParentID)
	If next
	{
		Loop
		{
			LastID := A_Index
			TV_GetText(value,Next)
			Menus.iniWrite(Section,A_Index,value)
			next := TV_GetNext(next)
			If not next
				Break
		}
	}
	GuiControl, +Redraw, MenuTreeView 
}
; TV_WriteMenu(section,ParentID,sub=False) {{{4
TV_WriteMenu(section,ParentID,sub=False){
	Global Menus,Items
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	If Sub
	{
		TV_GetText(Section,TV_GetSelection())
		Next := TV_GetChild(TV_GetSelection())
	}
	Else
		Next := TV_GetChild(ParentID)
	If next
	{
		Loop
		{
			LastID := A_Index
			TV_GetText(value,Next)
			Menus.iniWrite(Section,A_Index,value)
			next := TV_GetNext(next)
			If not next
				Break
		}
	}
	If Sub
		Items.iniWrite(Section,"Sub",Section)
}
; TV_GetSection() {{{4
TV_GetSection(){
	Global Items,EditType
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
	return Section
}
; TV_SetItemStyle(mid,i) {{{4
TV_SetItemStyle(mid,i){
	Global mzTreeView,Items,Menus
	mzTreeView.TV_Color({hwnd:mid,fore:rgb_bgr_swap((f:=Items.GetValue(i,"TColor"))?f:(fd:=Menus.GetValue("config","Tcolor"))?fd:0),back:rgb_bgr_swap((b:=Items.GetValue(i,"BGColor"))?b:(bd:=Menus.GetValue("config","BGColor"))?bd:0xffffff)})
	;mzTreeView.TV_Color({hwnd:mid,fore:rgb_bgr_swap((f:=Items.GetValue(i,"TColor"))?f:0),back:rgb_bgr_swap((b:=Items.GetValue(i,"BGColor"))?b:0xffffff)})
}
; TV_GetItemIcon(Item) {{{4
TV_GetItemIcon(Item){
	Global Menus,Items,ImageListID
	icon := Items.GetValue(Item,"icon")
	Options := "icon9999"
	If Strlen(icon){
		If RegExMatch(icon,":[-\d]*$"){
			pos := RegExMatch(Icon,":[^:]*$")
			file   := Menus.ReplaceEnv(SubStr(icon,1,pos-1))
			Number := (number := substr(icon,pos+1)) > 0 ? Number + 1 : Number
			ic := IL_Add(ImageListID,file,Number)
			If ic
				Options := "icon" ic
		}
		Else
		{
			file := Menus.ReplaceEnv(icon)
			ic := IL_Add(ImageListID,file)
			If ic
			Options := "icon" ic
		}
	}
	return Options
}
; TV_ClearSubItem(SelectID) {{{4
TV_ClearSubItem(SelectID){
	If not SelectID
		return
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	Loop
	{
		Next := TV_GetChild(SelectID)
		If not Next
			break
		TV_Delete(Next)
	}
}
; ConfigGUI_DeleteMenu() {{{2
ConfigGUI_DeleteMenu(){
	Global Menus,Items,EditType,Languages,Lang,MenuTreeView
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	GuiControl, -Redraw, MenuTreeView 
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
	; ==========================================
	If not SelectID
		return
	TV_GetText(Item,SelectID)
	MsgBox, 36,, % languages.GetValue(Lang,"do you delete this menu") " [ " Item " ] ?`n" Languages.GetValue(Lang,"unable to undo")
	IfMsgBox No
		return
/*
	Items.iniDelete(Item)
	Menus.iniDelete(Item)
	SubItem := FindChild(SelectID)
	Loop,Parse,SubItem,`n
	{
		If not strlen(A_LoopField)
			continue
		Menus.iniDelete(A_LoopField)
		;Items.iniDelete(A_LoopField)
	}
*/
	TV_Delete(SelectID)
	Menus.iniDelete(Section)
	Next := TV_GetChild(ParentID)
	If next
	{
		Loop
		{
			LastID := A_Index
			TV_GetText(value,Next)
			Menus.iniWrite(Section,A_Index,value)
			next := TV_GetNext(next)
			If not next
				Break
		}
	}
	GuiControl, +Redraw, MenuTreeView 
}
FindChild(sid){
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	If (Next := TV_GetChild(sid))
	{
		Loop
		{
			TV_GetText(i,Next)
			If TV_GetChild(Next)
				Item .= FindChild(Next) "`n"
			Item .= i "`n" 
			Next := TV_GetNext(Next)
			If not Next
				Break
		}
	}
	Else
		TV_GetText(Item,SelectID)
	return Item
}

; CreateTVItem(item="") {{{2
CreateTVItem(item="")
{
	Global languages,Lang,EditType,Menus,mzTreeView,hTreeView
	mzTreeView := new TreeView(hTreeView)
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

; CreateTVItemSub(item,id) {{{2
CreateTVItemSub(item,id)
{
	Global Items,Menus,MenuTreeView,ImageListID,hTreeView,mzTreeView
	GUI,Config:Default
	GUI,Config:TreeView,SysTreeView321
	GuiControl, -Redraw, MenuTreeView 
	Menus_Keys := iSort(Menus.GetKeys(Item))
	Loop,Parse,Menus_Keys,`n
	{ 
		If Strlen(A_LoopField){
			i := Menus.GetValue(item,A_LoopField)
			icon := Items.GetValue(i,"icon")
			Options := "icon9999 Expand"
			If Strlen(icon){
				If RegExMatch(icon,":[-\d]*$"){
					pos := RegExMatch(Icon,":[^:]*$")
					file   := Menus.ReplaceEnv(SubStr(icon,1,pos-1))
					Number := (number := substr(icon,pos+1)) > 0 ? Number + 1 : Number
					ic := IL_Add(ImageListID,file,Number)
					If ic
					Options := "icon" ic " Expand"
				}
				Else
				{
					file   := Menus.ReplaceEnv(icon)
					ic := IL_Add(ImageListID,file)
					If ic
					Options := "icon" ic " Expand"
				}
			
			}
			Options := Items.GetValue(i,"Bold") ? Options " bold" : Options
			mid := TV_Add(i,id,options)
			mzTreeView.TV_Color({hwnd:mid,fore:rgb_bgr_swap((f:=Items.GetValue(i,"TColor"))?f:(fd:=Menus.GetValue("config","Tcolor"))?fd:0),back:rgb_bgr_swap((b:=Items.GetValue(i,"BGColor"))?b:(bd:=Menus.GetValue("config","BGColor"))?bd:0xffffff)})
			;mid := mzTreeView.TV_Add({label:i,parent:id,option:Options
			If ( sub := Items.GetValue(Menus.GetValue(item,A_LoopField),"sub")) 
				If RegExMatch(Item,ToMatch(sub))
					Items.IniDelete(Item,"Sub")
				Else
					CreateTVItemSub(sub,mid)
		}
	}
	GuiControl, +Redraw, MenuTreeView 
	return mid
}
d2Hex(var){
SetFormat, IntegerFast, hex
Var += 0  ; 赋值 Var (原来的内容为 11) 为 0xb.
Var .= ""  ; 这行是必须的, 因为在快速模式.
SetFormat, IntegerFast, d
return var
}

; class treeview {{{2
class treeview{
	static list:=[]
	__New(hwnd){
		this.list[hwnd]:=this
		OnMessage(0x4e,"WM_NOTIFY")
		this.hwnd:=hwnd
	}
	TV_Add(info){
		hwnd:=TV_Add(info.Label,info.parent,info.option)
		Gui,TreeView,% this.hwnd
		if info.fore!=""
			this.control[hwnd,"fore"]:=info.fore
		if info.back!=""
			this.control[hwnd,"back"]:=info.back
		this.control[hwnd]
		return hwnd
	}
	TV_Color(info){
		Gui,TreeView,% this.hwnd
		if info.fore!=""
			this.control[info.hwnd,"fore"]:=info.fore
		if info.back!=""
			this.control[info.hwnd,"back"]:=info.back
		this.control[hwnd]
	}
}
WM_NOTIFY(Param*){
	;Toolbar_onNotify(Wparam,Lparam,Msg,Hwnd)
	Toolbar_onNotify(Param.1,Param.2,Param.3,Param.4)
	control:=
	if (this:=treeview.list[NumGet(Param.2)])&&(NumGet(Param.2,2*A_PtrSize,"int")=-12){
		stage:=NumGet(Param.2,3*A_PtrSize,"uint")
		if (stage=1) ; dwDrawStage == CDDS_PREPAINT
		return 0x20 ;sets CDRF_NOTIFYITEMDRAW
		if (stage=0x10001&&info:=this.control[numget(Param.2,A_PtrSize=4?9*A_PtrSize:7*A_PtrSize,"uint")]){ ;NM_CUSTOMDRAW && Control is in the list
			if info.fore!=""
				NumPut(info.fore,Param.2,A_PtrSize=4?12*A_PtrSize:10*A_PtrSize,"int") 	;sets the foreground
			if info.back!=""
				NumPut(info.back,Param.2,A_PtrSize=4?13*A_PtrSize:10.5*A_PtrSize,"int") ;sets the background
		}
	}
}
; Notify(wParam, lParam, msg, hwnd, obj) {{{2
Notify(wParam, lParam, msg, hwnd, obj){
	Global SCI,Menus
	If (obj.scnCode = SCN_AUTOCSELECTION)
	{
		text := StrGet(obj.text,"UTF-8")
		If RegExMatch(text,"\{file:")
		{
			new := "{file:path}`n{file:dir}"
			sci.AutocShow(6,new)
		}
	}
	If (obj.scnCode = SCN_CHARADDED)
	{
		pos := sci.GetCurrentPos()
		startPos := sci.WordStartPosition(pos-1)
		endPos := sci.WordEndPosition(pos-1)
		sci.GetTextRange([startPos,endPos],word)
		If RegExMatch(word,"[\s\r\n]*{")
		{
			new := "{file:`n{box:"
			sci.AutocShow(1,new)
		}
		If RegExMatch(word,"[\s\r\n]*%")
		{
			old := Menus.GetKeys("Env")
			Loop,parse,old,`n
			{
				if not strlen(A_LoopField)
					continue
				new .= "%" A_LoopField "%`n" 
			}
			sci.AutocShow(1,new)
		}
	}
	If (obj.scnCode = SCN_AUTOSELECTION )
	{
		msgbox 5 
	}
	If (obj.scnCode = SCN_UPDATEUI)
	{
		sci.StartStyling(0,0x1f)
		sci.SetStyling(sci.GetLength()+1,STYLE_DEFAULT)
		sci.GetText(sci.GetLength()+1,String)

		P0 := 1
		Loop
		{
			P1 := RegExMatch(String,"\{[^\{\}]*\}",switch,P0)
			If P1
			{
				P0 := P1 + strlen(switch)
				len := GetStringLen(SubStr(String,1,P1))
				sci.StartStyling(len-1,0x1f)
				sci.SetStyling(GetStringLen(switch),SCE_AHKL_LABEL)

				P2 := RegExMatch(switch,":([^\{\}]*)(?=\})",opt)
				If P2
				{
					len_opt := GetStringLen(SubStr(switch,1,P2))
					sci.StartStyling(len+len_opt-1,0x1f)
					sci.SetStyling(GetStringLen(opt1),SCE_AHKL_VAR)
				}
			}
			Else
				Break
		}

		P0 := 1
		Loop
		{
			P1 := RegExMatch(String,"%[^%]*%",switch,P0)
			If P1 
			{
				P0 := P1 + strlen(switch)
				If ( Menus.ReplaceEnv(switch) <> switch)
				{
					sci.StartStyling(GetStringLen(SubStr(String,1,P1))-1,0x1f)
					sci.SetStyling(GetStringLen(switch),SCE_AHKL_BUILTINVAR)
				}
			}
			Else
				Break
		}
	}
}
; GetStringLen(string) {{{2
GetStringLen(string)
{
	;[^\x00-\xff]
	count := 0
	Loop,Parse,String
		If RegExMatch(A_LoopField,"[^\x00-\xff]")
			Count += 2
		Else
			Count++
	return Count
}


LoadCUR:
Cross_CUR:="000002000100202002000F00100034010000160000002800000020000000400000000100010000000000800000000000000000000000020000000200000000000000FFFFFF000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF83FFFFFE6CFFFFFD837FFFFBEFBFFFF783DFFFF7EFDFFFEAC6AFFFEABAAFFFE0280FFFEABAAFFFEAC6AFFFF7EFDFFFF783DFFFFBEFBFFFFD837FFFFE6CFFFFFF83FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF28000000"

Cross_CUR_File:= A_Temp "\Cross.CUR"
BYTE_TO_FILE(StrToBin(Cross_CUR),Cross_CUR_File)

return

; Setico {{{2
Setico:
Setico1:
Setico2:
Setico3:
GUI,Config:Default
GuiControlGet,SaveClass,,Edit4
GuiControlGet,SaveCtrl,,Edit5
IfNotExist,%Cross_CUR_File%
	BYTE_TO_FILE(StrToBin(Cross_CUR),Cross_CUR_File)
;设置鼠标指针为十字标
CursorHandle := DllCall( "LoadCursorFromFile", Str,Cross_CUR_File )
DllCall( "SetSystemCursor", Uint,CursorHandle, Int,32512 )
gLable := "GetPos" SubStr(A_ThisLabel,7,1)
If !pToken := Gdip_Startup()
	return
Gui, 1: -Caption +E0x80000 +LastFound +OwnDialogs +Owner +AlwaysOnTop
Gui, 1: Show, NA
hwnd1 := WinExist()
hbm := CreateDIBSection(A_ScreenWidth, A_ScreenHeight)
hdc := CreateCompatibleDC()
obm := SelectObject(hdc, hbm)
G := Gdip_GraphicsFromHDC(hdc)
Gdip_SetSmoothingMode(G, 4)
pPen := Gdip_CreatePen(0xFFFF0000,3)

SetTimer,%gLable%,200
KeyWait,LButton
SetTimer,%gLable%,off

Gdip_DeletePen(pPen)
SelectObject(hdc, obm)
DeleteObject(hbm)
DeleteDC(hdc)
Gdip_DeleteGraphics(G)
Gdip_Shutdown(pToken)
GUI,1:Destroy


;还原鼠标指针
DllCall( "SystemParametersInfo", UInt,0x57, UInt,0, UInt,0, UInt,0 )
return

; GetPos {{{3
GetPos:
	MouseGetPos,,,id
	GUI,OptionGUI:Default
	WinGetClass,c,ahk_id %id%
	GuiControl,,Edit4,%c%
return
GetPos2:
	MouseGetPos,,,id
	GUI,Config:Default
	GuiControlGet, var, Enabled,Edit4
	if var
	{
		WinGetPos,x,y,w,h,ahk_id %id%
		Gdip_GraphicsClear(G)
		Gdip_DrawRectangle(G,pPen,x+2,y+2,w-4,h-4)
		UpdateLayeredWindow(hwnd1, hdc, 0, 0, A_ScreenWidth, A_ScreenHeight)
		WinGetClass,c,ahk_id %id%
		If RegExMatch(SaveClass,"\|$")
			c := SaveClass c
		GuiControl,,Edit4,%c%
	}
return
GetPos3:
	MouseGetPos,,,id,ctrl
	GUI,Config:Default
	WinGetPos,x1,y1,,,ahk_id %id%
	ControlGetPos , X, Y, W, H, %ctrl%,ahk_id %id%
	Gdip_GraphicsClear(G)
	Gdip_DrawRectangle(G,pPen,x1+x,y1+y,w,h)
	UpdateLayeredWindow(hwnd1, hdc, 0, 0, A_ScreenWidth, A_ScreenHeight)
	If RegExMatch(SaveCtrl,"\|$")
		ctrl := SaveCtrl ctrl
	GuiControl,,Edit5,%ctrl%
return





;字符串转二进制
StrToBin(Str) {
XMLDOM:=ComObjCreate("Microsoft.XMLDOM")
xmlver:="<?xml version=`"`"1.0`"`"?>"
XMLDOM.loadXML(xmlver)
Pic:=XMLDOM.createElement("pic")
Pic.dataType:="bin.hex"
pic.nodeTypedValue := Str
StrToByte := pic.nodeTypedValue
return StrToByte
}


; 数据流保存为文件
BYTE_TO_FILE(body, filePath){
  Stream := ComObjCreate("Adodb.Stream")
  Stream.Type := 1
  Stream.Open()
  Stream.Write(body)
  Stream.SaveToFile(filePath,2) ;文件存在的就覆盖
  Stream.Close()
}


; Tools 函数 {{{1
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

; iRelativePath(i) {{{2
iRelativePath(file){
	file := RegExReplace(file,"i)" ToMatch(A_ScriptDir "\Config"),"%CONFIG%")
	file := RegExReplace(file,"i)" ToMatch(A_ScriptDir "\Plugins"),"%PLUGINS%")
	file := RegExReplace(file,"i)" ToMatch(A_ScriptDir "\Script"),"%SCRIPT%")
	file := RegExReplace(file,"i)" ToMatch(A_ScriptDir "\Icons"),"%ICONS%")
	file := RegExReplace(file,"i)" ToMatch(A_ScriptDir "\Apps"),"%APPS%")
	file := RegExReplace(file,"i)" ToMatch(A_ScriptDir),"%A_SCRIPTDIR%")
	file := RegExReplace(file,"i)" ToMatch(A_WinDir),"%A_WINDIR%")
	return file
}



; GetType(s) {{{2
GetType(s){
	Global SelectType
	return SelectType
}

; rgbcolor(red=0,green=0,blue=0) {{{2
rgbcolor(red=0,green=0,blue=0)
{
  color:= (red << 16) + (green << 8) + blue
  return color
}

; bgrcolor(red=0,green=0,blue=0) {{{2
bgrcolor(red=0,green=0,blue=0)
{
  color:= (blue << 16) + (green << 8) + red
  return color
}

; rgb_bgr_swap(color) {{{2
; this swaps the color rgb <-> bgr
rgb_bgr_swap(color)
{
  red:= ((Color & 0xff0000) >> 16)
  green:= ((Color & 0x00ff00) >> 8)
  blue:= (Color & 0xff)

  color2:= (blue << 16) + (green << 8) + red
  return color2
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

; 复制函数 {{{1
; vim_copy(timeout=1,method=1) {{{2
; 设置在VIM中复制文件
vim_copy(timeout=1,method=1){
	mzClipBackup()
	mzClipClear()
	sendRaw,"+y
	ClipWait,% timeout,1
	If ErrorLevel
		return False
	mzSetText(Clipboard)
	mzClipRestore()
	return True
}
tc_copy(timeout=1,method=1){
	return iCopy(timeout,0)
/*
	mzClipBackup()
	mzClipClear()
	PostMessage 1075, 905, 0, , AHK_CLASS TTOTAL_CMD
	ClipWait,% timeout,1
	If ErrorLevel
		return False
	mzSetText(Clipboard)
	mzClipRestore()
	return True
*/
}

#Include %A_ScriptDir%\lib\INI.ahk
#Include %A_ScriptDir%\Plugins\Plugins.ahk
