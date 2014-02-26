
/******************************************************************************

	; ahk forum : http://www.autohotkey.com/community/viewtopic.php?f=2&t=84054
	; StructClass Make Helper : http://myhome.internet.olleh.com/~chaidy/CStruct_ScriptMaker/CStruct_ScriptMaker.rar

	< CStruct_Base class - AHK_L (32bit & 64bit) >

	ver 8.3 - last update 2014.01.17
   
	<Properties>
	struct.member 							: set or get struct member's value.
											  (ex. var := rect.right , rect.top := 1 , struct.rect := new CRect(0,0,100,100) )
	struct.member["count"]					: get struct member array count. normaly 1 return.
	struct.member["type"]					: get struct member type.
	struct.member["size"]					: get struct member type size. its same "sizeof(type)"
	struct.member["Ptr"] or [""]			: get struct member address. (ex. Numget(rect.top["Ptr"],0,"Uint") )
	struct.memberCount						: get struct member count.
	struct.Ptr   or   struct[""]			: get struct starting address. (ex. GetWindowRect(hwnd, rect.ptr) )
	struct.size  or   struct["size"]		: get struct size. (ex. cbSize := struct.size )
	struct['index']							: access to struct member by index number(1,2,3,4...). can't over struct memberCount
   
	struct.stringArrayMember := "hello." 	: set string to struct character array member. ( possable array type: CHAR,TCHAR,WCHAR )
	struct.LPSTRtypeMember["bufferSize"]	: set or get capacity for include string buffer. (its need for receive string data from win api)
	struct.LPSTRtypeMember := stringVar 	: set include string from stringVar. (auto capacity allocate for string buffer.)
	struct.LPSTRtypeMember := "String" 		: set include string from "String". but exclude number data in thease "42344", "330", ...
	struct.LPSTRtypeMember := &var			: not include string. only save variable address value.
   
   
	<Method>
	- AddStructVar(varName, varType, arrayCount, unionState)
		ex1. use window api variable type (byte, int, char, LPSTR, HICON, ...)
			AddStructVar("width", "int")     AddStructVar("winName", "wchar", 32)
         
		ex2. use defined CStruct class type (CPoint, CRect, ...)
			AddStructVar("pt", "CPoint")     AddStructVar("ptArray", "CPoint", 10)

		ex3. define union member
			AddStructVar("a" , "Byte" , "union_start")
			AddStructVar("b" , "Word")
			AddStructVar("c" , "DWord" , "union_end")
		 
		ex4. define bitfield type member
			AddStructVar("b" , "Word" , "bit:4")


	- SetStructCapacity(allocSize=0)	: allocate memory for added struct var. (allocSize: menual size setting.)
   
	- CopyFrom(Ptr or CStructObj)		: copy from 'Same struct' address or same CStruct object.
	- AddrFrom(from_addr)				: set struct address from memory address.
	- ToString()						: get struct member normaly infomation.
	- TreeView()						: Show Structure Object on TreeView.(CStruct_Base.TreeView() - show class template)
	- Clone()							: get clone object.
	- ZeroMemory()						; fill zero to struct memory.
	- CheckName(name)					: check struct member name validation. use in inherit class.
	- GetData(name, n)					: get struct member data. use in inherit class. (n: arrayNumber, "ptr", "type", "count")
	- SizeOf(varType)					: get varType size. 0 return if undefined type.
	- Encoding(name)  					: get default encoding. possable override in inherit class.
	- NewTemplate()						: clear current class template. (for variable array class)


    <NewTemplate() use sample>
	DllCall("gdiplus\GdipGetImageEncodersSize", "UintP",nCount, "UintP",nSize)
	VarSetCapacity(ci,nSize,0)
	DllCall("gdiplus\GdipGetImageEncoders", "Uint",nCount, "Uint",nSize, "UPTR",&ci)
	encoders := new CImageCodecInfoArray(nCount)
	encoders.AddrFrom(&ci)
	encoders.TreeView()
	; CImageCodecInfoArray class size is variable.
	class CImageCodecInfoArray extends CStruct_Base
	{
		__New(nCount)
		{
			this.NewTemplate()				; clear to this class old template.
			this.AddStructVar("array", "CImageCodecInfo", nCount)
			this.SetStructCapacity()
		}
	}

   
	<method overridding model> 
	__Get(name, arrayNum=0)	; 1 - possable use in all case. it use this if has problem following cases.
	__Get(name, arrayNum)	; 2 - possable use in the generality of cases.
	__Get(name)				; 3 - possable use in the generality of cases.
	{
	}
	
	__Set(name, p1, p2)	; 1	- possable use in all case.
	__Set(name, p*)		; 2 - possable use in all case.
	__Set(p*)			; 3	- possable use in all case.
	__Set(name, p)		; 4 - this case can't use if struct has array member. it will be fault at that time access to array member.
	{
	}
	
	Encoding(name)				; use it need to custom encoding option.
	{
		if name=utf8String
			return "UTF-8"
		return base.Encoding(name)
	}


	<structure class making sample>
	class CSample extends CStruct_Base						; must write "extends CStruct_Base"
	{
		__New()												; or  __New(param*)
		{
			this.AddStructVar("a", "int")					; <- int a;
			this.AddStructVar("b", "int", 3) 				; <- int b[3]
			this.AddStructVar("c", "int", "bit:3")			; <- int c :3;
			this.AddStructVar("d", "dword", "union_start")	; <- union { dword d;
			this.AddStructVar("e", "CRect")					; <- RECT e;
			this.AddStructVar("f", "char", 2, "union_end")	; <- char f[2]; }
			this.SetStructCapacity()						; allocate struct memory
		}
	}
*/

;******************************************************************************

class CStruct_Base
{
	;0 : not exit after error msg.
	;1 : exit after error msg
	static CStruct_Base_Flag_ExitAppAfterErrorMsg := 1
	static m_template := {}


	__New()
	{
		if (this.__class="CStruct_Base")
			this.__ErrorMsg("error_basenew", this.__class)
	}
	__Initialize()
	{
		this.m_struct := {"addr":0, "data":"", child:{}}
		if state := this.__MakeThisClassFromTemplate()
			this.m_useTemplate := 1
		else
		{
			tCls := this.m_struct.tCls := this.m_template[this.__class]
			if (state="")
				tCls.Insert("varList", {}), tCls.Insert("varOrder", {}), tCls.Insert("varCount", 0)
			else
				this.m_template[this.__class] := {"varList":{}, "varOrder":{}, "varCount":0}
			this.m_union := {"state":0, "state_old":0, "offset":0, "size":0}
			this.m_bitField := {"bitCount":0, "size_old":0, "offset_old":0}
			this.m_struct.tCls := this.m_template[this.__class]
		}
	}
	
	__Get(name, number=0)
	{
		if ("m_"<>SubStr(name, 1, 2)) and (name<>"__class")
			return this.GetData(name, number, 1)
	}
	GetData(name, number=0, callFrom__Get=0)
	{
		mObj := this.m_struct
		tCls := mObj.tCls
		if (name="ptr") or (name="")
			return mObj.addr
		else if name is Integer
			name := (name<=tCls.varCount and name>0) ? tCls.varOrder[name] : name
		else if (name="__base")
			return "CStruct_Base"
		else if (name="size")
			return mObj.addr? tCls.size : this.__GetNextVarOffset()
		else if (name="memberCount")
			return tCls.varCount
		if vObj := tCls.varList[name]
		{
			name_exist = 1
			if (number="ptr") or (number="")
				return mObj.addr + vObj.o
			if (number="count")
				return vObj.c
			if (number="type")
				return vObj.org
			if (number="size")
				return vObj.s
			if (number="bufferSize")
				return ObjGetCapacity(mObj.string, name)
			if (number=0)
			{
				orgType := vObj.org
				if (vObj.c = 1)
				{
					if callFrom__Get and InStr(orgType, "STR")
						return (encoding:=this.Encoding(name))? StrGet(NumGet(mObj.addr, vObj.o, vObj.t), encoding):StrGet(NumGet(mObj.addr, vObj.o, vObj.t))
					number := 1
				}
				else
				{
					if (encoding:=this.Encoding(name)) and InStr(orgType, "CHAR")
						return StrGet(mObj.addr+vObj.o, encoding)
					arry := Array()
					loop % vObj.c
						arry[A_index] := NumGet(mObj.addr, vObj.o + vObj.s * (A_index-1), vObj.t)
					return arry
				}
			}
			if (vObj.t="CStruct_Base")
				return mObj.child[name][number]
			else
			{
				if (number<=vObj.c)
				{
					if !mObj.addr
						return this.__ErrorMsg("error_addr", this.__Class "." A_ThisFunc)
					if !vObj.bc
						return NumGet(mObj.addr, vObj.o + vObj.s * (number-1), vObj.t)
					;bitfield get
					if (number=1)
					{
						field := NumGet(mObj.addr, vObj.o, vObj.t)
						mask = 0x
						loop % vObj.s
							mask .= "ff"
						mask >>= vObj.s*8 - vObj.bc  ,	mask <<= vObj.bo  ,  field &= mask
						return field >> vObj.bo
					}
				}
			}
		}
		if !name_exist
			return this.__ErrorMsg("error_notexistname", this.__Class "." A_thisFunc, name)
	}
	
	__Set(name, ByRef first, second*)
	{
		if ("m_"<>SubStr(name, 1, 2))
		{
			mObj := this.m_struct
			tCls := mObj.tCls
			if name is Integer
				name := (name<=tCls.varCount and name>0) ? tCls.varOrder[name] : name
			if vObj := tCls.varList[name]
			{
				name_exist = 1
				if !mObj.addr
					return this.__ErrorMsg("error_addr", this.__Class "." A_ThisFunc)
				if !ObjHasKey(second, 1)
				{
					if (vObj.t="CStruct_Base")
					{
						if (mObj.child[name][1].__class=first.__class)
							return first , mObj.child[name][1].CopyFrom(first)
					}
					else
					{
						orgType := vObj.org
						if first is not Integer
							strValue := 1
						if (vObj.c>1)
						{
							if IsObject(first)
							{
								if (maxIndex:=first.MaxIndex()) > vObj.c
									return
								loop % vObj.c
									NumPut((A_index>maxIndex)? 0:first[A_index]+0, mObj.addr, vObj.o + vObj.s*(A_index-1), vObj.t)
								return first
							}
							IfInString, orgType, CHAR
							{
								if (encoding := this.Encoding(name)) and (size := this.__GetEncodingSize(length, first, encoding)) 
								if (size <= vObj.c * vObj.s) and StrPut(first, mObj.addr+vObj.o, length, encoding)
									DllCall("RtlZeroMemory", "UPTR",mObj.addr+vObj.o+size, "UInt",vObj.c*vObj.s-size)
								return first
							}
						}
						if (vObj.c=1) and InStr(orgType, "STR")
						{
							if strValue or IsByRef(first)
							{
								if addr := this.__StringInclude(name, first)
									return first  ,  NumPut(addr, mObj.addr, vObj.o, vObj.t)
							}
							else
								sObj := this.m_struct.string , sObj[name] := ""
						}
						if !vObj.bc
							NumPut(first+0, mObj.addr, vObj.o, vObj.t)
						else
						{
							;bitfield set
							field := NumGet(mObj.addr, vObj.o, vObj.t)
							maskAll = 0x
							loop % vObj.s
								maskAll .= "ff"
							mask1 := maskAll
							mask2 := mask1 >>= vObj.s*8 - vObj.bc  ,  mask1 <<= vObj.bo  ,  mask1 ^= maskAll
							field &= mask1  ,  mask2 &= first+0    ,  mask2 <<= vObj.bo  ,  field |= mask2
							NumPut(field, mObj.addr, vObj.o, vObj.t)
						}
						return first
					}
				}
				else	;if ObjHasKey(second, 1)
				{
					if (first="bufferSize") and (vObj.c=1) and InStr(vObj.org, "STR") and (second[1]>=0)
					{
						this.__SetSizeForIncludeString(name, second[1], 1)
						NumPut(second[1]>0? ObjGetAddress(mObj.string, name):0, mObj.addr, vObj.o, vObj.t)
						return second[1]
					}
					if (first<1) or (first>vObj.c)
						return second[1]
					if (vObj.t="CStruct_Base")
					{
						obj := mObj.child[name][first]
						if (obj.__class=second[1].__class)
							obj.CopyFrom(second[1])
					}
					else
						NumPut(second[1]+0, mObj.addr, vObj.o + vObj.s * (first-1), vObj.t)
					return second[1]
				}
			}
			if !name_exist
				return this.__ErrorMsg("error_notexistname", this.__Class "." A_thisFunc, name)
		}
	}
	
	;------------------------------------------------------------
	AddStructVar(name, orgType, ByRef count=1, unionState="")
	{
		if !this.m_struct
			this.__Initialize()
		tCls := this.m_struct.tCls
		if IsByRef(count) and !tCls.sizeVariable
		{
			msg = IsByRef("arrayCount") = true : "arrayCount" only use constant number.`nif use it variable array count that insert 'this.NewTemplate()' in __New(count) below first line.
			return this.__ErrorMsg("error_addvar", A_ThisFunc, name, orgType, count, unionState, msg)
		}
		if this.m_useTemplate
			return

		orgCount := count , orgUState := unionState
		if count is not Integer
		IfInString, count, bit:
		{
			StringReplace, bitCount, count, % " ",, All
			bitCount := RegExReplace(bitCount, "i)bit:")
		}
		if bitCount is Integer
			count := 0
		else
			bitCount := 0
		if count is not Integer
		if (unionState="")
			unionState := count , count := 1
		uObj := this.m_union
		uObj.state_old := uObj.state
		if (unionState="union_start")
			uObj.state := 1 , uObj.state_old := 0
		else if (unionState="union_end")
			uObj.state := 2
		else if (unionState<>"")
		{
			msg = `"%unionState%`" : sentence error.
			return this.__ErrorMsg("error_addvar", A_ThisFunc, name, orgType, orgCount, orgUState, msg)
		}
		if (uObj.state=0)
			uObj.offset := uObj.size := 0
		if (uObj.state=3)
			uObj.state := 0
		if count is not integer
			countInvalid := 1
		else
		if count<0
			countInvalid := 1
		else
		if !bitCount and count<1
			countInvalid := 1

		size := InStr(orgType, ".")? 0:this.sizeof( type := this.wintype(orgType) )
		goodName := this.CheckName(name, 1)
		if !name or !orgType or countInvalid or !goodName or (badType:=RegExMatch(RegExReplace(orgType, "\."), "\W")) 
		or (badType:=RegExMatch(orgType, "\.\.")) or (badBit := !count and (bitCount<=0)) or (badBitType := !count and (size=8))
		{
			msg := (!orgType or badType)? "invalid varType." : !name? "Invalid varName." : goodName=0? """" name """  can't use this varName." 
			: (goodName="")? """" name """ : already exist this varName." : countInvalid? """" count """ : invalid array count." 
			: badBit? """bit:" bitCount """ invalid bit field count": badBitType? orgType ": can't use 8byte bitfield data type.":""
			return this.__ErrorMsg("error_addvar", A_ThisFunc, name, orgType, orgCount, orgUState, msg)
		}
		
		count := count<1 ? 1 : count
		if size
		{
			allSize := size * count
			if bitCount and (size*8<bitCount)
				return this.__ErrorMsg("error_addvar", A_ThisFunc, name, orgType, orgCount, orgUState
				, """bit:" bitCount """ type of bit field too small for number of bits")
		}
		else
		{
			cls := this.__MakeClass(orgType)
			if (badType:=(cls.__base<>"CStruct_Base")) or (sizeVariable:=cls.m_struct.tCls.sizeVariable) or bitCount
			{
				msg := badType? """" orgType """ : invalid this varType." : bitCount? """bit:" bitCount 
				. """ bit field can't use with a struct class type" : sizeVariable? """" orgType """ : can't use size variable class type":""
				return this.__ErrorMsg("error_addvar", A_ThisFunc, name, orgType, orgCount, orgUState, msg)
			}
			type := "CStruct_Base"
			size := cls.size
			allSize := size * count
			
			child := this.m_struct.child
			if !IsObject(child[name])
				child[name] := {}
			child[name].Insert(cls)
			loop % count-1
				child[name].Insert(this.__MakeClass(orgType))
		}
		
		;Get varList obj from class template object
		vList := tCls.varList
		if !tCls.size
			tCls.size := 0
		if !IsObject(vList[name])
			vList[name] := {}

		sizeBackup := tCls.size
		if (uObj.state=1)and(uObj.state_old=0) or (uObj.state=0)and(uObj.state_old=3)
			unionStateChange := 1
		if ((uObj.state=2)and(uObj.state_old=1)) or ((uObj.state=1)and(uObj.state_old=1))
		{
			if (uObj.state=2)
				uObj.state := 3
			vList[name].o := uObj.offset
			uObj.size := uObj.size<allSize? allSize:uObj.size
			tCls.size := uObj.offset + uObj.size
			union := 1
		}
		else 
		{
			vList[name].o := this.__GetNextVarOffset((type="CStruct_Base")? pack:=this.m_template[orgType].pack : size)
			tCls.size := vList[name].o + uObj.size := allSize
			if (uObj.state=1)
				uObj.offset := vList[name].o
		}

		if bitCount
		{
			tCls.size := sizeBackup
			bitObj := this.__GetNextVarBitObj(bitCount, size, unionStateChange, union)
			vList[name].o := bitObj.offset
			vList[name].bo := bitObj.bitOffset
			vList[name].bc := bitCount
			tCls.size += bitObj.addSize
		}

		vList[name].t := type
		vList[name].s := size
		vList[name].c := count
		vList[name].org := orgType
		pack := (type="CStruct_Base")? pack : size
		if !tCls.pack
			tCls.pack := pack
		else
			tCls.pack := (tCls.pack>pack)? tCls.pack : pack
		tCls.varOrder.Insert(name)
		if (type="CStruct_Base")
		{
			if !tCls.child
				tCls.child := {}
			tCls.child.Insert(name, orgType)
		}
		
		tCls.varCount++
		this.m_bitField.offset_old := vList[name].o
		this.m_bitField.size_old := bitCount? size : 0
	}
	
	;------------------------------------------------------------
	SetStructCapacity(allocSize=0, ParentPutAddress=0)
	{
		mObj := this.m_struct
		tCls := mObj.tCls

		if !ParentPutAddress and !this.m_useTemplate
		{
			tCls.size := this.__GetNextVarOffset()
			if this.m_union
				this.m_union := "" 
			if this.m_bitField
				this.m_bitField := ""
		}
		
		if ParentPutAddress
		{
			if ObjGetCapacity(mObj, "data")
				DllCall("RtlMoveMemory", "UPTR",ParentPutAddress, "UPTR",mObj.addr, "UInt",tCls.size)
			mObj.addr := ParentPutAddress, mObj.data := ""
		}
		else
		{
			if allocSize and (allocSize<tCls.size)
				return this.__ErrorMsg(A_ThisFunc, "invalid allocSize. (allocSize=" allocSize ", structSize=" tCls.size ")")
			ObjSetCapacity(mObj, "data", allocSize? allocSize:tCls.size)
			mObj.addr := ObjGetAddress(mObj, "data")
			DllCall("RtlZeroMemory", "UPTR",mObj.addr, "UInt",tCls.size)
		}
		;child struct starting address put.
		;tCls:templateObj, vObj:varObj , mObj:memberObj, cArray:sameNameArray
		vList := tCls.varList
		for cName, cArray in mObj.child
		{
			vObj := vList[cName]
			for number, obj in cArray
				obj.SetStructCapacity(0, mObj.addr + (vObj.o + vObj.s * (number-1)))
		}
	}
   
	Clone()
	{
		cls := this.__MakeClass(this.__class)
		cls.CopyFrom(this)
		return cls
	}

	ZeroMemory()
	{
		mObj := this.m_struct
		tCls := mObj.tCls
		DllCall("RtlZeroMemory", "UPTR",mObj.addr, "UInt",tCls.size)
	}
	
	AddrFrom(from)
	{
		mObj := this.m_struct
		tCls := mObj.tCls
		mObj.addr := from
		mObj.data := ""
		vList := tCls.varList
		for cName, cArray in mObj.child
		{
			vObj := vList[cName]
			for number, obj in cArray
				obj.AddrFrom(from? from + (vObj.o + vObj.s * (number-1)):0)
		}
	}

	CopyFrom(from)
	{
		fObj := from.m_struct
		mObj := this.m_struct
		tCls := mObj.tCls
		if !mObj.addr
			return this.__ErrorMsg("error_addr", this.__Class "." A_ThisFunc)
		if !IsObject(from)
		{
			DllCall("RtlMoveMemory", "UPTR",mObj.addr, "UPTR",from , "UInt",tCls.size)
			return
		}
		if (this.__class<>from.__class)
			return
		if tCls.sizeVariable
			return this.__ErrorMsg("error_sizeVariable", this.__Class "." A_ThisFunc)
		DllCall("RtlMoveMemory", "UPTR",mObj.addr, "UPTR",fObj.addr , "UInt",tCls.size)
		for name in fObj.string
		{
			vObj := tCls.varList[name]
			mObj.string[name] := ""
			size := ObjSetCapacity(mObj.string, name, ObjGetCapacity(fObj.string, name))
			DllCall("RtlMoveMemory", "UPTR", addr := ObjGetAddress(mObj.string, name), "UPTR", ObjGetAddress(fObj.string, name), "UInt", size)
			NumPut(addr, mObj.addr, vObj.o, vObj.t)
		}
	}

	NewTemplate()
	{
		this.m_template[this.__class] := {"SizeVariable":1}
	}

	; 1 return : ok
	; 0 return : invalid var name
	;"" return : exist var name
	CheckName(name, check_duplication=0)
	{
		if name is Integer
			return 0
		if name in __base,__class,count,type,ptr,size,memberCount,bufferSize
			return 0
		if ("m_"=SubStr(name, 1, 2))
			return 0
		tCls := this.m_struct.tCls
		if ObjHasKey(tCls.varList, name)
			return check_duplication? "" : 1
		else
			return check_duplication? 1 : 0
	}

	;------------------------------------------------------------
	;possable string type : CHAR,TCHAR,WCHAR,UCHAR , STR,LPSTR,TSTR,LPTSTR,LPCTSTR,WSTR,LPWSTR,LPCWSTR
	;"" return : wrong name
	Encoding(name)
	{
		static uni  := "UTF-16"
		static ansi := "CP0"
		tCls := this.m_struct.tCls
		type := tCls.varList[name].org
		if !type
			return ""
		if A_IsUnicode
		{
			if type in CHAR,UCHAR,STR,PSTR,PCSTR,LPSTR,LPCSTR
				return ansi
		}
		else
		{
			IfInString, type, W
				return uni
		}
		return A_IsUnicode? uni : ansi
	}

	Dec2Base(n, b)
	{
		return (n < b ? "" : this.Dec2Base(n//b,b)) . ((d:=Mod(n,b)) < 10 ? d : Chr(d+55))
	}

	;------------------------------------------------------------
	;STR type data copy and encoding then its address return
	;return : address=sucess , 0=fail
	__StringInclude(name, Byref string)
	{
		if !this.m_struct.string
			this.m_struct.string := {}
		sObj := this.m_struct.string
		if !encoding := this.Encoding(name)
			return 0
		size := this.__GetEncodingSize(length, string, encoding)
		this.__SetSizeForIncludeString(name, size)
		addr := ObjGetAddress(sObj, name)
		;DllCall("RtlZeroMemory", "UPTR",addr, "UInt",size)
		if encoding
			result := StrPut(string, addr, length, encoding)
		else
			result := StrPut(string, addr, length)
		return result? addr:0
	}
	
	__SetSizeForIncludeString(name, size, keepData=0)
	{
		if !hasObj := this.m_struct.string
			this.m_struct.string := {}
		vObj := this.m_struct.tCls.varList[name]
		sObj := this.m_struct.string
		if !hasObj or !ObjHasKey(sObj, name)
		{
			sObj[name] := ""
			result := ObjSetCapacity(sObj, name, size)
		}
		else
		{
			result := size_old := ObjGetCapacity(sObj, name)
			if (size>size_old)
			{
				if keepData
				{
					VarSetCapacity(str_old, size_old)
					DllCall("RtlMoveMemory", "UPTR", &str_old, "UPTR", ObjGetAddress(sObj, name), "UInt", size_old)
				}
				result := ObjSetCapacity(sObj, name, size)
				if keepData
					DllCall("RtlMoveMemory", "UPTR", ObjGetAddress(sObj, name), "UPTR", &str_old, "UInt", size_old)
			}
			else if size<=0
				result := ObjSetCapacity(sObj, name, 0)
		}
		return result
	}
	
	;length : return to chars count
	;return : encoding buffer size
	;checkName="name" - valid encoding : return 'encoding', invalid encoding : return 0
	;checkName call ex) __GetEncodingSize(tmp1,tmp2,tmp3, name)
	__GetEncodingSize(ByRef length, ByRef string, encoding, checkName="")
	{
		if checkName
			encoding := this.Encoding(checkName) , string := "test"
		size := 0
		if length := StrPut(string, encoding)
		{
			;UTF-8(CP65001)
			if encoding in UTF-8,CP0,CP65001
				size := length
			else if encoding in UTF-16,CP1200
				size := length * 2
		}
		if checkName
			return size? encoding:encoding? "'" encoding "' not support":"undesignated"
		return size
	}
	
	;------------------------------------------------------------
	;structure memory packing
	__GetNextVarOffset(nextVarSize=0)
	{
		tCls := this.m_struct.tCls
		if !nextVarSize
			nextVarSize := tCls.pack? tCls.pack : 1
		;nextVarSize : 1,2,4,8,...
		if (tCls.size=0) or (nextVarSize<=1)
			return tCls.size
		shareSize := tCls.size//8 * 8
		restSize  := tCls.size - shareSize
		if !restSize
			return tCls.size
		if (nextVarSize>=8)
			return shareSize+8
		if (restSize<=nextVarSize)
			return shareSize+nextVarSize
		else
		{
			s := restSize//nextVarSize * nextVarSize
			r := restSize - s
			return r? shareSize+s+nextVarSize : shareSize+s
		}
	}

	;bitfield make
	__GetNextVarBitObj(bitCount, size, stateChange, union)
	{
		tCls := this.m_struct.tCls
		mObj := this.m_struct
		bObj := this.m_bitField
		rObj := {}
		nextOffset := this.__GetNextVarOffset(size)
		if (bObj.size_old=0) or (bObj.size_old<>size) or stateChange
		{
			rObj.offset := nextOffset , rObj.bitOffset := 0 , rObj.addSize := size
			bObj.bitCount := bitCount
		}
		else if union
			rObj.Offset := bObj.offset_old , rObj.bitOffset := 0 , rObj.addSize := 0
		else
		{
			bitOver := (bObj.bitCount+bitCount) // (size*8) and Mod(bObj.bitCount+bitCount, size*8)
			rObj.offset := bitOver? nextOffset : bObj.offset_old
			rObj.bitOffset := bitOver? 0 : bObj.bitCount
			rObj.addSize := bitOver? size : 0
			bObj.bitCount := bitOver? bitCount : bObj.bitCount+bitCount
		}
		return rObj
	}

	__MakeClass(name)
	{
		loop Parse, name, .
			cls := A_Index = 1 ? %A_LoopField% : cls[A_LoopField]
		return new cls
	}
	
	; class template use
	__MakeThisClassFromTemplate()
	{
		if !IsObject(this.m_template[clsName := this.__class])
			return 0
		tCls := this.m_struct.tCls := this.m_template[clsName]
		if tCls.sizeVariable
			return ""
		child := this.m_struct.child
		for vName, clsName in tCls.child
		{
			if !child[vName]
				child[vName] := {}
			loop % tCls.varList[vName].c
				child[vName].Insert(this.__MakeClass(clsName))
		}
		return 1
	}

	__ErrorMsg(error_id, from, v*)
	{
		if (error_id="error_addr")
			msg := "event from: " from "()`n`nnot allocated struct memory.`n`n"
			. """this.SetStructCapacity()""  insert to """ this.__class ".__New()"""
		else if (error_id="error_addvar")
			msg := this.__Class "." from "(varName`, varType`, arrayCount, unionState)`n`nvarName = """ v[1]
			. """`nvarType = """ v[2] """" . "`narrayCount = " v[3] . "`nunionState = " v[4]
			. ((v.MaxIndex()=5)and(v[5])? "`n`n" v[5] : "")
		else if (error_id="error_notexistname")
			msg := "from: " from "()`n`n""" v[1] """  is not a member this Struct."
		else if (error_id="error_basenew")
			msg := "from: " from "()`n`ndon't make ""CStruct_Base"" instance.`n`n" 
			. "this class only use base class."
		else if (error_id="error_sizeVariable")
			msg := "from: " from "()`n`ndon't copy between size variabe class."
		else
			msg := "from: " from "()`n`n" error_id
		StringReplace, msg, msg, .CStruct_Base
		MsgBox,,Error, % msg
		if CStruct_Base.CStruct_Base_Flag_ExitAppAfterErrorMsg
			ExitApp
	}

	sizeof(type_name)
	{
		if (type_name="")
			return 0
		; http://l.autohotkey.net/docs/commands/DllCall.htm#types
		static Ptr:=A_PtrSize,UPtr:=A_PtrSize,Int64:=8,UInt64:=8,Int:=4,UInt:=4,Short:=2,UShort:=2,Char:=1,UChar:=1,Float:=4,Double:=8
		type_match := (%type_name%)
		if !type_match
			type_name := this.wintype(type_name)
		if !type_name
			return 0
		return (%type_name%)
	}
	
	wintype(type_name)
	{
		; http://msdn.microsoft.com/en-us/library/aa383751(v=vs.85).aspx
		static ATOM:="UShort",BOOL:="Int",BOOLEAN:="UChar",BSTR:="UPtr",BYTE:="UChar",CHAR:="Char",COLORREF:="UInt",DOUBLE:="Double",DWORD32:="UInt"
		,DWORD64:="UInt64",DWORD:="UInt",DWORD_PTR:="UPtr",DWORDLONG:="UInt64",FLOAT:="Float",HACCEL:="UPtr",HALF_PTR:=A_PtrSize=8?"Int":"Short"
		,HANDLE:="UPtr",HBITMAP:="UPtr",HBRUSH:="UPtr",HCOLORSPACE:="UPtr",HCONV:="UPtr",HCONVLIST:="UPtr",HCURSOR:="UPtr",HDC:="UPtr"
		,HDDEDATA:="UPtr",HDESK:="UPtr",HDROP:="UPtr",HDWP:="UPtr",HENHMETAFILE:="UPtr",HFILE:="Int",HFONT:="UPtr",HGDIOBJ:="UPtr",HGLOBAL:="UPtr"
		,HHOOK:="UPtr",HICON:="UPtr",HINSTANCE:="UPtr",HKEY:="UPtr",HKL:="UPtr",HLOCAL:="UPtr",HMENU:="UPtr",HMETAFILE:="UPtr",HMODULE:="UPtr"
		,HMONITOR:="UPtr",HPALETTE:="UPtr",HPEN:="UPtr",HRESULT:="Int",HRGN:="UPtr",HRSRC:="UPtr",HSZ:="UPtr",HWINSTA:="UPtr",HWND:="UPtr"
		,INT32:="Int",INT64:="Int64",INT:="Int",INT_PTR:="Ptr",LANGID:="UShort",LCID:="UInt",LCTYPE:="UInt",LGRPID:="UInt",LONG32:="Int"
		,LONG64:="Int64",LONG:="Int",LONG_PTR:="Ptr",LONGLONG:="Int64",LPARAM:="Ptr",LPBOOL:="UPtr",LPBYTE:="UPtr",LPCOLORREF:="UPtr"
		,LPCSTR:="UPtr",LPCTSTR:="UPtr",LPCVOID:="UPtr",LPCWSTR:="UPtr",LPDWORD:="UPtr",LPHANDLE:="UPtr",LPINT:="UPtr",LPLONG:="UPtr"
		,LPSTR:="UPtr",LPTSTR:="UPtr",LPVOID:="UPtr",LPWORD:="UPtr",LPWSTR:="UPtr",LRESULT:="Ptr",PBOOL:="Ptr",PBOOLEAN:="Ptr",PBYTE:="Ptr"
		,PCHAR:="Ptr",PCSTR:="Ptr",PCTSTR:="Ptr",PCWSTR:="Ptr",PDWORD32:="Ptr",PDWORD64:="Ptr",PDWORD:="Ptr",PDWORD_PTR:="Ptr",PDWORDLONG:="Ptr"
		,PFLOAT:="Ptr",PHALF_PTR:="Ptr",PHANDLE:="UPtr",PHKEY:="UPtr",PINT32:="UPtr",PINT64:="UPtr",PINT:="UPtr",PINT_PTR:="UPtr",PLCID:="UPtr"
		,PLONG32:="UPtr",PLONG64:="UPtr",PLONG:="UPtr",PLONG_PTR:="UPtr",PLONGLONG:="UPtr",POINTER_32:="UInt",POINTER_64:="Ptr"
		,POINTER_SIGNED:="Ptr",POINTER_UNSIGNED:="UPtr",PSHORT:="UPtr",PSIZE_T:="UPtr",PSSIZE_T:="UPtr",PSTR:="UPtr",PTBYTE:="UPtr",PTCHAR:="UPtr"
		,PTR:="Ptr",PTSTR:="UPtr",PUCHAR:="UPtr",PUHALF_PTR:="UPtr",PUINT32:="UPtr",PUINT64:="UPtr",PUINT:="UPtr",PUINT_PTR:="UPtr"
		,PULONG32:="UPtr",PULONG64:="UPtr",PULONG:="UPtr",PULONG_PTR:="UPtr",PULONGLONG:="UPtr",PUSHORT:="UPtr",PVOID:="UPtr",PWCHAR:="UPtr"
		,PWORD:="UPtr",PWSTR:="UPtr",SC_HANDLE:="UPtr",SC_LOCK:="UPtr",SERVICE_STATUS_HANDLE:="UPtr",SHORT:="Short",SIZE_T:="UPtr",SSIZE_T:="Ptr"
		,TBYTE:=A_IsUnicode?"UShort":"UChar",TCHAR:=A_IsUnicode?"UShort":"UChar",UCHAR:="UChar",UHALF_PTR:=A_PtrSize=8?"UInt":"UShort"
		,UINT32:="UInt",UINT64:="UInt64",UINT:="UInt",UINT_PTR:="UPtr",ULONG32:="UInt",ULONG64:="UInt64",ULONG:="UInt",ULONG_PTR:="UPtr"
		,ULONGLONG:="UInt64",UPTR:="UPtr",USHORT:="UShort",USN:="Int64",VOID:="Ptr",WCHAR:="UShort",WNDPROC:="UPtr",WORD:="UShort",WPARAM:="UPtr"
		return (%type_name%)
	}

	ToString(ByRef noBlock=0, t="")
	{
		static tab := "    "
		noBlock := 0
		mObj := this.m_struct
		tCls := mObj.tCls
		if !mObj.addr
			return this.__ErrorMsg("error_addr", this.__Class "." A_ThisFunc)
		for order, name in tCls.varOrder
		{
			vObj := tCls.varList[name]
			if (vObj.t="CStruct_Base")
			{
				for i, v in mObj.child[name]
				{
					child_str := v.ToString(noBlock:=0, t tab)
					index_str := vObj.c>1? "[" i "]":""
					str .= "`n" t name index_str (noBlock? " =":"") (noBlock? " ":" { ") child_str
					StringRight, chr, str, 1
					endBlock := (noBlock? "":"}")
					str .= (chr="`n")? t endBlock "`n" : " " endBlock "`n"
				}
			}
			else
			{
				nArr := this[name]	; => this.__Get(name,0)
				if IsObject(nArr)
				{
					tmp =
					loop % vObj.c
						tmp .= ", " nArr[A_Index]
					StringReplace, tmp, tmp, % ", "
					nArr = { %tmp% }
				}
				else if (vObj.c>1) or ((vObj.c=1) and InStr(vObj.org, "STR"))
					nArr = `"%nArr%`"
				StringRight, chr, str, 1
				str .= (chr="`n")? ("`, " t name "=" nArr ) : ("`, " name "=" nArr)
			}
		}
		StringReplace, str, str, % "`n`n",  `n, all
		StringReplace, str, str, % "`n`, ", `n, all
		if (SubStr(str, 1, 2)="`, ")
			StringReplace, str, str, % "`, "
		return str
	}

	;x=pointx, y=pointy, w=width, r=row
	;creat TreeWindow axis sample = "guix100 guiy100 guiw500 guir18"
	;ex) TreeView("guix100 guiy10")
	TreeView(Obj="", Option="", hParent="", r="", tObj="")
	{
		if (r = "")
		{
			guix := guiy := 0, guir := 18 , guiw := 400
			if (!IsObject(Obj))
			{
				if (obj)
				{
					guiTitle := Obj
					guix := RegExMatch(guiTitle, "guix(?<gui>\d*)", m_)? m_gui : guix
					guiy := RegExMatch(guiTitle, "guiy(?<gui>\d*)", m_)? m_gui : guiy
					guiw := RegExMatch(guiTitle, "guiw(?<gui>\d*)", m_)? m_gui : guiw
					guir := RegExMatch(guiTitle, "guir(?<gui>\d*)", m_)? m_gui : guir
					guiTitle := RegExReplace(guiTitle, "guix" guix "|guiy" guiy "|guiw" guiw "|guir" guir)
					guiTitle := RegExReplace(guiTitle, "(?:^\s+)")
				}
				if (this.__class="CStruct_Base")
				{
					Obj := this.m_template  ,  r := "template"  ,  guiw := 500
					guiTitle := (!guiTitle)? "Structure Class template" : guiTitle
				}
				else
					Obj := this , guir := Obj.MemberCount>guir-3? Obj.MemberCount+3 : guir
			}
			
			iFormat := A_FormatInteger
			SetFormat, integer, D
			Gui +LastFound
			hWnd := WinExist()
			loop 99
				Gui % (nDefaultGui := A_Index) ":+LastFoundExist"
			until (WinExist() = hWnd)
			loop 70
				Gui % (nGui := A_index+29) ":+LastFoundExist"
			until (hwnd:=WinExist() = 0)
			if !hwnd
			{
				SetFormat, integer, % iFormat
				return
			}
			Gui % nGui ":Default"
			Gui +ToolWindow +Resize +LabelCStruct_Base_TreeView
			Gui Margin, -1, -1
			Gui Add, TreeView, w%guiw% r%guir% AltSubmit hWndhWnd gCStruct_Base_TreeView
			global CStruct_Base_hTreeView
			CStruct_Base_hTreeView := CStruct_Base_hTreeView? CStruct_Base_hTreeView : object()
			CStruct_Base_hTreeView.Insert(nGui, hWnd)
			Gui Show, % "x" guix "y" guiy, % "[" (A_PtrSize=8? "64bit ":"") (A_IsUnicode? "Uni Ver":"Ansi Ver")
				. "] No. " nGui-29 (guiTitle? " - " guiTitle : "")
			if 0
			{
				CStruct_Base_TreeViewEscape:
				CStruct_Base_TreeViewClose:
				Gui % A_Gui ":Destroy"
				return
				CStruct_Base_TreeViewSize:
				global CStruct_Base_hTreeView
				CStruct_Base.__Anchor(CStruct_Base_hTreeView[A_Gui], "wh")
				return
				CStruct_Base_TreeView:
					if A_GuiEvent<>RightClick
						return
					TV_GetText(txt, A_EventInfo)
					id := (SubStr(txt, 1, 1)="*")? TV_GetParent(A_EventInfo):A_EventInfo
					if GetKeyState("Alt")
						Clipboard := CStruct_Base.__TreeViewGetChildText(id, ""), TV_Modify(A_EventInfo)
					else if GetKeyState("Shift")
						Clipboard := CStruct_Base.__TreeViewGetChildText(id, "child_all"), TV_Modify(A_EventInfo)
					else if GetKeyState("Ctrl")
						Clipboard := txt, TV_Modify(A_EventInfo)
				return
			}
		}

		if (Obj.__base="CStruct_Base")
		{
			if (tObj = "")
				tObj := {}
			tCls  := Obj.m_struct.tCls
			vList := tCls.varList
			mObj := Obj.m_struct
			tObj.Insert("*" Obj.__class, "size = " tCls.size " , address = " mObj.addr)
			for order, name in tCls.varOrder
			{
				vObj := vList[name]
				prop := vObj.s (vObj.c>1? " , count : " vObj.c : "")
				if (vObj.t="CStruct_Base")
				{
					for array, cObj in mObj.child[name]
					{
						scnt := strlen(tCls.size) - strlen(offset := vObj.o+vObj.s*(array-1))
						loop % scnt
							offset := "0" offset
						data := InStr(data:=cObj.ToString(noBlock), "`n")? "{ ... }" : noBlock? data:"{ " data " }"
						tObj.Insert(fullName := "[" offset "] " name (vObj.c>1? "[" array "]":"") " = " data, Object())
						this.TreeView(cObj, "", "", -1, tObj[fullName])
					}
				}
				else
				{
					scnt := strlen(tCls.size) - strlen(offset := vObj.o)
					loop % scnt
						offset := "0" offset
					
					data := Obj[name]
					str =
					loop % vObj.c
					{
						if !IsObject(data)
							hex := this.Dec2Base(NumGet(mObj.addr+vObj.o+vObj.s*(A_index-1)
							, 0, vObj.s=1? "UCHAR": vObj.s=2? "USHORT": vObj.s=4? "UINT": vObj.t), 16)
						str .= ", " (IsObject(data)? Obj.GetData(name, A_index) : hex)
					}
					StringReplace, str, str, % ", "
					if IsObject(data)
						data = { %str% }
					else if (vObj.c>1)
						encoding := Obj.__GetEncodingSize(tmp1,tmp2,tmp3,name)
						, prop .= " , Encoding : " encoding " , hex={ " str " }"
					else if (vObj.c=1) and ObjHasKey(mObj.string, name)
					{
						ptr := Obj.GetData(name)  ,  encoding := Obj.__GetEncodingSize(tmp1,tmp2,tmp3,name)
						prop .= " , Pointer : " ptr ((ptr=ObjGetAddress(mObj.string, name))? " , Buffer : " 
						. Obj.GetData(name, "buffersize") " byte" : "") " , Encoding : " encoding
					}
					else if (vObj.c=1) and InStr(vObj.org, "STR")
					{
						encoding := Obj.__GetEncodingSize(tmp1,tmp2,tmp3,name)
						prop .= " , Pointer : " Obj.GetData(name) " , Encoding : " encoding
					}
					if vObj.bc
					{
						scnt := strlen(vObj.s*8) - strlen(bitOffset := vObj.bo)
						Loop % scnt
							bitOffset := "0" bitOffset
						offset .= "][" bitOffset
						prop .= " , " vObj.bc "bit : 0~" 2**vObj.bc-1
						scnt := vObj.bc - strlen(bin := this.Dec2Base(data, 2))
						loop % scnt
							bin := "0" bin
						data .= "  [b]:" bin
					}
					
					tObj.Insert("[" offset "] " name (strlen(data)? " = " data:""), Object(vObj.org, prop))
				}
			}
			if (r = -1)
				return
			r := ""
			SetFormat, integer, % iFormat
			Obj := tObj
		}
		
		if (IsObject(Obj))
		{
			if (r = "")
				r := 1
			
			for k, v in Obj
			{
				if (IsObject(v))
					this.TreeView(v, Option, TV_Add(k, hParent, Option), r)
				else
					TV_Add(k (strlen(v)? " : " v : ""), hParent, Option)
			}
		}
		if (nDefaultGui)
			Gui % nDefaultGui ":Default"
	}

	;mode: child_all
	__TreeViewGetChildText(parentID, mode="", t="")
	{
		static mark_list := " : "
		static tab := " : "
		if (t="")
			if parentID
				TV_GetText(txt, parentID)
			else
				txt = [root]
		if !id := TV_GetChild(parentID)
				return ""
		loop
		{
			if TV_GetText(str, id)
				txt .= "`r`n" t mark_list . str
			if mode=child_all
				txt .= CStruct_Base.__TreeViewGetChildText(id, mode, t tab)
			if !id := TV_GetNext(id)
				break
		}
		
		if (t="")
			txt .= "`r`n"
		return txt
	}
	
	__Anchor(i, a = "", r = false) 
	{
		static c, cs = 12, cx = 255, cl = 0, g, gs = 8, gl = 0, gpi, gw, gh, z = 0, k = 0xffff
		If z = 0
			VarSetCapacity(g, gs * 99, 0), VarSetCapacity(c, cs * cx, 0), z := true
		If (!WinExist("ahk_id" . i)) {
			GuiControlGet, t, Hwnd, %i%
			If ErrorLevel = 0
				i := t
			Else ControlGet, i, Hwnd, , %i%
		}
		VarSetCapacity(gi, 68, 0), DllCall("GetWindowInfo", "UPTR", gp := DllCall("GetParent", "UPTR", i), "UPTR", &gi)
			, giw := NumGet(gi, 28, "Int") - NumGet(gi, 20, "Int"), gih := NumGet(gi, 32, "Int") - NumGet(gi, 24, "Int")
		If (gp != gpi) {
			gpi := gp
			Loop, %gl%
				If (NumGet(g, cb := gs * (A_Index - 1), "Int") == gp) {
					gw := NumGet(g, cb + 4, "Short"), gh := NumGet(g, cb + 6, "Short"), gf := 1
					Break
				}
			If (!gf)
				NumPut(gp, g, gl, "Int"), NumPut(gw := giw, g, gl + 4, "Short"), NumPut(gh := gih, g, gl + 6, "Short"), gl += gs
		}
		ControlGetPos, dx, dy, dw, dh, , ahk_id %i%
		Loop, %cl%
			If (NumGet(c, cb := cs * (A_Index - 1), "Int") == i) {
				If a =
				{
				cf = 1
				Break
				}
				giw -= gw, gih -= gh, as := 1, dx := NumGet(c, cb + 4, "Short"), dy := NumGet(c, cb + 6, "Short")
				, cw := dw, dw := NumGet(c, cb + 8, "Short"), ch := dh, dh := NumGet(c, cb + 10, "Short")
				Loop, Parse, a, xywh
				If A_Index > 1
					av := SubStr(a, as, 1), as += 1 + StrLen(A_LoopField)
							, d%av% += (InStr("yh", av) ? gih : giw) * (A_LoopField + 0 ? A_LoopField : 1)
				DllCall("SetWindowPos", "UPTR", i, "UPTR", 0, "Int", dx, "Int", dy
				, "Int", InStr(a, "w") ? dw : cw, "Int", InStr(a, "h") ? dh : ch, "Int", 4)
				If r != 0
				DllCall("RedrawWindow", "UPTR", i, "UPTR", 0, "UInt", 0, "UInt", 0x0101) ; RDW_UPDATENOW | RDW_INVALIDATE
				Return
			}
		If cf != 1
			cb := cl, cl += cs
		bx := NumGet(gi, 48, "Int"), by := NumGet(gi, 16, "Int") - NumGet(gi, 8, "Int") - gih - NumGet(gi, 52, "Int")
		If cf = 1
			dw -= giw - gw, dh -= gih - gh
		NumPut(i, c, cb, "Int"), NumPut(dx - bx, c, cb + 4, "Short"), NumPut(dy - by, c, cb + 6, "Short")
			, NumPut(dw, c, cb + 8, "Short"), NumPut(dh, c, cb + 10, "Short")
		Return, true
	}	
}


;-------------------------------------------------------------------------------------
;               Window API Structure Class
;-------------------------------------------------------------------------------------

; http://msdn.microsoft.com/en-us/library/dd162805%28VS.85%29.aspx 
class CPoint extends CStruct_Base
{
	__New(Param*)
	{
		this.AddStructVar("x", "int")
		this.AddStructVar("y", "int")
		this.SetStructCapacity()
		
		if (Param.MaxIndex()=1) and (Param[1].__class=this.__class)
			this.CopyFrom(Param[1])
		else if (Param.MaxIndex()=2) {
			this.x := Param[1]
			this.y := Param[2]
		}
	}

	__Get(name)
	{
		if name=int64
			return NumGet(base.GetData("ptr"), "INT64")
	}
	__Set(name, v)
	{
		if name=int64
			return v  ,  NumPut(v+0, base.GetData("ptr"), "INT64")
	}
}

;-------------------------------------------------------------------------------------
; http://msdn.microsoft.com/en-us/library/dd145106%28VS.85%29.aspx 
class CSize extends CStruct_Base
{
	__New(Param*)
	{
		this.AddStructVar("width" , "int")
		this.AddStructVar("height", "int")
		this.SetStructCapacity()
		
		if (Param.MaxIndex()=1) and (Param[1].__class=this.__class)
			this.CopyFrom(Param[1])
		else if (Param.MaxIndex()=2) {
			this.width  := Param[1]
			this.height := Param[2]
		}
	}
}

;-------------------------------------------------------------------------------------
; http://msdn.microsoft.com/en-us/library/dd162897%28VS.85%29.aspx 
class CRect extends CStruct_Base
{
	; CRect(CRect)
	; CRect(CPoint, CPoint)
	; CRect(CPoint, CSize)
	; CRect(CPoint, right, bottom)
	; CRect(left, top, right, bottom)
	__New(Param*)
	{
		this.AddStructVar("left",   "int")
		this.AddStructVar("top",    "int")
		this.AddStructVar("right",  "int")
		this.AddStructVar("bottom", "int")
		this.SetStructCapacity()
		
		if (Param[1].__class=this.__class) {
			this.CopyFrom(Param[1])
		}
		else if (Param[1].__class="CPoint") and (Param[2].__class="CPoint") {
			this.left := Param[1].x,   this.top := Param[1].y, this.right := Param[2].x,   this.bottom := Param[2].y
		}
		else if (Param[1].__class="CPoint") and (Param[2].__class="CSize") {
			this.left := Param[1].x,   this.top := Param[1].y, this.right := Param[1].x + Param[2].width, this.bottom := Param[1].y + Param[2].height
		}
		else if (Param[1].__class="CPoint") and (Param[2].__class="") and (Param[3].__class="")
			this.left := Param[1].x,   this.top := Param[1].y, this.right := Param[2],   this.bottom := Param[3]
		else if (Param.MaxIndex()=4) {
			this.left := Param[1], this.top := Param[2], this.right := Param[3], this.bottom := Param[4]
		}
	}
	
	__Get(name)
	{
		if name=width
			return Abs(this.right - this.left)
		if name=height
			return Abs(this.bottom - this.top)
	}
	
	GetSizeOfRect()
	{
		return new CSize(this.width, this.height)
	}
	
	ToString()
	{
		return base.Tostring() "`, " "*width=" this.width "`, *height=" this.height
	}
}

;-------------------------------------------------------------------------------------
;http://msdn.microsoft.com/en-us/library/windows/desktop/ms632610(v=vs.85).aspx
class CWindowInfo extends CStruct_Base
{
	__New()
	{
		this.AddStructVar("cbSize", "DWORD")
		this.AddStructVar("rcWindow", "CRect")    ;<-- Use CRect class.(already defined class)
		this.AddStructVar("rcClient", "CRect")    ;<-- Use CRect class.
		this.AddStructVar("dwStyle", "DWORD")
		this.AddStructVar("dwExStyle", "DWORD")
		this.AddStructVar("dwWindowStatus", "DWORD")
		this.AddStructVar("cxWindowBorders", "UINT")
		this.AddStructVar("cyWindowBorders", "UINT")
		this.AddStructVar("atomWindowType", "ATOM")
		this.AddStructVar("wCreatorVersion", "WORD")
		this.SetStructCapacity()
	}
}

;-------------------------------------------------------------------------------------
;http://msdn.microsoft.com/en-us/library/windows/desktop/ms632611(v=vs.85).aspx
class CWindowPlacement extends CStruct_Base
{
	__New()
	{
		this.AddStructVar("length", "UINT")
		this.AddStructVar("flags", "UINT")
		this.AddStructVar("showCmd", "UINT")
		this.AddStructVar("ptMinPosition", "CPoint")  ;<-- Use CPoint class.(already defined class)
		this.AddStructVar("ptMaxPosition", "CPoint")
		this.AddStructVar("rcNormalPosition", "CRect")
		this.SetStructCapacity()
	}
}

;-------------------------------------------------------------------------------------
;http://msdn.microsoft.com/en-us/library/windows/desktop/ms724950(v=vs.85).aspx
class CSystemTime extends CStruct_Base
{
	__New()
	{
		this.AddStructVar("wYear", "WORD")
		this.AddStructVar("wMonth", "WORD")
		this.AddStructVar("wDayOfWeek", "WORD")
		this.AddStructVar("wDay", "WORD")
		this.AddStructVar("wHour", "WORD")
		this.AddStructVar("wMinute", "WORD")
		this.AddStructVar("wSecond", "WORD")
		this.AddStructVar("wMilliseconds", "WORD")
		this.SetStructCapacity()
	}
}

;-------------------------------------------------------------------------------------
;http://msdn.microsoft.com/en-us/library/windows/desktop/ms725481(v=vs.85).aspx
class CTimeZoneInformation extends CStruct_Base
{
	__New()
	{
		this.AddStructVar("Bias", "LONG")
		this.AddStructVar("StandardName", "WCHAR", 32)
		this.AddStructVar("StandardDate", "CSystemTime")
		this.AddStructVar("StandardBias", "LONG")
		this.AddStructVar("DaylightName", "WCHAR", 32)
		this.AddStructVar("DaylightDate", "CSystemTime")
		this.AddStructVar("DaylightBias", "LONG")
		this.SetStructCapacity()
	}
}

;-------------------------------------------------------------------------------------
;http://msdn.microsoft.com/en-us/library/dd145065(v=vs.85).aspx
class CMoniterInfo extends CStruct_Base
{
	__New()
	{
		this.AddStructVar("cbSize", "DWORD")
		this.AddStructVar("rcMonitor", "CRect")
		this.AddStructVar("rcWork", "CRect")
		this.AddStructVar("dwFlags", "DWORD")
		this.SetStructCapacity()
		
		this.cbSize := this.size
	}
}

;-------------------------------------------------------------------------------------
;http://msdn.microsoft.com/en-us/library/dd183569(v=vs.85).aspx
;DllCall("EnumDisplayDevices", "PTR",0 , UINT,0 , Ptr,DisplayDevice[""] , UINT,0)
class CDisplayDevice extends CStruct_Base
{
	__New()
	{
		this.AddStructVar("cb", "DWORD")
		this.AddStructVar("DeviceName", "TCHAR", 32)
		this.AddStructVar("DeviceString", "TCHAR", 128)
		this.AddStructVar("StateFlags", "DWORD")
		this.AddStructVar("DeviceID", "TCHAR", 128)
		this.AddStructVar("DeviceKey", "TCHAR", 128)
		this.SetStructCapacity()
		
		this.cb := this.size
	}
}

;-------------------------------------------------------------------------------------
;http://msdn.microsoft.com/en-us/library/windows/desktop/ms684824(v=vs.85).aspx
class CPerformanceInfo extends CStruct_Base
{
	__New()
	{
		this.AddStructVar("cb", "DWORD")
		this.AddStructVar("CommitTotal", "SIZE_T")
		this.AddStructVar("CommitLimit", "SIZE_T")
		this.AddStructVar("CommitPeak", "SIZE_T")
		this.AddStructVar("PhysicalTotal", "SIZE_T")
		this.AddStructVar("PhysicalAvailable", "SIZE_T")
		this.AddStructVar("SystemCache", "SIZE_T")
		this.AddStructVar("KernelTotal", "SIZE_T")
		this.AddStructVar("KernelPaged", "SIZE_T")
		this.AddStructVar("KernelNonpaged", "SIZE_T")
		this.AddStructVar("PageSize", "SIZE_T")
		this.AddStructVar("HandleCount", "DWORD")
		this.AddStructVar("ProcessCount", "DWORD")
		this.AddStructVar("ThreadCount", "DWORD")
		this.SetStructCapacity()
	}
}

;-------------------------------------------------------------------------------------
;http://msdn.microsoft.com/en-us/library/windows/desktop/ms684839(v=vs.85).aspx
class CProcessEntry32 extends CStruct_Base
{
	__New(param*)
	{
		this.AddStructVar("dwSize", "DWORD")
		this.AddStructVar("cntUsage", "DWORD")
		this.AddStructVar("th32ProcessID", "DWORD")
		this.AddStructVar("th32DefaultHeapID", "ULONG_PTR")
		this.AddStructVar("th32ModuleID", "DWORD")
		this.AddStructVar("cntThreads", "DWORD")
		this.AddStructVar("th32ParentProcessID", "DWORD")
		this.AddStructVar("pcPriClassBase", "LONG")
		this.AddStructVar("dwFlags", "DWORD")
		this.AddStructVar("szExeFile", "TCHAR", 260)    ;MAX_PATH = 260
		this.SetStructCapacity()
		
		this.dwSize := this.size
		if (this.__class=param[1].__class)
			this.CopyFrom(param[1])
	}
}

;-------------------------------------------------------------------------------------
;http://msdn.microsoft.com/en-us/library/windows/desktop/ms684225(v=vs.85).aspx
class CModuleEntry32 extends CStruct_Base
{
	__New(param*)
	{
		this.AddStructVar("dwSize", "DWORD")
		this.AddStructVar("th32ModuleID", "DWORD")
		this.AddStructVar("th32ProcessID", "DWORD")
		this.AddStructVar("GlblcntUsage", "DWORD")
		this.AddStructVar("ProccntUsage", "DWORD")
		this.AddStructVar("modBaseAddr", "UPtr")      ;BYTE* --> UPtr
		this.AddStructVar("modBaseSize", "DWORD")
		this.AddStructVar("hModule", "HMODULE")
		this.AddStructVar("szModule", "TCHAR", 256)   ;MAX_MODULE_NAME32 + 1 = 256
		this.AddStructVar("szExePath", "TCHAR", 260)  ;MAX_PATH = 260
		this.SetStructCapacity()
		
		this.dwSize := this.size
		if (this.__class=param[1].__class)
			this.CopyFrom(param[1])
	}
}

;-------------------------------------------------------------------------------------
;http://msdn.microsoft.com/en-us/library/windows/desktop/aa373931(v=vs.85).aspx
class CGuid extends CStruct_Base
{
	__New(param*)
	{
		this.AddStructVar("Data1", "DWORD")
		this.AddStructVar("Data2", "WORD")
		this.AddStructVar("Data3", "WORD")
		this.AddStructVar("Data4", "BYTE", 8)
		this.SetStructCapacity()
		
		if (param.MaxIndex()=1)
		if Instr(param[1], "{")
			this.FromString(param[1])
		else
			this.FromProgID(param[1])
	}

	;return format ex. = {44F9A03B-A3EC-4F3B-9364-08E0007F21DF}
	ToString(ByRef noBlock=1)
	{
		noBlock := 1
		VarSetCapacity(String, 38*2+1)
		DllCall("ole32\StringFromGUID2", "UPTR",this[""], "UPTR",&String, "int",39)
		return StrGet(&String, "UTF-16")
	}
	;return format ex. = Control.TaskSymbol.1
	ToProgID()
	{
		DllCall("ole32\ProgIDFromCLSID", "UPTR",this[""], "UintP",pProgID)
		Return StrGet(pProgID, "UTF-16") , DllCall("ole32\CoTaskMemFree", "UPTR",pProgID)
	}
	
	;set class id from hex string.
	FromString(string)
	{
		string := "{" RegExReplace(string, "[{,}]", "") "}"
		VarSetCapacity(var, StrPut(string, "UTF-16")*2, 0)
		StrPut(string, &var, "UTF-16")
		DllCall("ole32\CLSIDFromString", "UPTR",&var, "UPTR",this[""])
	}
	;set class id from prog id.
	FromProgID(progID)
	{
		VarSetCapacity(var, StrPut(progID, "UTF-16")*2, 0)
		StrPut(progID, &var, "UTF-16")
		DllCall("ole32\CLSIDFromProgID", "UPTR",&var, "UPTR",this[""])
	}
}

;-------------------------------------------------------------------------------------
;http://msdn.microsoft.com/en-us/library/windows/desktop/bb773352.aspx
;DllCall( "Shell32\Shell_NotifyIcon" (A_IsUnicode ? "W" : "A") , UInt,0x1, UInt,NID[""] )
class CNotifyIconData extends CStruct_Base
{
	__New()
	{
		this.AddStructVar("cbSize", "DWORD")
		this.AddStructVar("hWnd", "HWND")
		this.AddStructVar("uID", "UINT")
		this.AddStructVar("uFlags", "UINT")
		this.AddStructVar("uCallbackMessage", "UINT")
		this.AddStructVar("hIcon", "HICON")
		this.AddStructVar("szTip", "TCHAR", 64)
		this.AddStructVar("dwState", "DWORD")
		this.AddStructVar("dwStateMask", "DWORD")
		this.AddStructVar("szInfo", "TCHAR", 256)
		this.AddStructVar("uTimeout", "UINT", "union_start")
		this.AddStructVar("uVersion", "UINT", "union_end")
		this.AddStructVar("szInfoTitle", "TCHAR", 64)
		this.AddStructVar("dwInfoFlags", "DWORD")
		this.AddStructVar("guidItem", "CGuid")        ; use CGuid class
		this.AddStructVar("hBalloonIcon", "HICON")
		this.SetStructCapacity()
		
		this.cbSize := this.size
	}
}
