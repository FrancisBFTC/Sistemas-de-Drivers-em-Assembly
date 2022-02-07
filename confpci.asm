; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;              FUNÇÕES DE COMUNICAÇÃO PCI PARA OS DRIVERS
;
;                     Kernel em Assembly x86
;                    Criado por Wender Francis
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%IFNDEF		__CONFPCI_ASM__
%DEFINE 	__CONFPCI_ASM__

PCI_ADDRESS		EQU	0x0CF8
PCI_DATA		EQU	0x0CFC
PCI_READ 		EQU 0x00
PCI_WRITE 		EQU 0x01

bus 	db 0
slot 	db 0
func 	db 0
offs 	db 0
pdata 	dd 0

os_PCIEnabled  db 0

PCIListStr:
		db "KiddieOS PCI List",13,10,13,10
		db "BUS   |DEV   |FUNC   |VENDOR   |DEVICE   |CLASS   |DEVICE NAME   ",13,10,0


Init_PCI:
	mov 	eax, 0x80000000		; Bit 31 set for 'Enabled'
	mov 	ebx, eax
	mov 	dx, PCI_ADDRESS
	out 	dx, eax
	in 		eax, dx
	xor 	edx, edx
	cmp 	eax, ebx
	sete 	dl				; Set byte if equal, otherwise clear
	mov 	byte [os_PCIEnabled], dl
ret


	
; -----------------------------------------------------------------------------
; PCI_Read_Word - Ler de um registro um dispositivo PCI
; IN:   AL  = Bus number
;		BL  = Device/Slot number
;		CL  = Function number
; 		DL  = Offset number
; OUT:	EAX = Register information
PCI_Read_Word:
	push 	ebx
	push 	ecx
	push 	edx
	
	shl 	eax, 16
	shl 	ebx, 11
	or 		eax, ebx
	shl 	ecx, 8
	or 		eax, ecx
	and 	edx, 0xFC
	or 	 	eax, edx
	and 	eax, 0x00FFFFFF
	or 		eax, 0x80000000
	mov 	dx, PCI_ADDRESS
	out 	dx, eax
	mov 	dx, PCI_DATA
    in 		eax, dx
	
	pop 	edx
	and 	edx, 0x02
	shl 	edx, 3          ; multiplique por 8
	mov 	ecx, edx
	shr 	eax, cl
	and 	eax, 0xFFFF
	
    pop 	ecx
	pop 	ebx
ret



; -----------------------------------------------------------------------------
; PCI_Write_Word - Escreve para um registro um dispositivo PCI
; IN:   AL  = Bus number
;		BL  = Device/Slot number
;		CL  = Function number
; 		DL  = Offset number
; OUT:	None.
PCI_Write_Word:
	push 	ebx
	push 	ecx
	push 	edx
	
	shl 	eax, 16
	shl 	ebx, 11
	or 		eax, ebx
	shl 	ecx, 8
	or 		eax, ecx
	and 	edx, 0xFC
	or 	 	eax, edx
	and 	eax, 0x00FFFFFF
	or 		eax, 0x80000000
	mov 	dx, PCI_ADDRESS
	out 	dx, eax
	mov 	dx, PCI_DATA
	mov 	eax, [pdata]
	out 	dx, eax
	
	pop 	edx
    pop 	ecx
	pop 	ebx
ret


; -----------------------------------------------------------------------------
; PCI_Check_Device - Verifica um Dispositivo PCI
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; OUT:	None.
;	Registradores preservados
PCI_Check_Device:
	pushad
	
	mov 	cl, 0          ; Função 0
	call 	PCI_Get_VendorID
	cmp 	word[Vendor], 0xFFFF
	je 		DeviceNoExist
	
	; EXIBIR INFORMAÇÕES ----------------
	call 	PCI_Show_Info
	; -----------------------------------
		
	;call 	PCI_Check_Function
	call 	PCI_Get_HeaderType
	and 	byte[Header], 0x80  
	cmp 	byte[Header], 0        ; Se bit 7 não estiver setado então
	jz  	ReturnDevice           ; Então não É um dispositivo multi-função
Multi_Func_Dev:                    ; Se tiver, É um dev multifunção
	mov 	cl, 1
	Loop_Check_Functions:
		cmp 	cl, 8
		jnb 	ReturnDevice
		
		call 	PCI_Get_VendorID
		cmp 	word[Vendor], 0xFFFF
		jne 	CheckFunction
	
		inc 	cl
		jmp 	Loop_Check_Functions
	CheckFunction:
		;call 	PCI_Check_Function
		
		; EXIBIR INFORMAÇÕES ----------------
		call 	PCI_Show_Info
		; -----------------------------------
	
		inc 	cl
		jmp 	Loop_Check_Functions
	
DeviceNoExist:
	;stc
	;mov 	si, MsgNoDev
	;call 	Print_String
ReturnDevice:
	popad
ret
SigMsg   	db " -",0
MsgChkFun 	db "|No Function|    ",13,10,0
NameBus 	db "Bus: ",0
MsgNoDev 	db "|No Device|",13,10,0


PCI_Check_Function:
	pushad
	
	popad
ret

PCI_Show_Info:
	push 	ax
	call 	Print_Dec_Value32
	call 	OffsetSpaces
	mov 	ax, bx
	call 	Print_Dec_Value32
	call 	OffsetSpaces
	mov 	ax, cx
	call 	Print_Dec_Value32
	call 	OffsetSpaces
	pop 	ax
	push 	ax
	call 	PCI_Get_DeviceID
	call 	PCI_Get_ClassCode
	mov 	ax, word[Vendor]
	call 	Print_Hexa_Value16
	mov 	ax, 0x0E20
	int 	0x10
	mov 	ax, word[Device]
	call 	Print_Hexa_Value16
	mov 	ax, 0x0E20
	int 	0x10
	mov 	ax, word[ClassCode]
	call 	Print_Hexa_Value16
	mov 	ax, 0x0E0A
	int 	0x10
	mov 	ax, 0x0E0D
	int 	0x10
	mov 	ax, 0x00
	int 	0x16
	pop 	ax
ret

OffsetSpaces:
	pushad
	mov 	cx, 1
	mov 	bx, 10
OffSpace1:
	xor 	dx, dx
	div 	bx
	cmp 	ax, 0
	je 	    RetOff	
IncVar:
	inc 	cx
	jmp 	OffSpace1
RetOff:
	mov 	ax, 0x0E20
	int 	0x10
	loop 	RetOff
	popad
ret

; -----------------------------------------------------------------------------
; PCI_Check_All_Buses - Brute Force Scan - All bus, All Slots & All Possibly funcs
; IN:   None.
; OUT:	None.
;	Registradores preservados
PCI_Check_All_Buses:
	pushad
	xor 	eax, eax
	xor 	ebx, ebx
	xor 	ecx, ecx
	xor 	edx, edx
	; EXIBIR INFORMAÇÕES ----------------
	mov 	si, PCIListStr
	call 	Print_String
	; -----------------------------------
	mov 	al, 0
	loop_all_buses1:
		cmp 	al, 255        ; Ler os primeiros 4 barramentos
		jb  	init_loop_bus
		jmp 	return_checkb
		
	init_loop_bus:
		mov 	bl, 0
	loop_all_buses2:
		cmp 	bl, 32
		jnb 	return_all_buses
		call 	PCI_Check_Device
		inc 	bl
		jmp 	loop_all_buses2
		
	return_all_buses:
		inc 	al
		jmp 	loop_all_buses1
return_checkb:
	popad
ret
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; PCI_Get_VendorID - Retorna o ID do fabricante do dispositivo PCI
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AX = ID do Fabricante
PCI_Get_VendorID:
	push 	eax

	mov 	dl, 0                  ; Offset 0 -> Fabricante
	call 	PCI_Read_Word          ; Efetua a leitura PCI
	mov 	word[Vendor], ax       ; Armazene o retorno em Vendor
	
	pop 	eax
ret
Vendor 	dw 	0x0000
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; PCI_Get_DeviceID - Retorna o ID do dispositivo PCI
; IN:   AL  = Bus number
;		BL  = Device/Slot number
;       CL  = Função
; OUT:	AX = ID do Dispositivo
PCI_Get_DeviceID:
	push 	eax
	
	mov 	dl, 2                  ; Offset 2 -> Dispositivo
	call 	PCI_Read_Word          ; Efetua a leitura PCI
	mov 	word[Device], ax       ; Armazene o retorno em Device

	pop 	eax
ret
Device 	dw 	0x0000
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; PCI_Get_Command - Retorna o valor de Comando
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AX = Comando
PCI_Get_Command:
	push 	eax

	mov 	dl, 4                   ; Offset 4 -> Command
	call 	PCI_Read_Word           ; Efetua a leitura PCI
	mov 	word[Command], ax       ; Armazene o retorno em Command

	pop 	eax
ret
Command 	dw 	0x0000
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; PCI_Get_Status - Retorna o Status
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AX = Status
PCI_Get_Status:
	push 	eax

	mov 	dl, 6                  ; Offset 6 -> Status
	call 	PCI_Read_Word          ; Efetua a leitura PCI
	mov 	word[Status], ax       ; Armazene o retorno em Status

	pop 	eax
ret
Status 	dw 	0x0000
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; PCI_Get_RevisionID - Retorna o ID de Revisão
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AL = ID de Revisão
PCI_Get_RevisionID:
	push 	eax

	mov 	dl, 8                  ; Offset 8 -> RevisionID
	call 	PCI_Read_Word          ; Efetua a leitura PCI
	mov 	byte[Revision], al     ; Armazene o retorno em Revision

	pop 	eax
ret
Revision  	db 	0x00
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; PCI_Get_ProgIF - Retorna o ProgIF
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AH = ProgIF
PCI_Get_ProgIF:
	push 	eax

	mov 	dl, 8                  ; Offset 8 -> ProgIF
	call 	PCI_Read_Word          ; Efetua a leitura PCI
	mov 	byte[ProgIF], ah       ; Armazene o retorno em ProgIF

	pop 	eax
ret
ProgIF  	db 	0x00
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; PCI_Get_SubClass - Retorna a SubClasse
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AL  = SubClass
PCI_Get_SubClass:
	push 	eax

	mov 	dl, 10                  ; Offset 10 -> SubClasse
	call 	PCI_Read_Word           ; Efetua a leitura PCI
	mov 	byte[SubClass], al      ; Armazene o retorno em SubClass

	pop 	eax
ret
SubClass  	db 	0x00
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; PCI_Get_ClassCode - Retorna o código de Classe
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AH = Código de Classe
PCI_Get_ClassCode:
	push 	eax

	mov 	dl, 10                   ; Offset 10 -> Código de Classe
	call 	PCI_Read_Word            ; Efetua a leitura PCI
	mov 	byte[ClassCode], ah      ; Armazene o retorno em ClassCode

	pop 	eax
ret
ClassCode  	db 	0x00
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; PCI_Get_CacheSize - Retorna o tamanho de linha de Cache
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AL = Cache Line Size
PCI_Get_CacheSize:
	push 	eax

	mov 	dl, 12                   ; Offset 12 -> Cache Line Size
	call 	PCI_Read_Word            ; Efetua a leitura PCI
	mov 	byte[CacheSize], al      ; Armazene o retorno em CacheSize

	pop 	eax
ret
CacheSize  	db 	0x00
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; PCI_Get_Latency - Retorna o Timer de latência
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AH = Timer de Latência
PCI_Get_Latency:
	push 	eax

	mov 	dl, 12                   ; Offset 12 -> Timer de Latência
	call 	PCI_Read_Word            ; Efetua a leitura PCI
	mov 	byte[Latency], ah        ; Armazene o retorno em Latency

	pop 	eax
ret
Latency  	db 	0x00
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; PCI_Get_HeaderType - Retorna o Tipo de Cabeçalho (Header)
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AL = Tipo de Cabeçalho
PCI_Get_HeaderType:
	push 	eax

	mov 	dl, 14                 ; Offset 14 -> Tipo De Cabeçalho
	call 	PCI_Read_Word          ; Efetua a leitura PCI
	mov 	byte[Header], al       ; Armazene o retorno em Header

	pop 	eax
ret
Header 	db 	0x00
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; PCI_Get_BIST - Retorna o Valor BIST
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AH = valor BIST
PCI_Get_BIST:
	push 	eax

	mov 	dl, 14                 ; Offset 14 -> BIST
	call 	PCI_Read_Word          ; Efetua a leitura PCI
	mov 	byte[BIST], ah         ; Armazene o retorno em BIST

	pop 	eax
ret
BIST 	db 	0x00
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; PCI_Get_Classes - Retorna a Classe base & subClasse
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AX  = Classe Base e SubClasse
PCI_Get_Classes:
	push 	eax
	
	mov 	dl, 10                   ; Offset 10 -> Base e SubClasse
	call 	PCI_Read_Word            ; Efetua a leitura PCI
	mov 	word[Classes], ax        ; Armazene o retorno em Classes

	pop 	eax
ret
Classes  	dw 	0x0000
; -----------------------------------------------------------------------------


; -----------------------------------------------------------------------------
; PCI_Get_SecondaryBus - Retorna o Barramento Secundário
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	AX  = Barramento Secundário
PCI_Get_SecondaryBus:
	push 	eax
	
	mov 	dl, 0x18                      ; Offset 24 -> Barramento Secundário
	call 	PCI_Read_Word                 ; Efetua a leitura PCI
	mov 	byte[SecondaryBus], ah        ; Armazene o retorno em SecondaryBus
	mov 	byte[PrimaryBus], al
	
	pop 	eax
ret
SecondaryBus  	db 	0x00
PrimaryBus 		db  0x00

; -----------------------------------------------------------------------------
;Configuration Mechanism One has two IO port rages associated with it.
;The address port (0xcf8-0xcfb) and the data port (0xcfc-0xcff).
;A configuration cycle consists of writing to the address port to specify which device and register you want to access and then reading or writing the data to the data port.

;ddress dd 10000000000000000000000000000000b
;          /\     /\      /\   /\ /\    /\
;        E    Res    Bus    Dev  F  Reg   0
; Bits
; 31		Enable bit = set to 1
; 30 - 24	Reserved = set to 0
; 23 - 16	Bus number = 256 options
; 15 - 11	Device/Slot number = 32 options
; 10 - 8	Function number = will leave at 0 (8 options)
; 7 - 2		Register number = will leave at 0 (64 options) 64 x 4 bytes = 256 bytes worth of accessible registers


%ENDIF

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++