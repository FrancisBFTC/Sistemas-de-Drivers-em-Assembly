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

init_pci:
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
; os_pci_read_reg -- Read from a register on a PCI device
;  IN:	BL  = Bus number
;	CL  = Device/Slot/Function number
;	DL  = Register number (0-15)
; OUT:	EAX = Register information
;	All other registers preserved
os_pci_read_reg:
	push edx
	push ecx
	push ebx

	shl ebx, 16			; Move Bus number to bits 23 - 16
	shl ecx, 8			; Move Device/Slot/Fuction number to bits 15 - 8
	mov bx, cx
	shl edx, 2
	mov bl, dl
	and ebx, 0x00ffffff		; Clear bits 31 - 24
	or ebx, 0x80000000		; Set bit 31
	mov eax, ebx
	mov dx, PCI_ADDRESS
	out dx, eax
	mov dx, PCI_DATA
	in eax, dx

	pop ebx
	pop ecx
	pop edx
ret
; -----------------------------------------------------------------------------


	
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
	
	xor 	eax, eax
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
	
	xor 	eax, eax
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
	push 	ax
	mov 	ax, word[Vendor]
	call 	Print_Hexa_Value16
	mov 	ax, 0x0E20
	int 	0x10
	mov 	ax, 0x00
	int 	0x16
	pop 	ax
	; -----------------------------------
		
	;call 	PCI_Check_Function
	call 	PCI_Get_HeaderType
	and 	word[Header], 0x80  
	cmp 	word[Header], 0        ; Se bit 7 estiver setado então
	jz  	ReturnDevice           ; Não É um dispositivo multi-função
Multi_Func_Dev:                    ; É um.. multi
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
		push 	ax
		mov 	si, SigMsg
		call 	Print_String
		mov 	ax, word[Vendor]
		call 	Print_Hexa_Value16
		mov 	ax, 0x00
		int 	0x16
		pop 	ax
	    ; -----------------------------------
	
		inc 	cl
		jmp 	Loop_Check_Functions
	
DeviceNoExist:
	stc
ReturnDevice:
	popad
ret
SigMsg   	db " -",0
MsgChkFun 	db "|No Function|    ",13,10,0
NameBus 	db "Bus: ",0



PCI_Check_Function:
	pushad
	
	popad
ret


; -----------------------------------------------------------------------------
; PCI_Check_All_Buses - Brute Force Scan - All bus, All Slots & All Possibly funcs
; IN:   None.
; OUT:	None.
;	Registradores preservados
PCI_Check_All_Buses:
	pushad
	mov 	al, 0
	loop_all_buses1:
		cmp 	al, 255        ; Ler os primeiros 4 barramentos
		jbe  	init_loop_bus
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
; OUT:	EAX = ID do Fabricante
PCI_Get_VendorID:
	push 	eax
	push 	ecx
	push 	edx

	mov 	dl, 0                  ; Offset 0 -> Fabricante
	call 	PCI_Read_Word          ; Efetua a leitura PCI
	mov 	word[Vendor], ax       ; Armazene o retorno em Vendor
	
	pop 	edx
	pop 	ecx
	pop 	eax
ret
Vendor 	dw 	0x0000
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; PCI_Get_DeviceID - Retorna o ID do dispositivo PCI
; IN:   AL  = Bus number
;		BL  = Device/Slot number
;       CL  = Função
; OUT:	EAX = ID do Dispositivo
PCI_Get_DeviceID:
	push 	eax
	push 	ecx
	push 	edx
	
	mov 	dl, 2                  ; Offset 2 -> Dispositivo
	call 	PCI_Read_Word          ; Efetua a leitura PCI
	mov 	word[Device], ax       ; Armazene o retorno em Device
	
	pop 	edx
	pop 	ecx
	pop 	eax
ret
Device 	dw 	0x0000
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; PCI_Get_HeaderType - Retorna o Tipo de Cabeçalho (Header)
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	EAX = Tipo de Cabeçalho
PCI_Get_HeaderType:
	push 	eax
	push 	ecx
	push 	edx

	mov 	dl, 14                 ; Offset 14 -> Tipo De Cabeçalho
	call 	PCI_Read_Word          ; Efetua a leitura PCI
	mov 	word[Header], ax       ; Armazene o retorno em Header
	
	pop 	edx
	pop 	ecx
	pop 	eax
ret
Header 	dw 	0x0000
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