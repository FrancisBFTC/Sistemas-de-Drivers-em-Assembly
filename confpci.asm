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

; -----------------------------------------------------------------------------
; PCI_Conf_Word - Ler/Escreve de/para um registro um dispositivo PCI
; IN:	AH  = Read/Write
;       AL  = Bus number
;		BL  = Device/Slot number
;		CL  = Function number
; 		DL  = Register/Offset number
; OUT:	EAX = Register information
;	Registradores preservados
PCI_Config_Word:
	push 	ebx
	push 	ecx
	push 	eax
	
	shl 	eax, 16
	shl 	ebx, 8
	or 		eax, ebx
	shl 	ecx, 8
	or 		eax, ecx
	push 	edx
	and 	edx, 0xFC
	or 	 	eax, edx
	and 	eax, 0x00FFFFFF
	or 		eax, 0x80000000
	mov 	dx, PCI_ADDRESS
	out 	dx, eax
	mov 	dx, PCI_DATA
	pop 	eax
	and 	ah, PCI_READ
	jz 		R_H
	mov 	eax, [pdata]
	out 	dx, eax
	jmp 	_EN
	
R_H:in 		eax, dx
	pop 	edx
	and 	edx, 0x02
	shl 	edx, 3          ; multiplique por 8
	mov 	ecx, edx
	shr 	eax, cl
	and 	eax, 0xFFFF
	
_EN:pop 	ecx
	pop 	ebx
ret

PCI_Check_Device:
	pushad
	
	
	popad
ret

; -----------------------------------------------------------------------------
; PCI_Get_VendorID - Retorna o ID do fabricante do dispositivo PCI
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; OUT:	EAX = ID do Fabricante
;	Registradores preservados
PCI_Get_VendorID:
	push 	ecx
	push 	edx
	
	mov 	ah, PCI_READ           ; Leitura do PCI
	mov 	cl, 0                  ; Função 0
	mov 	dl, 0                  ; Offset 0 -> Fabricante
	call 	PCI_Config_Word        ; Efetua a leitura PCI
	
	mov 	word[Vendor], eax      ; Armazene o retorno em Vendor
	
	pop 	edx
	pop 	ecx
ret
Vendor 	dw 	0x0000


; -----------------------------------------------------------------------------
; PCI_Get_DeviceID - Retorna o ID do dispositivo PCI
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; OUT:	EAX = ID do Dispositivo
;	Registradores preservados
PCI_Get_DeviceID:
	push 	ecx
	push 	edx
	
	mov 	ah, PCI_READ           ; Leitura do PCI
	mov 	cl, 0                  ; Função 0
	mov 	dl, 2                  ; Offset 2 -> Dispositivo
	call 	PCI_Config_Word        ; Efetua a leitura PCI
	
	mov 	word[Device], eax      ; Armazene o retorno em Device
	
	pop 	edx
	pop 	ecx
ret
Device 	dw 	0x0000


; -----------------------------------------------------------------------------
; PCI_Get_HeaderType - Retorna o Tipo de Cabeçalho (Header)
; IN:   AL  = Bus number
;		BL  = Device/Slot number
; 		CL  = Função
; OUT:	EAX = Tipo de Cabeçalho
;	Registradores preservados
PCI_Get_HeaderType:
	push 	ecx
	push 	edx
	
	mov 	ah, PCI_READ           ; Leitura do PCI
	mov 	dl, 0x0E               ; Offset 14 -> Tipo De Cabeçalho
	call 	PCI_Config_Word        ; Efetua a leitura PCI
	
	mov 	word[Header], eax      ; Armazene o retorno em Header
	
	pop 	edx
	pop 	ecx
ret
Header 	dw 	0x0000



;Configuration Mechanism One has two IO port rages associated with it.
;The address port (0xcf8-0xcfb) and the data port (0xcfc-0xcff).
;A configuration cycle consists of writing to the address port to specify which device and register you want to access and then reading or writing the data to the data port.

PCI_CONFIG_ADDRESS	EQU	0x0CF8
PCI_CONFIG_DATA		EQU	0x0CFC

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