; 8MB / 64K ops => 128bytes
; 8+8 32bit => 64 bytes
; USP => 4 bytes
; Vector + Trap => 4 bytes
; SR => 2 bytes
; PC 2/4/6/8/-1 => 2 bytes capped
; CRC32 mem => 4bytes
; ==> 80 bytes



; extended mode = 80 bytes

		; compare Dn + An + USP
		; Dx.l + Ax.l + USP.l
		; SR.w + EX.w + PC.w
		; CRC32 



		jmp	S

START_OPCODE	= $0241
OPCODE_COUNT	= $1

REG_An_DELTA	= $100

	section data,bss_f
ReservedMem	ds.b	REG_An_DELTA*$10

BASE_ADDRESS	= ReservedMem ;$50000

REG_D0 = $00000000
REG_D1 = $11112233
REG_D2 = $44445566
REG_D3 = $77778899
REG_D4 = $aaaabbcc
REG_D5 = $ddddeeff
REG_D6 = $01234567
REG_D7 = $89abcdef
REG_A0 = BASE_ADDRESS+REG_An_DELTA*3
REG_A1 = BASE_ADDRESS+REG_An_DELTA*4
REG_A2 = BASE_ADDRESS+REG_An_DELTA*5
REG_A3 = BASE_ADDRESS+REG_An_DELTA*6
REG_A4 = BASE_ADDRESS+REG_An_DELTA*7
REG_A5 = BASE_ADDRESS+REG_An_DELTA*8
REG_A6 = BASE_ADDRESS+REG_An_DELTA*9
REG_SSP= BASE_ADDRESS+REG_An_DELTA*10
REG_USP= BASE_ADDRESS+REG_An_DELTA*11


	section code,code_f

ZeroVBRAmiga	pea	.cleanup(pc)
		pea	S(pc)
		lea	.zerovbr(pc),a5
.super		move.l	$4.w,a6
		jmp	-30(a6)			; Supervisor()
.zerovbr	movec	vbr,d0
		lea	.oldvbr(pc),a0
		move.l	d0,(a0)
		beq	.done
		movea.l	d0,a0
		lea	$0.w,a1
		moveq.l	#256/2-1,d7
.copyvbr	move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		dbf	d7,.copyvbr
.done		moveq.l	#0,d0
		movec	d0,vbr
		rte
.cleanup:	lea	.restorevbr(pc),a5
		bra.b	.super
.restorevbr	move.l	.oldvbr(pc),d0
		movec	d0,vbr
		rte
.oldvbr		dc.l	0


S
		movem.l	d1-a6,-(sp)
		move.l	#START_OPCODE,d0	; start opcode
		move.l	#OPCODE_COUNT-1,d7
		lea	Compact,a0

.tryOp
		move.w	d0,$dff180		; some visual feedback (COLOR00)

		moveq.l	#$0000,d1		; USER mode, CCR = $0
		bsr.b	Execute			; Execute opcode d0.w in SR mode d1.w

;		move.w	VectorState(pc),d1
;		and.w	#1<<4,d1
;		bne.b	.notLegal
;		move.w	d0,(a0)+
;.notLegal

		bsr.w	WriteCompactState
		adda.w	#16,a0

		addq.l	#1,d0
		dbf	d7,.tryOp

;		suba.l	#Compact,a0
;		move.l	a0,d0
;		lsr.l	#1,d0

		movem.l	(sp)+,d1-a6
		rts



;; ----------------------------------------------------------------------------

; d0.w = opcode
; d1.w = SR

Execute:
		move.l	#.execute,$80.w		; Execute one instruction
		trap	#0			; switch to SUPER

		move.w	VectorState(pc),d1
		and.w	#1<<8,d1		; check for PRIV violation
		beq.b	.done

		cmp.w	#$4e70,d0		; $4e70 = RESET
		beq.b	.done

		or.w	#$2000,d1		; run in SUPER
		bra.b	Execute

.done		rts

.execute	move.w	#$2700,sr

		move.w	d0,.p

		movem.l	d0-a6,-(sp)
		move.l	a7,SuperStack

		eor.w	#$1000,sr		; enable ISP
		move.l	a7,InterruptStack
		eor.w	#$1000,sr		; disable ISP

		move.l	USP,a0
		move.l	a0,UserStack

		moveq.l	#0,d0
		movec	d0,cacr			; all caches off

		move.w	#0,ccr			; clear CCR
		move.w	sr,d2			; get full SR
		eor.w	#$a000,d1		; enable trace + switch to usermode
		eor.w	d2,d1			; keep original SR bits
		move.w	d1,.smc+2		; patch opcode

		bsr.w	SaveVectors
		bsr.w	SetupExceptionHandlers
		bsr.w	ClearMemory
		bsr.w	ClearState
		bsr.w	SetupRegisters

;		bra.b	Cleanup

.smc:		move.w	#$ffff,sr		; dummy write - smc
.p:		dc.l	0,0,0,0,0

a7check		dc.l	0

Cleanup:	
		move.l	a7,a7check
		; compare Dn + An + USP
		; Dx.l + Ax.l + USP.l
		; SR.w + EX.w + PC.w
		; CRC32 

		move.l	d0,RegState_D0
		move.l	d1,RegState_D1
		move.l	d2,RegState_D2
		move.l	d3,RegState_D3
		move.l	d4,RegState_D4
		move.l	d5,RegState_D5
		move.l	d6,RegState_D6
		move.l	d7,RegState_D7
		move.l	a0,RegState_A0
		move.l	a1,RegState_A1
		move.l	a2,RegState_A2
		move.l	a3,RegState_A3
		move.l	a4,RegState_A4
		move.l	a5,RegState_A5
		move.l	a6,RegState_A6
		move.l	a7,RegState_SSP
		add.l	#12,RegState_SSP	; normal RTE frame
		move.l	USP,a0
		move.l	a0,RegState_USP

		move.l	ProgramCounter(pc),d0
		sub.l	#Execute\.p,d0
		cmp.l	#32,d0			; let's assume no instruction is longer than 32bytes ;)
		blo.b	.sizeOk
		move.w	#$ffff,d0
.sizeOk		move.w	d0,InstructionSize

		move.l	UserStack(pc),a0
		move.l	a0,USP

		eor.w	#$1000,sr		; enable ISP
		move.l	InterruptStack(pc),a7
		eor.w	#$1000,sr		; disable ISP

		move.l	SuperStack(pc),a7

		bsr.w	RestoreVectors

		movem.l	(sp)+,d0-a6
		rte

SuperStack	dc.l	0
InterruptStack	dc.l	0
UserStack	dc.l	0

ClearState	lea	State(pc),a0
		moveq.l	#(StateEnd-State)/4-1,d7
.clear		move.l	#0,(a0)+
		dbf	d7,.clear
		rts


WriteCompactState
	; a0.l = state
	;
	; compare current state against the 'ideal' state
	; output 16 bytes compact mode state
	;
	; VECT|TRAP + SR|Isize + Dn|An + CRC32mem
	;
		movem.l	d0-d1,-(sp)

		move.w	VectorState(pc),(a0)
		move.w	TrapState(pc),2(a0)
		move.w	StatusRegister(pc),4(a0)
		move.w	InstructionSize(pc),6(a0)

		moveq.l	#0,d0
		moveq.l	#1,d1

CompareReg	MACRO
		cmp.l	#\1,\2
		beq.b	*+4
		or.l	d1,d0
		add.l	d1,d1
		ENDM

		CompareReg	REG_D0,RegState_D0
		CompareReg	REG_D1,RegState_D1
		CompareReg	REG_D2,RegState_D2
		CompareReg	REG_D3,RegState_D3
		CompareReg	REG_D4,RegState_D4
		CompareReg	REG_D5,RegState_D5
		CompareReg	REG_D6,RegState_D6
		CompareReg	REG_D7,RegState_D7

		CompareReg	REG_A0,RegState_A0
		CompareReg	REG_A1,RegState_A1
		CompareReg	REG_A2,RegState_A2
		CompareReg	REG_A3,RegState_A3
		CompareReg	REG_A4,RegState_A4
		CompareReg	REG_A5,RegState_A5
		CompareReg	REG_A6,RegState_A6
		CompareReg	REG_USP,RegState_USP

		move.l	d0,8(a0)

		moveq.l	#0,d0
		move.l	#$8000,d1
		CompareReg	REG_SSP,RegState_SSP
		or.w	d0,(a0)			; reuse Vector15 to signal SSP change

		move.l	#$01234567,12(a0)

		movem.l	(sp)+,d0-d1
		rts

		cnop	0,4
State:	
VectorState:	dc.w	0
TrapState:	dc.w	0
StatusRegister	dc.w	0
InstructionSize:dc.w	0
ProgramCounter:	dc.l	0
		dc.l	0	; pad
RegState_D0	dc.l	0
RegState_D1	dc.l	0
RegState_D2	dc.l	0
RegState_D3	dc.l	0
RegState_D4	dc.l	0
RegState_D5	dc.l	0
RegState_D6	dc.l	0
RegState_D7	dc.l	0
RegState_A0	dc.l	0
RegState_A1	dc.l	0
RegState_A2	dc.l	0
RegState_A3	dc.l	0
RegState_A4	dc.l	0
RegState_A5	dc.l	0
RegState_A6	dc.l	0
RegState_SSP	dc.l	0
RegState_USP	dc.l	0
		cnop	0,4
StateEnd:

SetupRegisters:
		move.l	(a7)+,REG_SSP-4
		move.l	#REG_D0,d0
		move.l	#REG_D1,d1
		move.l	#REG_D2,d2
		move.l	#REG_D3,d3
		move.l	#REG_D4,d4
		move.l	#REG_D5,d5
		move.l	#REG_D6,d6
		move.l	#REG_D7,d7

		movea.l	#REG_A0,a0
		movea.l	#REG_A1,a1
		movea.l	#REG_A2,a2
		movea.l	#REG_A3,a3
		movea.l	#REG_A4,a4
		movea.l	#REG_A5,a5
		movea.l	#REG_A6,a6
		movea.l	#REG_USP,a7
		move.l	a7,USP
		eor.w	#$1000,sr		; enable ISP
		movea.l	#REG_SSP-4,a7		; == ISP
		eor.w	#$1000,sr		; disable ISP
		movea.l	#REG_SSP-4,a7		; == SSP

		; Fake exception frame

		move.w	#$0000,REG_SSP
		move.l	#Execute\.p+2,REG_SSP+2

		move	#0,ccr
		rts

CompareRegisters:
		rts


ClearMemory:	move.w	#REG_An_DELTA/4-1,d7
		moveq.l	#0,d0
		move.l	a7,d1

		movea.l	#REG_A0,a0
		movea.l	#REG_A1,a1
		movea.l	#REG_A2,a2
		movea.l	#REG_A3,a3
		movea.l	#REG_A4,a4
		movea.l	#REG_A5,a5
		movea.l	#REG_A6,a6
		movea.l	#REG_SSP,a7

.clear:
	rept 4
		move.l	d0,(a0)+
		move.l	d0,(a1)+
		move.l	d0,(a2)+
		move.l	d0,(a3)+
		move.l	d0,(a4)+
		move.l	d0,(a5)+
		move.l	d0,(a6)+
		move.l	d0,(a7)+	; SSP
		move.l	d0,(a7)+	; USP
	endr
		dbf	d7,.clear
		movea.l	d1,a7
		rts

SaveVectors:	lea	$0.w,a0			; assume VBR = 0
		lea	SystemVectors(pc),a1
		moveq.l	#32+16-1,d7
.copyVecs	move.l	(a0)+,(a1)+
		dbf	d7,.copyVecs
		rts

RestoreVectors	lea	0.w,a0
		lea	SystemVectors(pc),a1
		moveq.l	#32+16-1,d7
.restoreVecs	move.l	(a1)+,(a0)+
		dbf	d7,.restoreVecs
		rts

SystemVectors:	ds.l	32+16


SetupExceptionHandlers:
		move.l	#$EDB88320,d0
		lea	$0.w,a0
		move.l	d0,(a0)+		; Reset SP
		move.l	d0,(a0)+		; Reset PC
		moveq.l	#10-1,d7
		lea	Vector2(pc),a1
.initVectors:	move.l	a1,(a0)+		; Vector2 - Vector11
		adda.w	#Vector3-Vector2,a1		
		dbf	d7,.initVectors

		moveq.l	#20-1,d7
.initDummies	move.l	d0,(a0)+		; Vector20 - Vector31
		dbf	d7,.initDummies

		moveq.l	#16-1,d7
		lea	Trap0(pc),a1
.initTraps:	move.l	a1,(a0)+		; Vector32 - Vector47
		adda.w	#Trap1-Trap0,a1		
		dbf	d7,.initTraps

		rts

ExceptionHandler:
	; TODO - detect 000 bus/address exception frame ; NON-STANDARD!

		move.w	(sp),StatusRegister
		move.l	2(sp),ProgramCounter

	; TRACEing CHK/TRAP will generate a TRACE exception from the CHK/TRAP exception handler
	; In this case the TRACE bit is cleared, but an exception is still generated - detect that

		btst	#1,VectorState
		beq.b	.notTrace
		btst	#7,(sp)
		bne.b	.notTrace

	; When TRACEing in SUPER, the instruction might do all sort of weirdness to SR
	; As long as the PC matches something around the instruction being TRACEd we're good

		cmp.l	#Execute\.p+0,2(sp)
		beq.b	.notTrace
		cmp.l	#Execute\.p+2,2(sp)
		beq.b	.notTrace
		cmp.l	#Execute\.p+4,2(sp)
		beq.b	.notTrace
		cmp.l	#Execute\.p+6,2(sp)
		beq.b	.notTrace

		rte				; ignore this exception and continue

.notTrace	add.l	#1,ExceptionCounter

		suba.l	#8,sp			; create new exception frame
		move.w	8(sp),(sp)		; copy SR
		move.l	10(sp),2(sp)		; copy PC
		move.w	#$0000,6(sp)		; indicate 4-word stack frame

		or.w	#$2000,(sp)		; re-enable SUPER
		and.w	#$2fff,(sp)		; disable TRACE
		move.l	#Cleanup,2(sp)		; 
		rte

ExceptionCounter	dc.l	0

CreateVector:	MACRO
		ori.w	#1<<(\1),VectorState
		bra.w	ExceptionHandler
	        ENDM

Vector2:	CreateVector	2
Vector3:	CreateVector	3
Vector4:	CreateVector	4
Vector5:	CreateVector	5
Vector6:	CreateVector	6
Vector7:	CreateVector	7
Vector8:	CreateVector	8
Vector9:	CreateVector	9
Vector10:	CreateVector	10
Vector11:	CreateVector	11

CreateTrap	MACRO
		ori.w	#1<<(\1),TrapState
		bra.w	ExceptionHandler
	        ENDM

Trap0:		CreateTrap	0
Trap1:		CreateTrap	1
Trap2:		CreateTrap	2
Trap3:		CreateTrap	3
Trap4:		CreateTrap	4
Trap5:		CreateTrap	5
Trap6:		CreateTrap	6
Trap7:		CreateTrap	7
Trap8:		CreateTrap	8
Trap9:		CreateTrap	9
Trap10:		CreateTrap	10
Trap11:		CreateTrap	11
Trap12:		CreateTrap	12
Trap13:		CreateTrap	13
Trap14:		CreateTrap	14
Trap15:		CreateTrap	15

E:

	section opc,bss_f

Compact		ds.l	OPCODE_COUNT*4	; 4 longs per op, in compact mode
