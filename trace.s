;	move.l	$24.w,oldtrace
;	move.l	$80.w,oldtrap
;
;	move.l	#trace,$24.w	
;	move.l	#super,$80.w
;	trap	#0
;
;	move.l	oldtrace,$24.w
;	move.l	oldtrap,$80.w
;	rts
;
;oldtrace	dc.l	0
;oldtrap		dc.l	0
;
;super:	add.l	#1,dada
;	move.l	2(sp),a0
;	eor.w	#$a000,sr
;
;	nop
;
;	jmp	(a0)
;
;dada	dc.l	0
;
;trace:	add.l	#1,dodo
;	and.w	#$7fff,(sp)
;	move.w	(sp),status
;	rte
;dodo	dc.l	0
;status	dc.w	0

;	movem.l	d0-a6,-(sp)
;	move.l	a7,savesp
;	bsr.w	initregs
;	bsr.w	clearmem
;	bsr.w	initregs
;	move.l	savesp,a7
;	movem.l	(sp)+,d0-a6
;	rts

; Dx.l + Ax.l + USP.l
; SR.w + EX.w + PC.w
; CRC32 

; prologue
;
; d0 = $00000000
; d1 = $11112233
; d2 = $44445566
; d3 = $77778899
; d4 = $aaaabbcc
; d5 = $ddddeeff
; d6 = $01234567
; d7 = $89abcdef
;
; a0 = $00001000
; a1 = $00001100
; a2 = $00001200
; a3 = $00001300
; a4 = $00001400
; a5 = $00001500
; a6 = $00001600
; a7 = $00001700
;
; CCR = 0
;
		jmp	S

START_OPCODE	= $0000
OPCODE_COUNT	= $10000

REG_An_DELTA	= $100

	section data,bss_c
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
REG_A0 = BASE_ADDRESS+REG_An_DELTA*0
REG_A1 = BASE_ADDRESS+REG_An_DELTA*1
REG_A2 = BASE_ADDRESS+REG_An_DELTA*2
REG_A3 = BASE_ADDRESS+REG_An_DELTA*3
REG_A4 = BASE_ADDRESS+REG_An_DELTA*4
REG_A5 = BASE_ADDRESS+REG_An_DELTA*5
REG_A6 = BASE_ADDRESS+REG_An_DELTA*6
REG_A7 = BASE_ADDRESS+REG_An_DELTA*7
REG_USP= BASE_ADDRESS+REG_An_DELTA*8


;		bra.b	S
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

		
;		move.l	#Super,$80.w
;		trap	#0			; switch to super


;		rts


;Super:

		move.l	#START_OPCODE,d0	; start opcode
		move.l	#OPCODE_COUNT-1,d7
		lea	Results,a4

.writeOp	move.w	d0,p
		move.w	d0,$dff180
;		bsr.b	Execute			; Execute one instruction
		move.l	#Execute,$80.w
		trap	#0			; switch to super

		move.w	InstructionSize(pc),d1
		beq.b	.notLegal
		move.w	d0,(a4)+
;		subq.w	#2,d1
;		adda.w	d1,a4

.notLegal
		addq.l	#1,d0
		dbf	d7,.writeOp

		rts


;
;		lea	$0.w,a0			; assume VBR = 0
;		lea	vbrsave(pc),a1
;		moveq.l	#32-1,d7
;.copyvbr	move.l	(a0)+,(a1)+
;		dbf	d7,.copyvbr
;
;;		move.l	#super,$80.w
;;		trap	#0			; switch to super
;
;;		move.l	traces(pc),d0
;;		move.l	illegals(pc),d1
;
;		lea	0.w,a0
;		lea	vbrsave(pc),a1
;		moveq.l	#32-1,d7
;.restorevbr	move.l	(a1)+,(a0)+
;		dbf	d7,.restorevbr
;		rts
;
;vbrsave	ds.l	32
;

;super:
;	
;	move.w	#3,ccr
;	move.l	#illegal,$10.w
;;	move.l	#S,$80.w	; assume VBR = 0
;;	trap	#0		; switch to super
;
;	moveq.l	#0,d0
;	movec	d0,cacr
;
;	move.l	#trace,$24.w
;
;	move.l	#$d400,d0
;	moveq.l	#0,d7
;op	move.w	d0,p
;
;	jmp	instruction
;
;next:	addq.l	#1,d0
;	addq.l	#1,d7
;	cmp.l	#$1,d7
;	bne.b	op
;
;	rte
;
;
;savesp	dc.l	0
;
;instruction:
;	move.w	#0,step
;	movem.l	d0-a6,-(sp)
;	move.l	a7,savesp
;	bsr.w	initregs
;	bsr.w	clearmem
;	bsr.w	initregs
;	or	#$a000,sr
;	nop
;	nop
;p	dc.l	0,0,0,0,0
;
;
;step	dc.w	0
;
;trace:
;	tas.b	step
;	beq.b	.out
;	and.w	#$2fff,(sp)
;
;; status regs
;; instr length
;
;	move.l	#compare,2(sp)
;	add.l	#1,traces
;.out	rte
;	
;traces:	ds.l	8
;
;illegal:
;	add.l	#1,illegals
;	move.l	#compare,2(sp)
;	rte
;
;illegals:	dc.l	0


;initregs:
;
;		move.l	(a7)+,REG_A7-4
;		move.l	#REG_D0,d0
;		move.l	#REG_D1,d1
;		move.l	#REG_D2,d2
;		move.l	#REG_D3,d3
;		move.l	#REG_D4,d4
;		move.l	#REG_D5,d5
;		move.l	#REG_D6,d6
;		move.l	#REG_D7,d7
;		movea.l	#REG_A0,a0
;		movea.l	#REG_A1,a1
;		movea.l	#REG_A2,a2
;		movea.l	#REG_A3,a3
;		movea.l	#REG_A4,a4
;		movea.l	#REG_A5,a5
;		movea.l	#REG_A6,a6
;		movea.l	#REG_A7-4,a7
;
;		move	#0,ccr
;
;		rts
;
;clearmem:	move.w	#$1000/16-1,d7
;		move.l	(a7)+,d1
;.clear:
;	rept 4
;		move.l	d0,(a0)+
;		move.l	d0,(a1)+
;		move.l	d0,(a2)+
;		move.l	d0,(a3)+
;		move.l	d0,(a4)+
;		move.l	d0,(a5)+
;		move.l	d0,(a6)+
;		move.l	d0,(a7)+
;	endr
;		dbf	d7,.clear
;		movea.l	d1,a1
;		jmp	(a1)
;
;compare:
;		move	ccr,c

		


;	move.l	savesp,a7
;	movem.l	(sp)+,d0-a6
;	jmp	next
;	rts
;		rts

E:
		even



Execute:
		move.w	#$2700,sr

		movem.l	d0-a6,-(sp)
		move.l	a7,SuperStack

		move.l	USP,a0
		move.l	a0,UserStack

		moveq.l	#0,d0
		movec	d0,cacr			; all caches off

		bsr.w	SaveVectors
		bsr.w	SetupExceptionHandlers
		bsr.w	ClearMemory
		bsr.w	ClearState
		bsr.w	SetupRegisters
;		or	#$8000,sr		; enable trace
;		and	#$dfff,sr
		eor	#$a000,sr		; enable trace + switch to usermode
;		nop

p:		dc.l	0,0,0,0,0


Cleanup:	
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
		move.l	a7,RegState_A7
		move.l	USP,a0
		move.l	a0,RegState_USP

		move.l	ProgramCounter(pc),d0
		sub.l	#p,d0
		move.w	d0,InstructionSize

		bsr.w	RestoreVectors

		move.l	UserStack(pc),a0
		move.l	a0,USP

		move.l	SuperStack(pc),a7
		movem.l	(sp)+,d0-a6
		rte

DUMMY		dc.w	0

SuperStack	dc.l	0
UserStack	dc.l	0


ClearState	lea	State(pc),a0
		moveq.l	#(StateEnd-State)/4-1,d7
.clear		move.l	#0,(a0)+
		dbf	d7,.clear
		rts

		cnop	0,4
State:	
VectorState:	dc.l	0
TrapState:	dc.w	0
StatusRegister	dc.w	0
ProgramCounter:	dc.l	0
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
RegState_A7	dc.l	0
RegState_USP	dc.l	0
InstructionSize:dc.w	0
		cnop	0,4
StateEnd:

; {0} = "memory" ; {12-20} = IPM+CCR
; USP?
; Memory?
; other 

SetupRegisters:
		move.l	(a7)+,REG_A7-4
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
		movea.l	#REG_A7-4,a7

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
		movea.l	#REG_A7,a7

.clear:
	rept 4
		move.l	d0,(a0)+
		move.l	d0,(a1)+
		move.l	d0,(a2)+
		move.l	d0,(a3)+
		move.l	d0,(a4)+
		move.l	d0,(a5)+
		move.l	d0,(a6)+
		move.l	d0,(a7)+	; USP
		move.l	d0,(a7)+	; SSP
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

		lea	$0.w,a0

;		moveq.l	#12-1,d7
;		lea	Vector0(pc),a1
;.initVectors:	move.l	a1,(a0)+
;		adda.w	#Vector1-Vector0,a1		
;		dbf	d7,.initVectors

		lea	$80.w,a0

		moveq.l	#16-1,d7
		lea	Trap0(pc),a1
.initTraps:	move.l	a1,(a0)+
		adda.w	#Trap1-Trap0,a1		
		dbf	d7,.initTraps

;		move.l	SystemVectors+31*4,31*4

		move.l	#Vector2,(2*4).w			; bus
		move.l	#Vector3,(3*4).w			; address
		move.l	#Vector4,(4*4).w			; illegal
		move.l	#Vector5,(5*4).w			; div/0
		move.l	#Vector6,(6*4).w			; chk
		move.l	#Vector7,(7*4).w			; trapv
		move.l	#Vector8,(8*4).w			; privilege
		move.l	#Vector9,(9*4).w			; trace
		move.l	#Vector10,(10*4).w			; lineA
		move.l	#Vector11,(11*4).w			; lineF

		rts

ExceptionHandler:
	; TODO - detect 000 bus/address exception frame ; NON-STANDARD!

		move.w	(sp),StatusRegister
		move.l	2(sp),ProgramCounter

		btst	#1,VectorState+2
		beq.b	.notTrace

		btst	#7,(sp)
		bne.b	.notTrace

		rte

.notTrace	add.l	#1,ExceptionCounter

		suba.l	#8,sp			; create new exception frame
		move.w	8(sp),(sp)		; copy SR
		move.l	10(sp),2(sp)		; copy PC
		move.w	#$0000,6(sp)		; indicate 4-word stack frame

;		or.w	#$2000,(sp)		; re-enable SUPER
;		and.w	#$7fff,(sp)		; disable TRACE
		move.l	#Cleanup,2(sp)		; 
		or.w	#$2000,(sp)		; enable super
		and.w	#$2fff,(sp)		; disable trace
		rte

ExceptionCounter	dc.l	0

CreateVector:	MACRO
		ori.l	#1<<(\1),VectorState
		bra.w	ExceptionHandler
	        ENDM
VectorStart:
Vector0:	CreateVector	0
Vector1:	CreateVector	1
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
;Vector12:	CreateVector	12
;Vector13:	CreateVector	13
;Vector14:	CreateVector	14
;Vector15:	CreateVector	15
;Vector16:	CreateVector	16
;Vector17:	CreateVector	17
;Vector18:	CreateVector	18
;Vector19:	CreateVector	19
;Vector20:	CreateVector	20
;Vector21:	CreateVector	21
;Vector22:	CreateVector	22
;Vector23:	CreateVector	23
;Vector24:	CreateVector	24
;Vector25:	CreateVector	25
;Vector26:	CreateVector	26
;Vector27:	CreateVector	27
;Vector28:	CreateVector	28
;Vector29:	CreateVector	29
;Vector30:	CreateVector	30
;Vector31:	CreateVector	31

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
VectorEnd:

	section opc,bss_f

Results		ds.w	OPCODE_COUNT
