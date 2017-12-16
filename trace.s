
; Fixed:
;	BASE_ADDRESS - 96k
;
; Sandbox = 128k = $20000 bytes :
; 	BASE_ADDRESS - 64k
; 	BASE_ADDRESS = $30000 
; 	BASE_ADDRESS + 64k

;; ----------------------------------------------------------------------------

STANDALONE	= 1
RECORD		= 1
START_OPCODE	= $0000
OPCODE_COUNT	= $100

;; ----------------------------------------------------------------------------


; Preamble - System specific startup

	IFEQ	STANDALONE
		lea	Opcode,a0
	ENDC
	IFEQ	RECORD
		lea	Opcode,a0
	ENDC

	IFNE	STANDALONE
		bsr.b	AmigaPreamble
		bsr.w	AmigaSave
		rts
	ENDC
;; ----------------------------------------------------------------------------

AmigaPreamble	pea	.cleanup(pc)
		pea	.run(pc)
		lea	.zerovbr(pc),a5
.super		move.l	$4.w,a6
		jmp	-30(a6)			; Supervisor()
	; Disable()/Forbid() and copy VBR to $0
.zerovbr
		move.l	a0,-(sp)
		move.l	#ALLOCABS_SIZE,d0
		move.l	#BASE_ADDRESS-$10000,a1
		jsr	-204(a6)		; AllocAbs
		lea	.mem(pc),a0
		move.l	d0,(a0)
	IFNE	STANDALONE
		move.l	#OPCODE_COUNT*16,d0
		moveq.l	#0,d1			; MEMF_ANY
		jsr	-198(a6)		; AllocMem
		lea	.mem2(pc),a0
		move.l	d0,(a0)
		move.l	d0,(sp)
	ENDC
		move.l	(sp)+,a0
		jsr	-120(a6)		; Disable()
		btst.b	#0,$129(a6)		; AttnFlags
		beq.b	.novbr
		movec	vbr,d0
		lea	.oldvbr(pc),a5
		move.l	d0,(a5)
		beq	.done
		movea.l	d0,a5
		lea	$0.w,a6
		moveq.l	#256/2-1,d7
.copyvbr	move.l	(a5)+,(a6)+
		move.l	(a5)+,(a6)+
		dbf	d7,.copyvbr
.done
		movec	cacr,d0			; we assume either 000 or 020+ (not 010)
		lea	.oldcacr(pc),a5
		move.w	d0,(a5)
		moveq.l	#0,d0
		movec	d0,cacr			; all caches off
		movec	d0,vbr
.novbr		rte
.run
		moveq.l	#-1,d0
		tst.l	.mem(pc)
		beq.b	.nomem
		addq.l	#1,d0
		tst.l	a0
		beq.b	.nomem
		jmp	S
.nomem		move.l	d0,d1
		rts
	; tail cleanup
.cleanup:	lea	.restorevbr(pc),a5
		bra.w	.super
	; set VBR back to whatever it was, and Enable()/Permit()
.restorevbr	movem.l	d0-d1,-(sp)
		move.l	.oldvbr(pc),d0
		beq.b	.alreadyzero
		movec	d0,vbr
.alreadyzero	jsr	-126(a6)		; Enable()
		move.l	.mem(pc),d0
		beq.b	.skip
		move.l	d0,a1
		move.l	#ALLOCABS_SIZE,d0
		jsr	-210(a6)		; FreeMem()
	IFNE	STANDALONE
		move.l	.mem2(pc),d0
		beq.b	.skip
		move.l	d0,a1
		move.l	#OPCODE_COUNT*16,d0
		jsr	-210(a6)		; FreeMem()
	ENDC		
.skip		movem.l	(sp)+,d0-d1
		rte
.oldvbr		dc.l	0
.oldcacr	dc.w	0,0
.mem		dc.l	0
.mem2		dc.l	0

AmigaSave	movem.l	d0-a6,-(sp)

		move.l	a0,a5
		move.l	d0,d5

		move.l	$4.w,a6
		lea	.dos(pc),a1
		jsr	-408(a6)		; OldOpenLibrary()
		tst.l	d0
		beq	.failLib
		move.l	d0,a6

		lea	.file(pc),a1
		move.l	a1,d1
		move.l	#1006,d2		; MODE_NEWFILE
		jsr	-30(a6)			; Open()
		move.l	d0,d4
		beq.b	.failOpen
		
		move.l	d4,d1
		move.l	a5,d2
		move.l	d5,d3
		lsl.l	#4,d3
		jsr	-48(a6)			; Write()
		move.l	d4,d1
		jsr	-360(a6)		; Flush()
		move.l	d4,d1
		jsr	-36(a6)			; Close()

		bra.b	.done

.failOpen	jsr	-132(a6)

.done		move.l	a6,a1		
		move.l	$4.w,a6
		jsr	-414(a6)		; CloseLibrary()

.failLib	movem.l	(sp)+,d0-a6
		rts

.dos		dc.b	"dos.library",0
.file		dc.b	"RAM:opcode.bin",0
		even

;; ----------------------------------------------------------------------------

REG_An_DELTA	= $100
REG_An_SANDBOX	= REG_An_DELTA*$10
REG_An_START	= 1	; start offset in the sandbox

BASE_ADDRESS	= $30000
ALLOCABS_SIZE	= 2*$10000+$10000  ; 64k +/- BASE_ADDRESS + CODE = 64kB

CODE_START	= BASE_ADDRESS+$10000+$8000

REG_D0 = $00000000
REG_D1 = $11112233
REG_D2 = $44445566
REG_D3 = $77778899
REG_D4 = $aaaabbcc
REG_D5 = $ddddeeff
REG_D6 = $01234567
REG_D7 = $89abcdef
REG_A0 = BASE_ADDRESS+REG_An_DELTA*(REG_An_START+0)
REG_A1 = BASE_ADDRESS+REG_An_DELTA*(REG_An_START+1)
REG_A2 = BASE_ADDRESS+REG_An_DELTA*(REG_An_START+2)
REG_A3 = BASE_ADDRESS+REG_An_DELTA*(REG_An_START+3)
REG_A4 = BASE_ADDRESS+REG_An_DELTA*(REG_An_START+4)
REG_A5 = BASE_ADDRESS+REG_An_DELTA*(REG_An_START+5)
REG_A6 = BASE_ADDRESS+REG_An_DELTA*(REG_An_START+6)
REG_USP= BASE_ADDRESS+REG_An_DELTA*(REG_An_START+7)
REG_SSP= BASE_ADDRESS+REG_An_DELTA*(REG_An_START+8+1)	; SSP separation

ILLEGAL_OP	= $4AFC
BRA_LOOP	= $60FC
JMP_LONG	= $4EF9
MOVE_SR		= $46FC

		cnop	0,4

;; ----------------------------------------------------------------------------

S		movem.l	d2-a6,-(sp)

	; Clear memory around the 'sandbox' ; BASE_ADDRESS +/- 64K

		move.l	#BASE_ADDRESS-64*1024,a1
		moveq.l	#0,d0
		move.w	#2*64*1024/32-1,d7
.clear
	rept	8
		move.l	d0,(a1)+
	endr
		dbf	d7,.clear

		move.l	a0,a1

		moveq.l	#RECORD,d0
		beq.b	.skipRecord

	; Record mode 		
		move.l	#START_OPCODE,d0	; start opcode
		move.w	#OPCODE_COUNT-1,d7
.tryOp
		move.w	d0,$dff180		; some visual feedback (COLOR00 - background)

		moveq.l	#$0000,d1		; USER mode, CCR = $0
		bsr.w	Execute			; Execute opcode d0.w in SR mode d1.w

		bsr.w	WriteCompactState
		adda.w	#16,a0

		addq.l	#1,d0
		tst.w	d0
		dbeq	d7,.tryOp
	; ^^^

.skipRecord

	; Validate mode
		move.l	#START_OPCODE,d0	; start opcode
		move.w	#OPCODE_COUNT-1,d7
		moveq.l	#0,d2

.validateOp	cmp.w	(a1),d0			; find the matching opcode in the table
		blo.b	.nextop
		beq.b	.ok

		adda.w	#16,a1
		bra.b	.validateOp
.nextop		addq.l	#1,d0
		bra.b	.validateOp

.ok		move.w	d0,$dff182		; some visual feedback (COLOR01 - text)

		moveq.l	#$0000,d1		; USER mode, CCR = $0
		bsr.b	Execute			; Execute opcode d0.w in SR mode d1.w

		lea	.single(pc),a0
		bsr.w	WriteCompactState

		btst	#4,5(a0)		; Vector state = offset 4 (word)
		bne.b	.notLegal
		addq.l	#1,d2
.notLegal
		cmpm.l	(a0)+,(a1)+
		bne.w	.error
		cmpm.l	(a0)+,(a1)+
		bne.w	.error
		cmpm.l	(a0)+,(a1)+
		bne.w	.error
		cmpm.l	(a0)+,(a1)+
		bne.w	.error
		
		addq.l	#1,d0
		tst.w	d0
		dbeq	d7,.validateOp

		move.l	d2,d1			; number of valid opcodes
	; ^^^

		move.l	#OPCODE_COUNT,d0
.escape		movem.l	(sp)+,d2-a6
		rts

.single		ds.l	4
		dc.l	0,0,0,0

.error	;	move.l	#-1,d1
	;	bra.b	.escape
		jmp	ERROR

;; ----------------------------------------------------------------------------

; d0.w = opcode
; d1.w = SR
Execute:
		move.l	#.execute,$80.w		; Execute one instruction
		trap	#0			; switch to SUPER

		btst	#0,VectorState		; check for PRIV violation
		beq.b	.done

		cmp.w	#$4e70,d0		; $4e70 = RESET
		beq.b	.done

		or.w	#$2000,d1		; run in SUPER
		bra.b	Execute

.done		rts

.execute	move.w	#$2700,sr

		move.w	#MOVE_SR,InitSR
		move.w	d0,P
		move.w	#JMP_LONG,Done
		move.l	#Cleanup,Done+2

		move.w	#ILLEGAL_OP,ERROR
		move.l	#BRA_LOOP,ERROR+2

		movem.l	d0-a6,-(sp)

		eor.w	#$1000,sr		; enable ISP
		move.l	a7,a0
		eor.w	#$1000,sr		; disable ISP
		move.l	a0,-(sp)

		move.l	USP,a0
		move.l	a0,-(sp)

		move.l	a7,SystemStack

		move.w	#0,ccr			; clear CCR
		move.w	sr,d2			; get full SR
		eor.w	#$a000,d1		; enable trace + switch to usermode
		eor.w	d2,d1			; keep original SR bits
		move.w	d1,InitSR+2		; patch opcode

		bsr.w	SaveVectors
		bsr.w	SetupExceptionHandlers
		bsr.w	ClearMemory
		bsr.w	ClearState
		bsr.w	SetupRegisters

	; Record the memory state
		move.l	a7,SuperStack
		move.l	SystemStack(pc),a7
		bsr	CalcMemoryChecksum
		move.l	SuperStack(pc),a7

		jmp	InitSR

Cleanup:	move.l	d0,RegState_D0
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
		move.l	USP,a0
		move.l	a0,RegState_USP

		move.l	ProgramCounter(pc),d0
		sub.l	#P,d0
		cmp.l	#32,d0			; let's assume no instruction is longer than 32bytes ;)
		blo.b	.sizeOk
		move.w	#$ffff,d0
.sizeOk
		move.w	d0,InstructionSize

		move.l	SystemStack(pc),a7

		move.l	(sp)+,a0
		move.l	a0,USP

		move.l	(sp)+,a0
		eor.w	#$1000,sr		; enable ISP
		move.l	a0,a7
		eor.w	#$1000,sr		; disable ISP

		bsr.b	CalcMemoryChecksum
		bsr.w	RestoreVectors

		movem.l	(sp)+,d0-a6
		rte


ClearState	lea	State(pc),a0
		moveq.l	#(StateEnd-State)/4-1,d7
.clear		move.l	#0,(a0)+
		dbf	d7,.clear
		rts

;; ----------------------------------------------------------------------------

CalcMemoryChecksum
		movem.l	d0-a6,-(a7)

	; Reset state
		moveq.l	#-1,d0
		bsr	FeedCyclicSum

	; Feed in the exception vectors
		lea	0.w,a0
		move.l	#48*4,d0
		bsr	FeedCyclicSum

	; Feed in the *user* sandbox (8 An) - i.e. not including SSP!

		lea	BASE_ADDRESS,a0
		move.l	#REG_An_DELTA*(REG_An_START+8),d0
		bsr	FeedCyclicSum

		eor.l	d0,MemoryState

		movem.l	(a7)+,d0-a6
		rts

CalcSuperStackChecksum
		movem.l	d1-a6,-(a7)

	; Reset state w/ seed
		move.l	RegState_SSP,FeedCyclicSum\.crc

	; Feed in the *super* - i.e. only the SSP area!

		lea	REG_SSP-REG_An_DELTA,a0
		move.l	#REG_An_DELTA*2,d0
		bsr	FeedCyclicSum

		movem.l	(a7)+,d1-a6
		rts

FeedCyclicSum:	cmp.l	#-1,d0
		bne.b	.start

		move.l	d0,.crc
		bra.w	.out

.start		movem.l	d1-d5/a0,-(sp)

		lsr.l	#2,d0
		move.l	.crc(pc),d1
		moveq.l	#$1,d3
		move.l	#$edb88320,d4
		bra.b	.go
.calc:
		move.l	(a0)+,d2
		eor.l	d2,d1

		move.l	d1,d5
		and.l	d3,d5
		neg.l	d5

		lsr.l	d1
		and.l	d4,d5

		eor.l	d5,d1

.go		subq.l	#1,d0
		bpl.b	.calc

		move.l	d1,.crc
		move.l	d1,d0

		movem.l	(sp)+,d1-d5/a0

.out:		not.l	d0
		rts

.crc	dc.l	0


;; ----------------------------------------------------------------------------

WriteCompactState
	; a0.l = state
	;
	; compare current state against the 'ideal' state
	; output 16 bytes compact mode state
	;
	; opcode	WORD
	; registers	WORD
	; vectors	LONG
	; status	WORD
	; opcode size	WORD
	; memory check	WORD
	; stach sum	WORD
	;
		movem.l	d0-d1,-(sp)

		move.w	d0,(a0)				; current opcode

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

		move.w	d0,2(a0)			; register state

		move.w	VectorState(pc),4(a0)		; vector state
		move.w	TrapState(pc),6(a0)		; trap state

		move.w	StatusRegister(pc),8(a0)	; status register
		move.w	InstructionSize(pc),10(a0)	; opcode length (or -1)

		move.w	MemoryState(pc),12(a0)		; user memory checksum

		bsr.w	CalcSuperStackChecksum
		move.w	d0,14(a0)			; super stack checksum

		movem.l	(sp)+,d0-d1
		rts

;; ----------------------------------------------------------------------------

SetupRegisters:
		move.l	(a7)+,.return+2
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
		movea.l	#REG_SSP,a7		; == ISP
		eor.w	#$1000,sr		; disable ISP
		movea.l	#REG_SSP,a7		; == SSP

		; Fake exception frame

		move.w	#$2700,REG_SSP
		move.l	#P+2,REG_SSP+2

		move	#0,ccr
.return		jmp	$0.l

;; ----------------------------------------------------------------------------

ClearMemory:
		movem.l	d0-d4/a0-a4,-(sp)

		moveq.l	#0,d0
		moveq.l	#0,d1
		moveq.l	#0,d2
		moveq.l	#0,d3
		move.l	d0,a0
		move.l	d1,a1
		move.l	d2,a2
		move.l	d3,a3

		lea	BASE_ADDRESS+REG_An_SANDBOX,a4
		move.w	#REG_An_SANDBOX/256-1,d4
.clear
	rept	8
		movem.l	d0-d3/a0-a3,-(a4)	; write (4+4)*4 = 32bytes
	endr
		dbf	d4,.clear

		movem.l	(sp)+,d0-d4/a0-a4
		rts

;; ----------------------------------------------------------------------------

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


;; ----------------------------------------------------------------------------

SetupExceptionHandlers:

	; Install our custom handlers (Exceptions + Traps)

		moveq.l	#10-1,d7
		lea	$8.w,a0
		lea	Vector2(pc),a1
.initVectors:	move.l	a1,(a0)+		; Vector2 - Vector11
		adda.w	#Vector3-Vector2,a1
		dbf	d7,.initVectors
 

		moveq.l	#16-1,d7
		lea	$80.w,a0
		lea	Trap0(pc),a1
.initTraps:	move.l	a1,(a0)+		; Vector32 - Vector47
		adda.w	#Trap1-Trap0,a1
		dbf	d7,.initTraps

	; Wrap all (48) vectors so we have defined/known memory contents at address $0

		lea	$0.w,a0
		lea	WrapHandlers,a1
		moveq.l	#48-1,d7
.initWraps
		; Vector0 - Vector47
		move.w	#JMP_LONG,(a1)
		move.l	(a0),2(a1)
		move.l	a1,(a0)+
		addq.l	#6,a1

		dbf	d7,.initWraps

		rts

Counter		dc.l	0
DumpStack	ds.l	32

		cnop	0,4
ExceptionHandler:
		add.l	#1,Counter
		move.l	0(sp),DumpStack+0
		move.l	4(sp),DumpStack+4
		move.l	8(sp),DumpStack+8
		move.l	12(sp),DumpStack+12
		move.l	16(sp),DumpStack+16
		move.l	20(sp),DumpStack+20
		move.l	24(sp),DumpStack+24

		btst	#2,VectorState+1	; Bus error
		bne.b	.busOrAddrErr
		btst	#3,VectorState+1	; Address error
		bne.b	.busOrAddrErr

.notBusError68k
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

		cmp.l	#P+0,2(sp)
		beq.b	.notTrace
		cmp.l	#P+2,2(sp)
		beq.b	.notTrace
		cmp.l	#P+4,2(sp)
		beq.b	.notTrace
		cmp.l	#P+6,2(sp)
		beq.b	.notTrace

		rte				; ignore this exception and continue

.notTrace
		suba.l	#8,sp			; create new exception frame
		move.w	8(sp),(sp)		; copy SR
		move.l	10(sp),2(sp)		; copy PC
		move.w	#$0000,6(sp)		; indicate 4-word stack frame

.busError68k	or.w	#$2000,(sp)		; re-enable SUPER
		and.w	#$2fff,(sp)		; disable TRACE
		move.l	#Done,2(sp)		; .. and bounce back
		rte

.busOrAddrErr	btst	#6,(sp)			; T0 - not used
		beq.b	.notBusError68k		; not a 68000 exception frame

		suba.l	#6,sp			; create new exception frame
		move.w	14(sp),(sp)		; copy SR
		move.l	16(sp),2(sp)		; copy PC

		move.w	(sp),StatusRegister
		move.l	2(sp),ProgramCounter

		bra.b	.busError68k


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


		cnop	0,4

SuperStack	dc.l	0
SystemStack	dc.l	0
SystemVectors:	ds.l	32+16


;; ----------------------------------------------------------------------------

		cnop	0,4
State:	
VectorState:	dc.w	0
TrapState:	dc.w	0
StatusRegister	dc.w	0
InstructionSize:dc.w	0
ProgramCounter:	dc.l	0
MemoryState	dc.l	0
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
RegState_USP	dc.l	0
RegState_SSP	dc.l	0
		cnop	0,4
StateEnd:

E:

;; ----------------------------------------------------------------------------
	; ORG this separately so that code changes don't ripple through
;; ----------------------------------------------------------------------------
		org	CODE_START

ERROR		dc.w	0	; illegal			; will jump here if the check fails
		dc.w	0	; bra.b	ERROR

		org	CODE_START+$100
WrapHandlers
	rept	48
		dc.w	0,0,0	;jmp	$0.l
	endr

		org	CODE_START+$1000
InitSR:		dc.w	0,0	;move.w	#$ffff,sr		; dummy write - smc
P:		dc.l	0,0,0,0,0,0

		org	CODE_START+$1100
Done:		dc.w	0,0,0	;jmp	$0.l

;; ----------------------------------------------------------------------------


	IFEQ	STANDALONE
	section opc,bss_f
Opcode		ds.b	OPCODE_COUNT*16	; 4 longs per op, in compact mode

	ELSE
	
	IFEQ	RECORD
	section opc,data_f
Opcode		incbin	"sys:opcode.020nofpu.bin"

	ENDC

	ENDC