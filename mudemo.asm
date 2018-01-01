	device zxspectrum128
;        ORG #4000
; incbin "drac0.scr"
        ORG #6000
begin

;������������� ������
	ld hl,song
	call stc_init

;����������� ��������
	ld hl,picp,de,$4000:call dzx7_standard
;���������� ����� ����������
	ld hl,$5800
	ld b,32
alp:
	ld (hl),$45
	inc l
	djnz alp

mu:
	ei:halt
	ld a,0:out ($FE),a
;�������� ����� �� ������
	call scroll;������� ������

long=5;�������� ������, ����� �� ����� �������
cn1: ld a,long-1
	inc a
	cp long
	jr nz,skippy

;������� ������
cn2:
	ld a,-1
	inc a
	cp 6;28;��������� ��������, ������������ 0-27
	jr nz,no_loop
	xor a
no_loop
	ld (cn2+1),a
	xor a
skippy
	ld (cn1+1),a

; �� ������ ������������ �������� � ����� ������ �����
	ld a,(cn2+1)
	ld l,a
	ld h,0
;������ �������� 4 ����� ��� ���������� �����
	add hl,hl
	add hl,hl
	ld de,framet
	add hl,de

;������� ����
	ld a,(hl)
	inc hl
	inc hl
	ld bc,$7FFD
	out (c),a
;������ ����� ��� ������
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld (cp+1),hl
cp:call 82
;��� �������� ����� �� ������, ������ �������� �������
	ld a,0:out ($FE),a
	call stc_play
	ld a,1:out ($FE),a
	jp mu

framet
 dw $11,f0
 dw $11,f1
 dw $11,f2
 dw $11,f3
 dw $11,f4
 dw $11,f5

;========================================================================
screen_adr equ #401f;screen adress for output
scroll:
 ld hl,screen_adr
 ld b,8
scroll_text:
 push hl
 xor a

 dup 32;unroll cycle
 rl(hl)
 dec l
edup

 pop hl
 inc h
 dec b
 jp nz,scroll_text

;out text, IX: string with limiter 0
scrp:ld ix,scrtxt
 ld a,(ix)
 or a
 jr nz,noloop
;if zero loop text again
 ld ix,scrtxt
 ld a,(ix)
noloop:ld (scrp+2),ix

 ld de,screen_adr
 ld l,a,h,0; calculate adress ROM font

 add hl,hl;ascii*8
 add hl,hl
 add hl,hl
 ld bc,fnt;-256
 add hl,bc
 ex de,hl


 ld b,8;one loop for letter
letrlp:
 ld a,(de)
bw:and #80
 jr z,nobit
 set 0,(hl)
nobit:
 inc de,h
 djnz letrlp

 ld a,(bw+1)
 or a
 rrca
 ld (bw+1),a
 ret nc

 ld hl,(scrp+2)
 inc hl
 ld (scrp+2),hl
 ret
scrtxt: ;db "*simple scroll* "
 incbin "scrolly.txt"
 db 0
;========================================================================
plyr:include "stc_player.a80"
;song:incbin "h4ro_tune.STC"
song:incbin "VVS.STC";"zyn_tune.STC"
zx7:include "zx7.a80"
picp:incbin "0.scr.zx7"
fnt:incbin "casa2.fnt"
end

	page 1
	org $C000
page1b:
f0: incbin "0.bin"
f1: incbin "1.bin"
f2: incbin "2.bin"
f3: incbin "3.bin"
f4: incbin "4.bin"
f5: incbin "5.bin"
page1e:

	;display /d,end-begin
	;savesna "!void.sna",begin

;���������
;-----------------------------------------------------------
 macro	sectors datab,datae

 IF low (datae-datab)=0
   db 1,5,high (datae-datab)
 ELSE
   db 1,5,(1+high (datae-datab))
 ENDIF

	endm

 MODULE boot
Basic:
 db #00,#01;����� ������
 DW EndLine1 - Line1
Line1:
 db #EA;REM
 ld sp,#5FFE

    res 4,(iy+1)
    xor a:out ($FE),a
    ld hl,$5AFF,de,$5AFe,bc,$1B00-1,(hl),0:lddr

;demo
 di
 ld de,(#5CF4)
 ld hl, #6000
 ld a,#10,bc,#7FFD:out (c),a
 sectors begin,end
 call #3d13

 di
 ld de,(#5CF4)
 ld hl, #C000
 ld a,#11,bc,#7FFD:out (c),a
 sectors page1b,page1e
 call #3d13

 /* di
 ld de,(#5CF4)
 ld hl, #C000
 ld a,#13,bc,#7FFD:out (c),a
 sectors page3b,page3e
 call #3d13 */


 jp #6000


 db "yo,lamer!"
 db #0D
EndLine1:
 db #00,#02
 DW EndLine2 - Line2
Line2:
 db #20,#FD,#B0
 db #22,#32,#34,#35,#37,#35,#22;clear val "24575"
 db #3A; :
 db #F9,#C0,#28,#35;randomize usr (5+256*peek val "23635"+peek val "23636"
 db #0E,#00,#00,#05,#00,#00,#2B
 db #32,#35,#36
 db #0E,#00,#00,#00,#01,#00,#2A,#BE

 db #B0
 db #22,#32,#33,#36,#33,#36,#22;"23635"
 db #2B;???
 db #BE
 db #B0
 db #22,#32,#33,#36,#33,#35,#22;"23636"
 db #29,#0D;)
 db #80
 db #AA,1,0;;;;;;;;;;;;;autorun line,change program length to -4, e.g. 83-4=79
EndLine2:
EndBasic:
 ENDMODULE

	display /d,end-begin

    EMPTYTRD "draco.trd" ;create empty TRD image
	SAVETRD "draco.trd", "boot.B", boot.Basic, boot.EndBasic - boot.Basic
    SAVETRD "draco.trd","demo.C",begin,end-begin

    PAGE 1
    SAVETRD "draco.trd","pg1.C",page1b,page1e-page1b
    ;PAGE 3
    ;SAVETRD "draco.trd","pg3.C",page3b,page3e-page3b
;-----------------------------------------------------------
