;----------------------------------------------------------------------------------------------------
; Colorfading taken from Flashbang
;----------------------------------------------------------------------------------------------------

; usage: jsr initcolfade   to initialize

;        jsr colorfade     to fade colors, palettes will be switched all 256 calls

;        fading range is hardcoded and optimized, check to have best screen coverage

;        and #$0f not needed due to large palette 

;---------------------- farben fading nach tabelle  -----------------------------------

colorfade   ldx   #$00
cf_loop     ldy   $d828,x
            beq   cf_skip1
            lda   colmatrx,y
            sta   $d828,x
cf_skip1    ldy   $d928,x
            beq   cf_skip2
            lda   colmatrx,y
            sta   $d928,x
cf_skip2    ldy   $da28,x
            beq   cf_skip3
            lda   colmatrx,y
            sta   $da28,x
cf_skip3    inx
            bne   cf_loop

            ldx   #$00
cf_loop2    ldy   $db28,x
            beq   cf_skip4
            lda   colmatrx,y
            sta   $db28,x
cf_skip4    inx
            cpx   #$98
            bne   cf_loop2
            
            jsr   swtcol

            rts

;--------------------------------  reset/init    ----------------------------------------

initcolfade lda #$ff
            sta swtdelay+1
            lda #$00
            sta palettecount+1
            jsr colorsorter1
            rts

;-------------  umschalten der aktuellen fadingtabelle   --------------------------------

swtcol      dec swtdelay+1      ;verzögerungstimer runterzählen
swtdelay    lda #$ff            ;überlauf?
            cmp #$ff
            beq palcop          ;ja, -> palette neu kopieren
            rts
                        
palcop      inc palettecount+1  ;ja, palettenpointer +1
palettecount lda #$00           ;palettenpointer laden
            cmp #$01            ;palette 1 ?
            bne pal02           ;nein next test
            jmp colorsorter1    ;palettensortierer 1 anspringen
pal02       cmp #$02            ;palette 2 ?
            bne pal03           ;nein next test
            jmp colorsorter2    ;palettensortierer 2 anspringen
pal03       cmp #$03            ;palette 3 ?
            bne pal04           ;nein also palette 4
            jmp colorsorter3    ;palettensortierer 3 anspringen
pal04       lda #$00            ;palettenpointer
            sta palettecount+1  ;zurücksetzen
            jmp colorsorter4    ;palettensortierer 4 anspringen

;--------------------------- verläufe umschalten ------------------------------

colorsorter1 lda #$00
            sta colmatrx
            ldx #$00
            lda palette1,x
            ldy #$01
            sta colmatrx,y

morecs1     tax
            iny
            cpy #$0f
            beq colorfuck
            lda palette1,y
            sta colmatrx,x
            jmp morecs1

;-----------------------------------------

colorsorter2 lda #$00
            sta colmatrx
            ldx #$00
            lda palette2,x
            ldy #$01
            sta colmatrx,y
morecs2     tax
            iny
            cpy #$0f
            beq colorfuck
            lda palette2,y
            sta colmatrx,x
            jmp morecs2

;-----------------------------------------

colorsorter3 lda #$00
            sta colmatrx
            ldx #$00
            lda palette3,x
            ldy #$01
            sta colmatrx,y
morecs3     tax
            iny
            cpy #$0f
            beq colorfuck
            lda palette3,y
            sta colmatrx,x
            jmp morecs3

;-----------------------------------------

colorsorter4 lda #$00
            sta colmatrx
            ldx #$00
            lda palette4,x
            ldy #$01
            sta colmatrx,y
morecs4     tax
            iny
            cpy #$0f
            beq colorfuck
            lda palette4,y
            sta colmatrx,x
            jmp morecs4
 
;-----------------------------------------
            
colorfuck   ldx #$00            ;damit man gepflegt auch aus d800 +++ lesen kann, 
cofulp      lda colmatrx,x      ;müssen die werte sich leider 16x wiederholen
            sta colmatrx1,x     ;weil beim lesen das high nibble randomisiert wird
            sta colmatrx2,x     ;und ein eor in der sortierroutine des 
            sta colmatrx3,x     ;fadings viel zu viel zeit verschwenden würde
            sta colmatrx4,x
            sta colmatrx5,x
            sta colmatrx6,x
            sta colmatrx7,x
            sta colmatrx8,x
            sta colmatrx9,x
            sta colmatrxa,x
            sta colmatrxb,x
            sta colmatrxc,x
            sta colmatrxd,x
            sta colmatrxe,x
            sta colmatrxf,x   
            inx
            cpx #$10
            bne cofulp  
            rts       

;----------------------------------- tabellen ------------------------------------------------------
            ;     00  01  02  03  04  05  06  07  08  09  0a  0b  0c  0d  0e  0f
colmatrx     byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx1    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx2    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx3    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx4    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx5    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx6    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx7    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx8    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrx9    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxa    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxb    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxc    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxd    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxe    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
colmatrxf    byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

;---------------- paletten ------------------------------------------------------------------
            ; es sind nur 0e werte, es gibt keine 01, da diese immer als init gesetzt wird.
            ; das erste byte der späteren palette ist auch immer 0, weil 0=0
;palette     byte  $0d,$07,$0f,$03,$0a,$0c,$05,$0e,$04,$06,$02,$08,$0b,$09,$00 ;e-groden-sorting
;palette     byte  $0d,$07,$0f,$03,$0e,$05,$0a,$0c,$04,$08,$02,$06,$0b,$09,$00 ;optisorting gray opt-palette   

            ; NEU! Paletten können jetzt als Verlauf angelegt werden, 
            ; hellste Farbe (weiss unnötig, wird immer gesetzt) zuerst.
palette1     byte  $0f,$03,$0c,$06,$0b,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;chrome
palette2     byte  $0d,$07,$0f,$03,$0e,$05,$0a,$0c,$04,$08,$02,$06,$0b,$09,$00 ;optisorting rgb opt-palette
palette3     byte  $0d,$03,$0e,$0c,$06,$0b,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;cobalt
palette4     byte  $07,$0f,$0a,$08,$02,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;magma

;--------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------
