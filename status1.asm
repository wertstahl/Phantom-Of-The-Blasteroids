;--------------------------------------------------------------------------------------
; SHIP STATUS PLAYER 1
;--------------------------------------------------------------------------------------

; cols
; 00 blk  01 wht  02 red  03 cyn  04 pur  05 grn  06 blu  07 yel 
; 08 lbr  09 brn  0A pnk  0B dgr  0C mgr  0D lgn  0E lbl  0F lgr

chrg1col    byte  0
plotstat1
            ldx   #$27
pls1loop    lda   status1,x
            sta   $0400,x
            lda   #$02
            sta   $d800,x
            dex
            bpl   pls1loop

            lda   #$06
            sta   chrg1col

            ldx   shot1avail
            beq   exchrg1
            cpx   #$08
            bne   sh1full

            lda   #$02
            sta   chrg1col

sh1full     lda   chargchar1
            sta   $041c,x
            lda   chrg1col
            sta   $d81c,x
            dex
            bne   sh1full

exchrg1     ldx   p1energy
            lda   chargchar1
disp1nrg    sta   $040c,x
            dex
            bne   disp1nrg

            rts
            
                  ;                1               2
                  ;0123456789abcdef0123456789abcdef01234567
                  ;0123456789012345678901234567890123456789
status1     text  'player1  nrg         <  shot         <  '
                  ;             ||||||||        ||||||||

chargchar1  text  '#'


;------------------------------------------------------------------------------------

shmarkoutlo = $fe
shmarkouthi = $ff


shipmarker1 lda shot1flies    ;calculate y = beginning of screen line
            beq   shm1dont
            lda   spry+3
            lsr
            lsr
            lsr
            tay
            dey
            dey
            dey
            dey
            dey
            bpl   shm1_nocorr
            rts
shm1_nocorr lda   screenloclo,y
            sta   shmarkoutlo
            lda   screenlochi,y
            clc
            adc   #$d4
            sta   shmarkouthi

            ;calculate x = which row of the line
            lda   players8
            and   #%00001000
            lsr
            lsr
            lsr
            ror
            lda   sprx+3
            ror
            lsr
            lsr
            tay
            dey
            dey
            bpl   shm1_nocorr2
            rts
shm1_nocorr2

            ;pour the sugar
            lda   #$01
            tax
            sta   (shmarkoutlo),y
shm1dont    rts

;------------------------------------------------------------------------------------

shm1_hit    lda   player1y
            lsr
            lsr
            lsr
            tay
            dey
            dey
            dey
            dey
            dey
            dey
            bpl   shm1_hitnc
            ldy   #$00
shm1_hitnc  lda   screenloclo,y
            sta   shmarkoutlo
            lda   screenlochi,y
            clc
            adc   #$d4
            sta   shmarkouthi

            ;calculate x = which row of the line
            lda   players8
            and   #%00000010
            lsr
            ror
            lda   player1x
            ror
            lsr
            lsr
            tay
            dey
            dey
            dey
            dey
            bpl   shm1_hitnc2
            ldy   #$00

shm1_hitnc2
            ;pour the sugar
            lda   #$01
            tax
            sta   (shmarkoutlo),y
            iny
            iny
            iny
            sta   (shmarkoutlo),y
            tax

            tya
            clc
            adc   #$4d
            tay

            txa
            sta   (shmarkoutlo),y
            iny
            iny
            iny
            sta   (shmarkoutlo),y

shm1hitno   rts


;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------
