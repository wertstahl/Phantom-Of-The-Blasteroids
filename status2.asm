;--------------------------------------------------------------------------------------
; SHIP STATUS PLAYER 2
;--------------------------------------------------------------------------------------

; cols
; 00 blk  01 wht  02 red  03 cyn  04 pur  05 grn  06 blu  07 yel 
; 08 lbr  09 brn  0A pnk  0B dgr  0C mgr  0D lgn  0E lbl  0F lgr

chrg2col    byte  0
plotstat2
            ldx   #$27
pls2loop    lda   status2,x
            sta   $07c0,x
            lda   #$05
            sta   $dbc0,x
            dex
            bpl   pls2loop

            lda   #$06
            sta   chrg2col

            ldx   shot2avail
            beq   exchrg2
            cpx   #$08
            bne   sh2full

            lda   #$05
            sta   chrg2col

sh2full     lda   chargchar2
            sta   $07dc,x
            lda   chrg2col
            sta   $dbdc,x
            dex
            bne   sh2full

exchrg2     ldx   p2energy
            lda   chargchar2
disp2nrg    sta   $07cc,x
            dex
            bne   disp2nrg

            rts
            
                  ;c               d               e
                  ;0123456789abcdef0123456789abcdef01234567
                  ;0123456789012345678901234567890123456789
status2     text  'player2  nrg         <  shot         <  '
                  ;             ||||||||        ||||||||

chargchar2  text  '#'

;------------------------------------------------------------------------------------

shmarkoutlo = $fe
shmarkouthi = $ff


shipmarker2 lda shot2flies    ;calculate y = beginning of screen line
            beq   shm2dont
            lda   spry+7
            lsr
            lsr
            lsr
            tay
            dey
            dey
            dey
            dey
            dey
            bpl   shm2_nocorr
            rts
shm2_nocorr lda   screenloclo,y
            sta   shmarkoutlo
            lda   screenlochi,y
            clc
            adc   #$d4
            sta   shmarkouthi

            ;calculate x = which row of the line
            lda   players8
            and   #%10000000
            clc
            rol
            
            lda   sprx+7
            ror
            lsr
            lsr
            tay
            dey
            dey
            bpl   shm2_nocorr2
            rts

shm2_nocorr2
            ;pour the sugar
            lda   #$01
            tax
            sta   (shmarkoutlo),y
shm2dont    rts

;------------------------------------------------------------------------------------

shm2_hit    lda   player2y
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
            bpl   shm2_hitnc
            ldy   #$00
shm2_hitnc  lda   screenloclo,y
            sta   shmarkoutlo
            lda   screenlochi,y
            clc
            adc   #$d4
            sta   shmarkouthi

            ;calculate x = which row of the line
            lda   players8
            and   #%00100000
            lsr
            lsr
            lsr
            lsr
            lsr
            ror
            lda   player2x
            ror
            lsr
            lsr
            tay
            dey
            dey
            dey
            dey
            bpl   shm2_hitnc2
            ldy   #$00

shm2_hitnc2
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

shm2hitno   rts



;------------------------------------------------------------------------------------