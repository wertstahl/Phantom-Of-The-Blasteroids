;-----------------------------------------------------------------------------
; Player II Orb + Turret Calculations
;-----------------------------------------------------------------------------

ship2count  byte  $00
orb2count   byte  $00
orb2pospnt  byte  $3f
25hz2       byte  $00
rotdir2     byte  $00

end2anim    rts
spr2anim    lda   25hz2
            eor   #$01
            sta   25hz2
            beq   end2anim

            ;-------------------
ship2_anim  ; ship gfx animation

            dec   ship2count
            lda   ship2count
            cmp   #$ff
            bne   nsh2rs
            lda   #$07
            sta   ship2count
nsh2rs      clc
            adc   initbank+5
            sta   screen_01+1021

            ;-------------------------------------------
orb2_anim   ; orb gfx animation

            inc   orb2count
            lda   orb2count
            cmp   #$08
            bne   nor2rs
            lda   #$00
            sta   orb2count
nor2rs      clc
            adc   initbank+6
            sta   screen_01+1022

            ;-----------------------
            ; orb position animation

                  ;--------------------------
                  ; paused when shot is fired

                  lda shot2starre
                  beq goonorbit2
                  dec shot2starre
                  jmp orb2skipmov

                  ;--------------------------

goonorbit2  lda   rotdir2
            beq   thisway2

            ;------

            ldx   orb2pospnt
            dex
            cpx   #$ff
            bne   ox2plns
            ldx   #$7f
ox2plns     stx   orb2pospnt
            jmp   orb2skipmov

            ;-------------------------

thisway2    ldx   orb2pospnt
            inx
            cpx   #$80
            bne   ox2pnrs
            ldx   #$00
ox2pnrs     stx   orb2pospnt

orb2skipmov ldx   orb2pospnt            

            lda   orbx+32,x         ;+16 = sin cosine offset
            lsr
            sec
            sbc   #$18
            clc
            adc   spry+5            ;add offset to ship position
            sta   spry+6 

            ;-----------------------------------------

            lda   orbx,x
            lsr
            sec
            sbc   #$18
            clc   
            adc   sprx+5            ;add offset to ship position
            sta   sprx+6



            lda   players8         ; for intro left border underwrap
            and   #%00010000       ; check ship hi byte
            bne   skipcorr2        ; we are in high, dont care
            lda   sprx+5           ; we are in low get ship x
            bmi   skipcorr2        ; ship is in the right area, skip corr
            lda   sprx+6           ; ship is in the left area
            clc
            adc   #$30
            bcc   skipcorr2

            lda   #$00  
            sta   sprx+6

skipcorr2

            ;-----------------------

            lda   players8
            and   #%10111111        ;initially remove ext bit 
            sta   players8

            lda   players8
            and   #%00010000
            beq   ship2lo

            lda   #$90
            sec
            sbc   sprx+6
            bcc   do2turret

            lda   players8
            ora   #%01000000        ;set orb x-ext
            sta   players8



sh2noorbclip
            jmp   do2turret

ship2lo     lda   sprx+5
            sec
            sbc   #$c0              ;a simple way to determine if 
            bcc   do2turret         ;the ship is in low
            lda   sprx+6            ;without being in low of the ext area
            bmi   do2turret         ;for this bmi to work

            lda   players8
            ora   #%01000000        ;set orb x-ext
            sta   players8
            
            ;------------------------------------------------
            ; turret angle animation
            ; (relative to orb position)

do2turret   lda   orb2pospnt
            eor   #$7f
            lsr
            lsr

            sec
            sbc   #$07
            and   #$1f

            clc
            adc   initbank
 
            sta   screen_01+1020

            rts

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------