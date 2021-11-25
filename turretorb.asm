;-----------------------------------------------------------------------------
; Player I Orb + Turret Calculations
;-----------------------------------------------------------------------------

ship1count  byte  $00
orb1count   byte  $00
orb1pospnt  byte  $3f
25hz1       byte  $00
rotdir1     byte  $00

end1anim    rts
spr1anim    lda   25hz1
            eor   #$01
            sta   25hz1
            beq   end1anim

            ;-------------------
ship1_anim  ; ship gfx animation

            inc   ship1count
            lda   ship1count
            cmp   #$08
            bne   nsh1rs
            lda   #$00
            sta   ship1count
nsh1rs      clc
            adc   initbank+1
            sta   screen_01+1017

            ;-------------------------------------------
orb1_anim   ; orb gfx animation

            inc   orb1count
            lda   orb1count
            cmp   #$08
            bne   nor1rs
            lda   #$00
            sta   orb1count
nor1rs      clc
            adc   initbank+2
            sta   screen_01+1018

            ;-----------------------
            ; orb position animation

                  ;--------------------------
                  ; paused when shot is fired

                  lda shot1starre
                  beq goonorbit1
                  dec shot1starre
                  jmp orb1skipmov

                  ;--------------------------

goonorbit1  lda   rotdir1
            beq   thisway1

            ;------

            ldx   orb1pospnt
            inx
            cpx   #$80
            bne   ox1plns
            ldx   #$00
ox1plns     stx   orb1pospnt
            jmp   orb1skipmov

            ;-------------------------

thisway1    ldx   orb1pospnt
            dex
            cpx   #$ff
            bne   ox1pnrs
            ldx   #$7f
ox1pnrs     stx  orb1pospnt

orb1skipmov ldx   orb1pospnt  
            lda   orbx+32,x         ;+32 = sin cosine offset
            lsr
            sec
            sbc   #$18
            clc
            adc   spry+1            ;add offset to ship position
            sta   spry+2 

            ;-----------------------------------------

            lda   orbx,x
            lsr
            sec
            sbc   #$18
            clc   
            adc   sprx+1            ;add offset to ship position
            sta   sprx+2


            
            lda   players8         ; for intro left border underwrap
            and   #%00000001       ; check ship hi byte
            bne   skipcorr1        ; we are in high, dont care
            lda   sprx+1           ; we are in low get ship x
            bmi   skipcorr1        ; ship is in the right area, skip corr
            lda   sprx+2           ; ship is in the left area
            clc
            adc   #$30
            bcc   skipcorr1

            lda   #$00  
            sta   sprx+2

skipcorr1



            ;-----------------------

            lda   players8
            and   #%11111011        ;initially remove ext bit 
            sta   players8

            lda   players8
            and   #%00000001
            beq   ship1lo

            lda   #$90
            sec
            sbc   sprx+2
            bcc   do1turret

            lda   players8
            ora   #%00000100        ;set orb x-ext
            sta   players8
            jmp   do1turret

ship1lo     lda   sprx+1
            sec
            sbc   #$c0              ;a simple way to determine if 
            bcc   do1turret         ;the ship is in low
            lda   sprx+2            ;without being in low of the ext area
            bmi   do1turret         ;for this bmi to work

            lda   players8
            ora   #%00000100        ;set orb x-ext
            sta   players8
            
            ;------------------------------------------------
            ; turret angle animation
            ; (relative to orb position)

do1turret   lda   orb1pospnt
            eor   #$7f
            lsr
            lsr

            sec
            sbc   #$07
            and   #$1f

            clc
            adc   initbank
 
            sta   screen_01+1016

            rts

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------