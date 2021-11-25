
;--------------------------------------------------------------------------------------
; Shot II Calculations
;--------------------------------------------------------------------------------------

;Generalized Values
;shotrspeed       = #$01      ;shot recharge time
;shotavailt       = #$08      ;shot recharge multiplier f.e. 8 x shotrspeed
;shotstarreval    = #$20      ;how long is the turret arrested during firing
;shotspeed        byte  $03   ;shot movement speed

;--------------------------------------------------------------------------------------
; Player II Shot Calculations
;--------------------------------------------------------------------------------------

shot2x            byte  $00
shot2hi           byte  $00
shot2y            byte  $00

shot2avail        byte  $00
shot2rechrg       byte  $03

shot2flies        byte  $00
shot2dir          byte  $00
shot2step         byte  $00   ;shot movement calculation step
shot2starre       byte  $00


shot2ex     rts

shoz2fired  lda   shot2flies
            bne   shot2_do

            lda   shot2avail  ;is the shot recharged?
            cmp   shotavailt
            beq   tryfire2

            dec   shot2rechrg ;shot recharge counter
            bne   shot2ex  
            lda   shotrspeed  ;reset shot recharge time counter
            sta   shot2rechrg

            inc   shot2avail  ;shot recharge counter multiplier
            lda   shot2avail
            cmp   shotavailt
            bne   shot2ex

tryfire2    lda   trig2_fire
            beq   shot2ex

            ;--------------------------------------------------------
            ; shot can be fired, setup bullet

            ; get angle

            lda   #$7f
            sec
            sbc   orb2pospnt        ;fetch shot direction
            sta   shot2dir          ;store value

            ;---------
            ;get shot position origin 

            lda   players8
            and   #%00010000        ;get hibit of turret (shot originates from turret!)
            asl
            asl
            asl
            sta   shot2hi          ;shothi stores initial hi-ext value
            ora   players8
            sta   players8

            lda   sprx+4             ;get turret position
            clc
            adc   #$03
            sta   shot2x
            bcc   getshot2y

            lda   #%10000000
            sta   shot2hi            ;shothi stores initial hi-ext value
            ora   players8
            sta   players8

getshot2y   lda   spry+4
            sta   shot2y

            ;-----
            ; finish up

            lda   #$00
            sta   shot2avail
            inc   shot2flies

            lda   primerval
            sta   primer2

            lda   shotstarreval
            sta   shot2starre 

            lda   #$d0
            sta   initbank+7
            sta   screen_01+1023

            rts                     ; done ------------------------------------------

            ;------------------------------------------------------------------------
            ; process the shot 

primer2     byte $10
shcount2    byte $0

shot2_do    lda primer2
            beq skippr2

            dec primer2
            bne skippr2
            inc armed2


skippr2     ldx shotspeed
            stx shcount2

lp_sh2      jsr shot2_spd
            dec shcount2        
            bne lp_sh2
            rts

shot2_spd   lda shot2dir
            sta shottempdir
            lda shot2step
            sta shottempstp
            jsr mk_advancevalx ; requires shottempdir shottempstp - generates advanceval

            ;---------
            ; do evaluation of direction and set hibit accordingly

            lda   advanceval   ;fetch respective step value

            clc

            adc   shot2x       ;CARRY IS BEING SET OR CLEARED NORMALLY
            sta   shot2x       ;calculate new shot position
            tay

            lda   advanceval   ;fetch respective step value again
            bpl   xadd2        ;is positve? -> addition

            lda   shot2hi      ;is the shot in extended space?
            bne   xsub2inhigh  ;yes, check hi values
                               ;no, check low values
            lda   shot2x
            sec
            sbc   #$09        ;are we below f? (inside border) 
            bcc   removeshot2  ;yes removeshot

            ;--------

xsub2inhigh bcs   doshot2y     ;did we wrap to low? no - just shoot
flipxhi2    lda   shot2hi      ;else flip the hi bit
            eor   #%10000000
            sta   shot2hi

            lda   players8
            and   #%01111111
            ora   shot2hi
            sta   players8
            jmp   doshot2y     ;and go on

            ;---------

xadd2       lda   shot2hi      ;are we in high?
            bne   xadd2inhi    ;yes

            bcs   flipxhi2     ;if we wrapped to high, flip the bit (reuse^)!
            bcc   doshot2y     ;didnt wrap, just go on

xadd2inhi   lda   shot2x       ;get shot x value
            sec
            sbc   #$4f         ;are we in valid space?
            bcs   removeshot2  ;nope, remove 

            ;-----------
            ;vertical shot flight processing

doshot2y    lda   shot2dir
            sta   shottempdir
            lda   shot2step
            sta   shottempstp
            jsr   mk_advancevaly ;requires shottempdir shottempstp - generates advanceval

            lda   advanceval

            clc   
            adc   shot2y
            sta   shot2y
            tay                  ;keep value

            ;----
            ;check upper border 

            sec
            sbc   #$25
            bcc   removeshot2

            ;-----
            ;check lower border 

            tya                     ;get value back
            sbc   #$f0
            bcc   finshot2fl

            ;--------------
            ;carry is set, position seems higher than f0
            ;thus remove the shot 

removeshot2 lsr   shot2flies
            lda   #$00
            sta   shot2x
            sta   shot2y
            sta   sprx+7
            sta   spry+7
            sta   armed2
            sta   shot2step

            sta   trig2_fire

            lda   #$9f
            sta   initbank+7
            sta   screen_01+1023

            lda   players8
            and   #%01111111
            sta   players8

            rts

            ;-----------------------
            ; complete shot trace cycle

finshot2fl  lda   shot2x
            sta   sprx+7

            lda   shot2y
            sta   spry+7

            inc   shot2step
            lda   shot2step
            and   #$1f

            sta   shot2step
endshot2    rts


;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------