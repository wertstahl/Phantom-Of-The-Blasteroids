
;--------------------------------------------------------------------------------------
; Shot Calculations
;--------------------------------------------------------------------------------------

;Generalized Values
shotrspeed       byte $10    ;shot recharge time
shotavailt       = #$08      ;shot recharge multiplier f.e. 8 x shotrspeed
shotstarreval    = #$20      ;how long is the turret arrested during firing
shotspeed        byte  $06   ;shot movement speed

;--------------------------------------------------------------------------------------
; Player I Shot Calculations
;--------------------------------------------------------------------------------------

shot1x            byte  $00
shot1hi           byte  $00
shot1y            byte  $00

shot1avail        byte  $00
shot1rechrg       byte  $03

shot1flies        byte  $00
shot1dir          byte  $00
shot1step         byte  $00   ;shot movement calculation step
shot1starre       byte  $00


shot1ex     rts

shoz1fired  lda   shot1flies
            bne   shot1_do

            lda   shot1avail  ;is the shot recharged?
            cmp   shotavailt
            beq   tryfire1

            dec   shot1rechrg ;shot recharge counter
            bne   shot1ex  
            lda   shotrspeed  ;reset shot recharge time counter
            sta   shot1rechrg

            inc   shot1avail  ;shot recharge counter multiplier
            lda   shot1avail
            cmp   shotavailt
            bne   shot1ex

tryfire1    lda   trig1_fire
            beq   shot1ex

            ;--------------------------------------------------------
            ; shot can be fired, setup bullet

            ; get angle

            lda   #$7f              ;128 circular steps
            sec
            sbc   orb1pospnt        ;fetch shot direction
            
            sta   shot1dir          ;store value

            ;---------
            ;get shot position origin 

            lda   players8
            and   #%00000001       ;get hibit of turret (shot originates from turret!)
            asl
            asl
            asl
            sta   shot1hi          ;shothi stores initial hi-ext value
            ora   players8
            sta   players8

            lda   sprx             ;get turret position
            clc
            adc   #$03
            sta   shot1x
            bcc   getshot1y

            lda   #%00001000
            sta   shot1hi            ;shothi stores initial hi-ext value
            ora   players8
            sta   players8

getshot1y   lda   spry
            sta   shot1y

            ;-----
            ; finish up

            lda   #$00
            sta   shot1avail
            inc   shot1flies

            lda   primerval
            sta   primer1

            lda   shotstarreval
            sta   shot1starre 

            lda   #$d0
            sta   initbank+3
            sta   screen_01+1019

            rts                     ; done ------------------------------------------

            ;------------------------------------------------------------------------
            ; process the shot 

primer1     byte $00
shcount1    byte $00

shot1_do    lda primer1
            beq skippr1

            dec primer1
            bne skippr1
            inc armed1


skippr1     ldx shotspeed
            stx shcount1

lp_sh1      jsr shot1_spd
            dec shcount1        
            bne lp_sh1
            rts

shot1_spd   lda shot1dir
            sta shottempdir
            lda shot1step
            sta shottempstp
            jsr mk_advancevalx ; requires shottempdir shottempstp - generates advanceval

            ;---------
            ; do evaluation of direction and set hibit accordingly

            lda   advanceval   ;fetch respective step value

            clc

            adc   shot1x       ;CARRY IS BEING SET OR CLEARED NORMALLY
            sta   shot1x       ;calculate new shot position
            tay

            lda   advanceval   ;fetch respective step value again
            bpl   xadd1        ;is positve? -> addition

            lda   shot1hi      ;is the shot in extended space?
            bne   xsub1inhigh  ;yes, check hi values
                               ;no, check low values
            lda   shot1x
            sec
            sbc   #$09         ;are we below f? (inside border) 
            bcc   removeshot1  ;yes removeshot

            ;--------

xsub1inhigh bcs   doshot1y     ;did we wrap to low? no - just shoot
flipxhi1    lda   shot1hi      ;else flip the hi bit
            eor   #%00001000
            sta   shot1hi

            lda   players8
            and   #%11110111
            ora   shot1hi
            sta   players8
            jmp   doshot1y     ;and go on

            ;---------

xadd1       lda   shot1hi      ;are we in high?
            bne   xadd1inhi    ;yes

            bcs   flipxhi1     ;if we wrapped to high, flip the bit (reuse^)!
            bcc   doshot1y     ;didnt wrap, just go on

            ;----------

xadd1inhi   lda   shot1x       ;get shot x value
            sec
            sbc   #$4f         ;are we in valid space?
            bcs   removeshot1  ;nope, remove 

            ;-----------
            ;vertical shot flight processing

doshot1y    lda   shot1dir
            sta   shottempdir
            lda   shot1step
            sta   shottempstp
            jsr   mk_advancevaly ;requires shottempdir shottempstp - generates advanceval

            lda   advanceval

            clc   
            adc   shot1y
            sta   shot1y
            tay                  ;keep value

            ;----
            ;check upper border 

            sec
            sbc   #$25
            bcc   removeshot1

            ;-----
            ;check lower border 

            tya                     ;get value back
            sbc   #$f0
            bcc   finshot1fl

            ;--------------
            ;carry is set, position seems higher than f0
            ;thus remove the shot 

removeshot1 lsr   shot1flies
            lda   #$00
            sta   shot1x
            sta   shot1y
            sta   sprx+3
            sta   spry+3
            sta   armed1
            sta   shot1step
           
            sta   trig1_fire

            lda   #$9f
            sta   initbank+3
            sta   screen_01+1019

            lda   players8
            and   #%11110111
            sta   players8

            rts

            ;-----------------------
            ; complete shot trace cycle

finshot1fl  lda   shot1x
            sta   sprx+3

            lda   shot1y
            sta   spry+3

            inc   shot1step
            lda   shot1step
            and   #$1f

            sta   shot1step
endshot1    rts


;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------