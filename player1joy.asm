;--------------------------------------------------------------------------------------
; CHECK JOYSTICK PORT I = PLAYER 1
;--------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------
; Freudenstab1

joy1last    byte  $00
trig1_fire  byte  $00
stick1_l    byte  $00
stick1_r    byte  $00
stick1_d    byte  $00
stick1_u    byte  $00
player1x    byte  $00
player1y    byte  $00

;--------------------------------------------------------------------------------------

joycontrol1 lda   #$00
            sta   stick1_l
            sta   stick1_r
            sta   stick1_u
            sta   stick1_d
            sta   trig1_fire

            jsr   do_chkj1

            lda   stick1_r     
            beq   st1r
            jsr   orb1_rotr
            jsr   do_pl1_rite

st1r        lda   stick1_l
            beq   st1d
            jsr   orb1_rotl
            jsr   do_pl1_left

st1d        lda   stick1_u
            beq   st1u
            jsr   do_pl1_up

st1u        lda   stick1_d
            beq   st1nomov
            jsr   do_pl1_down

st1nomov    lda   player1x
            sta   sprx+1      ;ship x
            sta   sprx        ;cannon x
            lda   player1y
            sta   spry+1      ;ship y
            sta   spry        ;cannon y

            rts

            ;-----------------------------------

do_chkj1    lda   $dc01           ;check joystick I
            sta   joy1last
            tax

            and   #%00010000      ;joy1 fire
            bne   nojf1 
            inc   trig1_fire

nojf1       txa 
            and   #%00000100      ;joy1 left
            bne   nojl1
            inc   stick1_l  

nojl1       txa
            and   #%00001000      ;joy1 right
            bne   nojr1
            inc   stick1_r  

nojr1       txa
            and   #%00000010      ;joy1 down
            bne   nojd1
            inc   stick1_d 

nojd1       txa
            and   #%00000001      ;joy1 up
            bne   chkj1_exit
            inc   stick1_u    

chkj1_exit  rts

            ;---------------------------------------------
            ; Player 1 Movement Mechanics

do_pl1_left lda   players8    ;check if xext bit is set     
            and   #%00000010  ;player xext is bit 2
            bne   p1x_do_dec  ;it is set! just decrease!
            lda   player1x    ;it is not set! check if we hit left border!
            tay               ;not set, first store value
            sec
            sbc   min_x       ;limit reached?
            bcs   p1x_do_dec  ;no, decrease

                  lda   intromode
                  beq   p1x_halt
                  jmp   p1x_wrap

p1x_halt    lda   player1x    ;limit reached, hold position
            bcc   p1x_dundec   ;and exit

p1x_do_dec  lda   player1x    ;load value (first time here)
            sec
            sbc   movespeed   ;if we go to minus, carry will be cleared 
            bcs   p1x_dundec  ;no minus, all good.
            tay               ;backup value
            lda   players8    ;we went minus. xext must be unset   
            and   #%11111100  ;kill xext bit
            sta   players8       
            tya               ;get value back
p1x_dundec  sta   player1x    ;store!
            rts

            ;---------------------------------------------------------

do_pl1_rite lda   players8    ;check if xext bit is set     
            and   #%00000011  ;player xext is bit 2
            beq   p1x_do_inc  ;its not set! just increase!
            lda   player1x     
            tax
            sec               
            sbc   max_x       
            bcc   p1x_do_inc
            txa
            bcs   p1x_done_inc ;maximum reached= no action

p1x_do_inc  lda   player1x
            tax
            clc
            adc   movespeed    ;if we go to minus-wrap, carry will be set 
            bcc   p1x_done_inc ;no wrap, all good.
            tax                ;oh pardon, sind sie der fuerst von cleverstein      
            lda   players8     
            ora   #%00000011  ;set xext bit
            sta   players8            
            txa
p1x_done_inc sta   player1x
            rts

            ;--------------------------------------------------------------

do_pl1_down lda   player1y     
            tax
            sec               
            sbc   max_y              
            bcc   p1y_do_inc   
            txa
            bcs   p1y_done_inc
            
p1y_do_inc  lda   player1y
            clc
            adc   movespeed   
p1y_done_inc sta   player1y     
            rts   

            ;---------------------------------------------------------------

do_pl1_up   lda   player1y
            tax
            sec
            sbc   min_y       
            bcs   p1y_do_dec    
            txa    
            bcc   p1y_done_dec

p1y_do_dec  txa     
            sec
            sbc   movespeed   
p1y_done_dec sta   player1y
            rts   

;--------------------------------------------------------------------------------------

p1x_wrap    lda   i_max_x
            sta   player1x

            lda   players8     
            ora   #%00000011  ;set xext bit
            sta   players8           

            rts
            
;--------------------------------------------------------------------------------------

orb1_rotr   lda   #$00
            sta   rotdir1
            rts

;--------------------------------------------------------------------------------------

orb1_rotl   lda   #$01
            sta   rotdir1
            rts

;--------------------------------------------------------------------------------------

;            ;--------flipmode------------
;            ; level range is 0 to 128 while 0 to 32 and 64 to 128 is on the left side, 
;            ; due to animation choices, we need to shift this 45 degrees clockwise
;            ; so we can determine easily if the gun is pointed to the left or the right
;            ; this does that:

;flipmode1   lda   orb1pospnt
;            clc
;            adc   #$80
;            clc
;            adc   #$20
;            bcc   nound1
;            sbc   #$80
;nound1      sec  
;            sbc   #$80
;            lsr  
;            lsr
;            lsr
;            lsr
;            lsr
;            lsr
;            rts

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------