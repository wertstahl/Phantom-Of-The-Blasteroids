;-----------------------------------------------------------------------
; collision shot 2 with player 1 check
;-----------------------------------------------------------------------

collision2x1 byte  0
collision2x2 byte  0
collision2y1 byte  0
collision2y2 byte  0
stor2        byte  0

x2size1      = #$11 ;The area of the drawn sprite on the left
x2size2      = #$11 ;The area of the drawn sprite on the right
y2size1      = #$11 ;The area of the drawn sprite at the top
y2size2      = #$11 ;The area of the drawn sprite at the bottom

armed2      byte  0

collcheck2  lda   armed2
            beq   nothit2

checkimp2   lda   coll_reg
            tay
            and   #%10000000
            beq   nothit2

            tya
            and   #%10001000
            cmp   #%10001000
            beq   nothit2

hit2        jsr   removeshot2
            inc   shaketheroom

            lda   intromode
            bne   nothit1

            jsr   shm1_hit
            jsr   sfx_smash
            dec   p1energy
            bne   nothit2

death2      sei
            jmp   gameover1

nothit2     rts



;-----------------------------------------------------------------------
