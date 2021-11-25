            ;===============================================================


sfx_smash   lda   #$88
            sta   $d416
            lda   #$88
            sta   $d415

            lda   #$a0             ;freq 1 fine
            sta   $d40e
            lda   #$a0             ;freq 1 coarse
            sta   $d40f
            ;-------------------------
            lda   #$1f             ;AD
            sta   $d413
            lda   #$fa             ;SR
            sta   $d414
            ;-------------------------
            lda   #%10000001       ;Voice #1 control register. Bits:
                                   ;Bit #0: 0 = Voice off, Release cycle
                                   ;  --->  1 = Voice on, Attack-Decay-Sustain cycle.
                                   ;Bit #1: 1 = Synchronization enabled.
                                   ;Bit #2: 1 = Ring modulation enabled.
                                   ;Bit #3: 1 = Disable voice, reset noise generator.

                                   ;Bit #4: 1 = Triangle waveform enabled. ($10)
                                   ;Bit #5: 1 = Saw waveform enabled.      ($20)
                                   ;Bit #6: 1 = Rectangle waveform enabled.($40) --> requres pulswidth
            sta   $d412            ;Bit #7: 1 = Noise enabled.             ($80)
            ;-------------------------

            lda   #$80             ;Bit #0: 0 = Voice off, Release cycle
            sta   $d412
 
smashexit   rts

            ;======================================================================