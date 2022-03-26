.import L9500_install_wedge

.segment "INIT"
.segment "ONCE"
.segment "STARTUP"

; Install SAM wedge and return to BASIC
_main:
    jmp L9500_install_wedge     ; tail call
