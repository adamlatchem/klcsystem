C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       EXITIT
C
C ABSTRACT:     This closes up all the open files and links with SMG$.
C
C ENVIRONMENT:  SMG$
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  17-MAR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: EXITIT v1.2'
        SUBROUTINE EXITIT( paste, display, keyboard, errorc )
        IMPLICIT INTEGER (a-z)
C
        INTEGER*4       paste, display, keybaord, errorc
        EXTERNAL        KLC_NORMAL
C
        errorc = %LOC(KLC_NORMAL)
        s = SMG$UNPASTE_VIRTUAL_DISPLAY( display, paste )
        IF( .NOT.s ) CALL ERROR( s )
        s = SMG$DELETE_VIRTUAL_DISPLAY( display )
        IF( .NOT.s ) CALL ERROR( s )
        s = SMG$DELETE_VIRTUAL_KEYBOARD( keyboard )
        IF( .NOT.s ) CALL ERROR( s )
        s = SMG$DELETE_PASTEBOARD( paste )
        IF( .NOT.s ) CALL ERROR( s )
        RETURN
	END
