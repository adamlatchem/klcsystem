C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       INIT
C
C ABSTRACT:     This routine sets up various internal variables and gets a
C               pasteboard id for SMG$. Also creates a screen to draw on.
C               Sets up a virtual keyboard link. It returns the path of
C               all the KLC system data by translating logical name KLC$PATH
C
C ENVIRONMENT:  SMG$
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  17-MAR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: INIT v1.2'
        SUBROUTINE INIT( pasteboard, display, keyboard, errorc ) 
        IMPLICIT INTEGER (A-Z)
C
        INTEGER*4       pasteboard, display, keyboard, errorc
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
        s = SMG$CREATE_PASTEBOARD ( pasteboard )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$CREATE_VIRTUAL_DISPLAY( 24, 132, display )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PASTE_VIRTUAL_DISPLAY( display, pasteboard, 1, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$CREATE_VIRTUAL_KEYBOARD( keyboard )
        IF (.NOT.s) CALL ERROR( s )
        RETURN
	END
