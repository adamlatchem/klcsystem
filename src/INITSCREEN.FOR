C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       INITSCREEN
C
C ABSTRACT:     This routine allows us to get a new display id.  It also
C               displays a title in double height at the top of the screen
C               The display id is returned by this module.
C
C ENVIRONMENT:  SMG$ is used quelle surprise
C               The pasteboard parameter is required by this routine (paste)
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  13-OCT-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: INITSCREEN v1.2'
        SUBROUTINE INITSCREEN( display_id, title, paste, errorc )
        IMPLICIT INTEGER (s)
C
        INTEGER*4       display_id, paste, errorc
        CHARACTER*(*)   title
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
C       Create a new display and paste it on the screen
        s = SMG$CREATE_VIRTUAL_DISPLAY( 24, 80, display_id )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PASTE_VIRTUAL_DISPLAY( display_id, paste, 1, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
C       Greetings ...
        S = SMG$PUT_CHARS_WIDE( display_id, title, 1, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        RETURN
	END
