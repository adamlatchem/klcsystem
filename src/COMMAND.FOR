C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       COMMAND
C
C ABSTRACT:     This allows a DCL command to be executed in a sub-process.
C               The user must type the lo (Logoff) command to return to the
C               application.
C
C ENVIRONMENT:  Called by KLC module.  Creates a user sub-process
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  23-MAR-1993      
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: COMMAND v1.2'
        SUBROUTINE COMMAND( keyboard, paste, errorc )
        IMPLICIT INTEGER(a-z)
C
        INTEGER*4       keyboard, paste, errorc
        EXTERNAL        KLC_NORMAL
C
        CALL INITSCREEN( localdisplay, '- DCL Commands -', paste, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        s = LIB$SPAWN( )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$DELETE_VIRTUAL_DISPLAY( localdisplay )
        IF ( .NOT.s ) CALL ERROR( s )
        RETURN
        END
