+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       ERROR
C
C ABSTRACT:     This routine handles errors returned by system calls
C
C ENVIRONMENT:  It takes a paramter (long integer) which is a VMS message code.
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  18-MAR-1993
C
C MODIFIED BY:
C
C       If error is a dataoverubn then who actually cares? (ignore it)
C       Ignore EOF errors
C-
CDEC$   IDENT 'KLC SYSTEM: ERROR v1.2'
        SUBROUTINE ERROR ( enum )
        IMPLICIT INTEGER (e)
        INCLUDE '($SSDEF)'
        INCLUDE '($SMGMSG)'
C
        IF (((enum.EQ.SS$_DATAOVERUN).OR.(enum.EQ.SMG$_EOF)).OR.
        1       (enum.EQ.SS$_BADESCAPE)) THEN
        ELSE
                CALL LIB$SIGNAL(%VAL(enum))
        ENDIF
        RETURN
	END
