C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       UCASE
C
C ABSTRACT:     This converts a string of characters to all uppercase
C
C ENVIRONMENT:  Used to convert all short codes to upper case
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  17-APR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: UCASE v1.2'
        SUBROUTINE UCASE( string, errorc )
C
        CHARACTER*(*)   string
        INTEGER         i, j, errorc
        EXTERNAL        KLC_NORMAL
C
        errorc = %LOC(KLC_NORMAL)
        DO i=1,LEN(string)
                IF ((ICHAR(string(i:i)).GT.ICHAR('`')).AND.
        1               (ICHAR(string(i:i)).LT.ICHAR('{')))
        1               string(i:i)=CHAR(ICHAR(string(i:i))-32)
                IF (ICHAR(string(i:i)).LT.32) string(i:i)=' '
        END DO
        RETURN
	END
