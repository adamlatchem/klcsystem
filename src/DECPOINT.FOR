C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       DECPOINT
C
C ABSTRACT:     This routine makes sure that the price and profit fields have
C               a decimal point in so that errors do not occur when the
C               CALCULATE routine is called by CENQUIRE.
C
C ENVIRONMENT:  The routine needs a string holding the field and the length of
C               the field.
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  29-MAR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: DECPOINT v1.2'
        SUBROUTINE DECPOINT( string, length, errorc )
C
        CHARACTER*(*)   string
        INTEGER*4       length, errorc, flag
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
        flag = 0
        DO i=1,length
                IF ( (string(i:i).EQ.'.').AND.(flag.EQ.1) )
        1               string(i:i)=' '
                IF (string(i:i).EQ.'.') flag=1
        END DO
        IF (INDEX(string(1:length),'.').EQ.0)
        1       string(INDEX(string,' '):INDEX(string,' ')+2)='.00'
	END
