C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       VALIDATE
C
C ABSTRACT:     This routine takes an input string and depending on
C               the validation type will validate the string.
C               Any invalid characters will be removed
C               Current validation types are
C                       1  - Any printable character
C                       2  - Digits only
C                       3  - Digits and decimal point
C                       4  - letters and digits and space
C                       5  - the letter S or digits
C
C ENVIRONMENT:  KLC system used by CENQUIRE etc...
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  5-APR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: VALIDATE v1.2'
        SUBROUTINE VALIDATE( string, type, errorc )
C
        CHARACTER*(*) string, valid(5)*120
        INTEGER*4       type, errorc
C
        DATA valid(1)/ ' !"#$%& ()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOP
        1QRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~' /
        DATA valid(2)/ '0123456789' /
        DATA valid(3)/ '0123456789.' /
        DATA valid(4)/ ' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghij
        1klmnopqrstuvwxyz' /
        DATA valid(5)/ 'Ss0123456789' /
        EXTERNAL        KLC_NORMAL, KLC_VCISTR
C+
C       The type argument indexes which string is used to validate the given 
C       text.
C-
        errorc = %LOC(KLC_NORMAL)
        DO i=1,LEN(string)
10              k = 0
                DO j=1,LEN(valid(type))
                        IF (string(i:i).EQ.valid(type)(j:j)) k=1
                END DO
                IF (k.EQ.0) THEN
                        string(i:LEN(string))=string(i+1:len(string))//' '
                        PRINT *,CHAR(7)
                        errorc = %LOC(KLC_VCISTR)
                        GOTO 10
                END IF
        END DO
        RETURN
	END
