C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       PARSE
C
C ABSTRACT:     This routine will read all the lines from inunit and put them
C               in outunit, but the lines are parsed so that dates and 
C               information can be inserted.  This information is obtained from
C               the dbaseunit file which must be the supplier/customer database.
C
C ENVIRONMENT:  Requires 3 FORTRAN unit numbers that have open files on them.
C               Also requires the short code of the supplier/customer you want
C               info on.  The enquiry number is also used
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  8-APR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: PARSE v1.2'
        SUBROUTINE PARSE( inunit, outunit, dbaseunit, short, enquiry, errorc )
        IMPLICIT INTEGER (a-z)
        INCLUDE '($JPIDEF)'
C
1       FORMAT( A7, A40, A30, A30, A20, A20, A240, A30, A30 )
        INTEGER*4       dbaseunit, inunit, outunit, uic, errorc, length*2
        CHARACTER*180   string, company*40, fax*20, tel*20, address*240
        CHARACTER*30    contact1, title1, contact2, title2, short*7, dat*9
        CHARACTER*10    enquiry, identifier*50, command*1
        EXTERNAL        KLC_PNSUCSC, KLC_PUSDID, KLC_NORMAL
C       Get the UIC of this user and find their identifier
        s = LIB$GETJPI ( JPI$_UIC, , , uic )
        IF (.NOT.s) CALL ERROR( s )
        s = SYS$IDTOASC ( %VAL(uic), length, identifier, , , )
        errorc = %LOC(KLC_NORMAL)
C       If we can't Get a UIC identifier then use a default value
        IF (.NOT.s) THEN
                identifier = 'KLC INTERNATIONAL'
                length = 17
                errorc = %LOC(KLC_PUSDID)
        END IF
C       Get the date string
        CALL DATE(dat)
C       Now get customer/supplier infomration
        READ( UNIT=dbaseunit, KEY=short, ERR=4, FMT=1 ) short, company, 
        1       contact1, contact2, fax, tel, address, title1, title2
        UNLOCK( UNIT=dbaseunit )
C+
C       The following commands are available
C               \D - Date
C               \R - Enquiry reference number
C               \F - Fax number                             \
C               \T - telephone number                       |
C               \An- Address line n                         | of the supplier
C               \1s- Contact 1 \                            |
C               \2s- Contact 2 / s=(T)itle or (N)ame        |
C               \N - Users UIC identifier                   |
C               \C - company name                           /
C-
2       READ( UNIT=inunit, END=3, FMT='(A80)' )string
C       Scan string for emedded commands and replace strings
        IF (string(1:5).EQ.'[END]') GOTO 3
        DO i=1,80
C       If we have found a \ then there must be a command so put data in string
                IF (string(i:i).EQ.'\') THEN
                        command = string(i+1:i+1)
                        IF (command.EQ.'A') THEN
                                b = ICHAR(string(i+2:i+2)) - 49
                                string(i:i+39) = address((b*40)+1:(b+1)*40)
                        END IF
                        IF (command.EQ.'C') string(i:i+39)=company
                        IF (command.EQ.'D') string(i:i+8)=dat
                        IF (command.EQ.'F') string(i:i+19)=fax
                        IF (command.EQ.'N')
        1                       string(i:i+length)=identifier(1:length)
                        IF (command.EQ.'R') string(i:i+9)=enquiry
                        IF (command.EQ.'T') string(i:i+19)=tel
                        IF (command.EQ.'1') THEN
                                IF (string(i+2:i+2).EQ.'T') THEN
                                        string(i:i+29)=title1
                                ELSE
                                        string(i:i+29)=contact1
                                END IF
                        END IF
                        IF (command.EQ.'2') THEN
                                IF (string(i+2:i+2).EQ.'T') THEN
                                        string(i:i+29)=title2
                                ELSE
                                        string(i:i+29)=contact2
                                END IF
                        END IF
                END IF
        END DO
        WRITE( UNIT=outunit, FMT='(A80)' ) string(1:80)
        GOTO 2
3       RETURN
C       If we can't find the short code then flag an error and return
4       errorc = %LOC(KLC_PNSUCSC)
        RETURN
	END
