C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       CRENEWNUM
C
C ABSTRACT:     This routine will produce a new enquiry number based on the
C               date and the last enquiry created in the enquiry database
C
C ENVIRONMENT:  Called by the main loop when the create new enquiry option
C               is selected.  It requires the enquiry number string to be
C               passed to it.
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  29-MAR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: CRENEWNUM v1.2'
        SUBROUTINE CRENEWNUM( enquiry, errorc )
        IMPLICIT INTEGER ( s-z )
C
1       FORMAT( A158 )
        CHARACTER*10    enquiry, date*12, dummyrec*136
        INTEGER*4       localdisplay, errorc
        EXTERNAL        KLC_NORMAL, KLC_CNNUMAVL
        errorc = %LOC(KLC_NORMAL)
C       First get todays date.  We must also convert it to integers
        CALL IDATE( t, u, v )
        ENCODE( 12, '(3I4)', date ) v, t, u
        enquiry(1:2)=date(3:4)
        enquiry(3:4)=date(7:8)
        enquiry(5:6)=date(11:12)
        DO j=1,6
                IF (enquiry(j:j).EQ.' ') enquiry(j:j)='0'
        END DO
C       Open the enquiry file so we can get the latest enquiry number
        OPEN( UNIT=5, FILE='KLC$PATH:ENQUIRY.KLC', STATUS='UNKNOWN', 
        1       ACCESS='KEYED', DISPOSE='KEEP', FORM='FORMATTED',
        1       KEY=(1:15:CHARACTER:ASCENDING), ORGANIZATION='INDEXED',
        1       RECL=158, RECORDTYPE='FIXED', SHARED )
        DO i=1,9999
                ENCODE( 4,'(I4)', enquiry(7:10) )i
C       Replace ' 's with '0's in enquiry number
                DO j=7,10
                        IF (enquiry(j:j).EQ.' ') enquiry(j:j)='0'
                END DO
                READ( UNIT=5, KEY=enquiry//'     ', ERR=2, FMT=1 )
                UNLOCK( UNIT=5 )
        END DO
        errorc = %LOC(KLC_CNNUMAVL)
C+
C       Come here when the specified key is not found i.e. we have a new key
C       Close the file and exit from this routine
C-
2       CLOSE( UNIT=5 )     
        RETURN
	END
