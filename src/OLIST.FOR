C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       OLIST
C
C ABSTRACT:     This routine lists all the enquiries on the system, the customer
C               asociated with each enquiry and the status of each enquiry
C
C ENVIRONMENT:  It requires a keyboard and pasteboard id.  The enquiry file
C               is used by this routine
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  17-APR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: OLIST v1.2'
        SUBROUTINE OLIST( keyboard, paste, errorc )
        IMPLICIT INTEGER (a-z)
C
        STRUCTURE /line/ 
                UNION
                        MAP
                                CHARACTER*10    enqnum
                                CHARACTER*5     item
                                CHARACTER*5     qty
                                CHARACTER*18    partnum
                                CHARACTER*30    descr
                                CHARACTER*7     potsupp(5)
                                CHARACTER*7     suppused
                                CHARACTER*18    revpartnum
                                CHARACTER*5     discount
                                CHARACTER*5     cdiscount
                                CHARACTER*10    unitprice
                                CHARACTER*5     profit
                                CHARACTER*5     numshipped
                        END MAP
                        MAP
                                CHARACTER*158   all
                        END MAP
                END UNION
        END STRUCTURE

        RECORD /line/ eline

1       FORMAT( A10, A5, A5, A18, A30, 5A7, A7, A18, A5, A5, A10, A5, A5 )
2       FORMAT( A7, A40, A30, A30, A20, A20, A240, A30, A30 )

        INTEGER*4       keyboard, paste, localdisplay, errorc
        CHARACTER*10    enquiry
        EXTERNAL        KLC_NORMAL
        enquiry='          '
        errorc = %LOC(KLC_NORMAL)
C       okay lets get our selves a new display id ...
        CALL INITSCREEN( localdisplay,
        1       'KLC - Enquiry Information', paste, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
C       Lets open the file YEAH !!
        OPEN( UNIT=1, FILE='KLC$PATH:ENQUIRY.KLC', STATUS='UNKNOWN', 
        1       ACCESS='KEYED', DISPOSE='KEEP', FORM='FORMATTED',
        1       KEY=(1:15:CHARACTER:ASCENDING),
        1       ORGANIZATION='INDEXED', RECL=158, RECORDTYPE='FIXED', SHARED )
C       Put info lines
        s = SMG$PUT_CHARS( localdisplay, 
        1               'Status codes : S-supplier order made', 3 , 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'Q-Quote sent', 4, 16 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'I-Invoice sent', 5, 16 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'O-Supplier order produced', 6, 16 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'P-Packing list produced', 7, 16 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'ENQUIRY    CUSTOMER STATUS', 9, 1 )
        IF (.NOT.s) CALL ERROR( s )
        i = 10
C       Try to get the next enquiry
3       READ ( UNIT=1, KEYNXTNE=enquiry//'zzzzz', ERR=5, FMT=1 )
        1       eline.enqnum, eline.item, eline.qty, eline.partnum,
        1       eline.descr, eline.potsupp(1), eline.potsupp(2),
        1       eline.potsupp(3), eline.potsupp(4), eline.potsupp(5),
        1       eline.suppused, eline.revpartnum, eline.discount,
        1       eline.cdiscount, eline.unitprice, eline.profit,
        1       eline.numshipped
        UNLOCK( UNIT=1 )
        enquiry = eline.enqnum
        s = SMG$PUT_CHARS( localdisplay, eline.enqnum//' '//
        1       eline.partnum(1:7)//'  '//eline.potsupp(1)(1:5), i, 1 )
        IF (.NOT.s) CALL ERROR( s )
C       Try to read a line of the enquiry
        eline.enqnum='NOT EQUAL '
        READ ( UNIT=1, KEYNXTNE=enquiry//'     ', ERR=4, FMT=1 )
        1       eline.enqnum, eline.item, eline.qty, eline.partnum,
        1       eline.descr, eline.potsupp(1), eline.potsupp(2),
        1       eline.potsupp(3), eline.potsupp(4), eline.potsupp(5),
        1       eline.suppused, eline.revpartnum, eline.discount,
        1       eline.cdiscount, eline.unitprice, eline.profit,
        1       eline.numshipped
4       IF (eline.enqnum.NE.enquiry) THEN
                s = SMG$PUT_CHARS( localdisplay, 'Empty', i, 40 )
                IF (.NOT.s) CALL ERROR( s )
        ENDIF
        IF (i.GT.21) THEN
                s = SMG$PUT_CHARS( localdisplay,
        1               'Press any key to view next screen.', 23, 1 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$READ_KEYSTROKE( keyboard, a )
                IF (.NOT.s) CALL ERROR( s )
                DO i=10,21
                        s = SMG$ERASE_LINE( localdisplay, i, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                END DO
                i = 10
        ENDIF
        i = i + 1
        GOTO 3
C       This is where we go when we have run out of enquiries
5       s = SMG$PUT_CHARS( localdisplay, 
        1       'Press any key to return to the menu ...', 24, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$DELETE_VIRTUAL_DISPLAY( localdisplay )
        IF (.NOT.s) CALL ERROR( s )
        RETURN
	END
