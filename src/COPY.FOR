C+
C
C VERSION:      1.2-002
C
C FACILITY:     KLC
C
C MODULE:       COPY
C
C ABSTRACT:     This routine allows you to copy data between the customer and
C               supplier databases
C
C ENVIRONMENT:  This routine makes SMG$ calls and requires access to both the
C               customer and suipplier database files.  It also uses the
C               RSTRING routine.
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  23-APR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: COPY v1.2'
        SUBROUTINE COPY( keyboard, paste, errorc )
        IMPLICIT INTEGER(a-z)
        INCLUDE '($SMGDEF)'
C
1       FORMAT          ( A7, A40, A30, A30, A20, A20, A240, A30, A30 )
        CHARACTER*240   address, string, short*7, company*40, contact1*30
        CHARACTER*20    fax, tel, file*6, contact2*30, title1*30, title2*30
        INTEGER*4       localdisplay, keybpoard, paste, keyp, errorc, status
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
C       Open a new display and put a title in to it.
        CALL INITSCREEN( localdisplay, 'KLC - Copy Between Databases',
        1       paste, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 
        1       'Enter "S" for supplier, "C" for customer or "Q" to quit.',
        1        3, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'Copy from :', 5, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, keyp )
        IF ( .NOT.s ) CALL ERROR( s )
        IF ((keyp.NE.SMG$K_TRM_UPPERCASE_Q).AND.
        1       (keyp.NE.SMG$K_TRM_LOWERCASE_Q))THEN
                IF ((keyp.EQ.SMG$K_TRM_UPPERCASE_S).OR.
        1       (keyp.EQ.SMG$K_TRM_LOWERCASE_S)) THEN
                        file = 'SUPCUS'
                        s = SMG$PUT_CHARS( localdisplay, 'Supplier database',
        1               5, 12 )
                        IF ( .NOT.s ) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay, 
        1               'To :Customer database', 6, 8) 
                        IF ( .NOT.s ) CALL ERROR( s )
                ELSE                
                        file = 'CUSSUP'
                        s = SMG$PUT_CHARS( localdisplay, 'Customer database',
        1               5, 12 )
                        IF ( .NOT.s ) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay,
        1               'To :Supplier database', 6, 8)
                        IF ( .NOT.s ) CALL ERROR( s )
                END IF
C       Now open input on unit 1 and output on unit 2
                OPEN( UNIT=1, FILE='KLC$PATH:'//file(1:3)//'DATA.KLC', 
        1       STATUS='UNKNOWN', ACCESS='KEYED', DISPOSE='KEEP',
        1       FORM='FORMATTED', KEY=(1:7:CHARACTER:ASCENDING),
        1       ORGANIZATION='INDEXED', RECL=447, RECORDTYPE='FIXED', SHARED )
                OPEN( UNIT=2, FILE='KLC$PATH:'//file(4:6)//'DATA.KLC', 
        1       STATUS='UNKNOWN', ACCESS='KEYED', DISPOSE='KEEP',
        1       FORM='FORMATTED', KEY=(1:7:CHARACTER:ASCENDING),
        1       ORGANIZATION='INDEXED', RECL=447, RECORDTYPE='FIXED', SHARED )
C       Now get the source short code and destination short
                s = SMG$PUT_CHARS( localdisplay, 
        1       'Enter the short code to be coppied, "QUIT" to finish.   ',
        1       3, 1 )
                IF ( .NOT.s ) CALL ERROR( s )
                s = SMG$PUT_CHARS( localdisplay, 'Source short code :', 8, 1 )
                IF ( .NOT.s ) CALL ERROR( s )
                string(1:7) = '       '
2               CALL RSTRING( localdisplay, string, 80, 20, 8, 7, keyboard,
        1       paste, status, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                CALL UCASE( string(1:7), s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                IF (string(1:4).NE.'QUIT') THEN
                        READ( UNIT=1, FMT=1, KEYEQ=string(1:7), ERR=5 )
        1               short, company, contact1, contact2, fax, tel, address,
        1               title1, title2
                        UNLOCK( UNIT=1 )
                        s = SMG$PUT_CHARS( localdisplay, company, 8, 29 )
                        IF ( .NOT.s ) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay, 
        1       'Enter the destination short code or "QUIT" to finish. ',
        1               3, 1 )
                        IF ( .NOT.s ) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay, 'Desti. short code :',
        1               9, 1 )
                        IF ( .NOT.s ) CALL ERROR( s )
                        string(1:7) = '       '
3                       CALL RSTRING( localdisplay, string, 80, 20, 9, 7,
        1               keyboard, paste, status, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        CALL UCASE( string(1:7), s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        IF (string(1:4).NE.'QUIT') THEN
                                WRITE( UNIT=2, FMT=1, ERR=6 ) string(1:7),
        1                       company, contact1, contact2, fax, tel,
        1                       address, title1, title2
                                UNLOCK( UNIT=1 )
                                s = SMG$PUT_CHARS( localdisplay,
        1                       'Operation completed.', 10, 1 )
                                IF ( .NOT.s ) CALL ERROR ( s )
                                s = SMG$PUT_CHARS( localdisplay,
        1                       'Press any key to continue ...',11,1)
                                IF ( .NOT.s ) CALL ERROR( s )
                                s = SMG$READ_KEYSTROKE( keyboard, a )
                                IF ( .NOT.s ) CALL ERROR( s )
                        END IF
                END IF
        END IF
4       s = SMG$DELETE_VIRTUAL_DISPLAY( localdisplay )
        IF ( .NOT.s ) CALL ERROR( s )
        RETURN
C       If the source short code does not exist then tell the user.
5       s = SMG$RING_BELL( localdisplay )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 
        1       'This short code does not exist.', 8, 29 )
        IF ( .NOT.s ) CALL ERROR( s )  
        GOTO 2
C       If the destination already exists tell the user
6       s = SMG$RING_BELL( localdisplay )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay,
        1       'This short code already exists, enter a new one.', 9, 29 )
        IF ( .NOT.s ) CALL ERROR( s )
        GOTO 3
	END
