C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       CREATE_FAX
C
C ABSTRACT:     This module allows the user to produce a stand alone fax and
C               edit it using VAXTPU.  The user must specify a short code so
C               that the system can create the fax initially.
C
C ENVIRONMENT:  Not many parameters just the pasteboard.
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  23-APR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: CREATE_FAX v1.2'
        SUBROUTINE CREATE_FAX( keyboard, paste, enquiry, errorc )
        IMPLICIT INTEGER (a-z)
        INCLUDE '($SMGDEF)'
C
1       FORMAT( A7, A40, A30, A30, A20, A20, A240, A30, A30 )
        INTEGER*4       keyboard, paste, localdisplay, errorc, status
        CHARACTER*240   string, enquiry*10, file*20
        EXTERNAL        KLC_NORMAL
C       We need a display to write to
        CALL INITSCREEN( localdisplay, 'KLC - Create A Fax', paste, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
C       Find out if we need a customer or a supplier
        s = SMG$PUT_CHARS( localdisplay, 'Press "C", "S" or "Q" to Quit.',
        1       3, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay,
        1       'Send a fax to a Customer or a Supplier:', 4, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF ( .NOT.s ) CALL ERROR( s )
        IF ( ( a.NE.SMG$K_TRM_UPPERCASE_Q ).AND.
        1       ( a.NE.SMG$K_TRM_LOWERCASE_Q ) ) THEN
C       Open the correct database on unit 2.
                IF ( ( a.EQ.SMG$K_TRM_UPPERCASE_C ).OR.
        1       ( a.EQ.SMG$K_TRM_LOWERCASE_C ) ) THEN
                        OPEN( UNIT=2, FILE='KLC$PATH:CUSDATA.KLC', 
        1               STATUS='UNKNOWN', ERR=4,
        1               ACCESS='KEYED', DISPOSE='KEEP', FORM='FORMATTED',
        1               KEY=(1:7:CHARACTER:ASCENDING), ORGANIZATION='INDEXED',
        1               RECL=447, RECORDTYPE='FIXED', SHARED )
                ELSE
                        OPEN( UNIT=2, FILE='KLC$PATH:SUPDATA.KLC', 
        1               STATUS='UNKNOWN', ERR=4,
        1               ACCESS='KEYED', DISPOSE='KEEP', FORM='FORMATTED',
        1               KEY=(1:7:CHARACTER:ASCENDING), ORGANIZATION='INDEXED',
        1               RECL=447, RECORDTYPE='FIXED', SHARED )
                END IF
C       Open the fax template on unit 1
                OPEN( UNIT=1, STATUS='UNKNOWN', CARRIAGECONTROL='LIST',
        1       DISPOSE='KEEP', FILE='KLC$PATH:FAX1.LIS',
        1       ERR=2 )
2               s = SMG$PUT_CHARS( localdisplay, 'Enter name for FAX:', 5, 1 )
                IF ( .NOT.s ) CALL ERROR( s )
                string = '                                            '
                CALL RSTRING( localdisplay, string, 80, 20, 5, 20, keyboard,
        1       paste, status, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                CALL UCASE( string(1:20), s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                file = string(1:20)
C       Now read the short code
                s = SMG$PUT_CHARS( localdisplay, 
        1       'Please enter the short code of the company you require:'
        1       , 6, 1 )
                IF ( .NOT.s ) CALL ERROR( s )
                string = '        '
                CALL RSTRING( localdisplay, string, 80, 56, 6, 7, keyboard,
        1       paste, status, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
C       Create the file
                CALL UCASE( string, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                READ( UNIT=2, KEY=string(1:7), ERR=6, FMT=1 )
C       Open the fax file on unit 3
                OPEN( UNIT=3, STATUS='NEW', CARRIAGECONTROL='LIST', ERR=5,
        1       DISPOSE='KEEP', FILE='KLC$PATH:'//string(1:20),
        1       DEFAULTFILE = ' ' )
                CALL PARSE( 1, 3, 2, string(1:7), enquiry, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                CLOSE( UNIT=1 )
                CLOSE( UNIT=2 )
                CLOSE( UNIT=3 )
C       And edit it.
                s = TPU$EDIT( '/SECTION=KLC$PATH:KLC_EDIT '//
        1       '/INIT=KLC$PATH:EDITINIT KLC$PATH:'//file )
                s = SMG$REPAINT_SCREEN( paste )
                IF ( .NOT.s ) CALL ERROR( s )
                s = SMG$PUT_CHARS( localdisplay,
        1       'Do wish to print this fax now [Y/N]:'
        1       , 7, 1 )
                IF ( .NOT.s ) CALL ERROR( s )
                s = SMG$READ_KEYSTROKE( keyboard, a )
                IF ( .NOT.s ) CALL ERROR( s )
                IF ( ( a.EQ.SMG$K_TRM_UPPERCASE_Y ).OR.
        1       (a.EQ.SMG$K_TRM_LOWERCASE_Y ) ) THEN
                        OPEN( UNIT=1, FILE='KLC$PATH:'//file,
        1               DISPOSE='PRINT', STATUS='OLD', CARRIAGECONTROL='LIST',
        1                 DEFAULTFILE = ' ' )
                        CLOSE( UNIT=1 )
                        s = SMG$PUT_CHARS( localdisplay,
        1               'The FAX is being printed', 8, 1 )
                        IF ( .NOT.s ) CALL ERROR( s )
                END IF
                s = SMG$PUT_CHARS( localdisplay,
        1       'Press any key to continue ...'
        1       , 11, 1 )
                IF ( .NOT.s ) CALL ERROR( s )
                s = SMG$READ_KEYSTROKE( keyboard, a )
                IF ( .NOT.s ) CALL ERROR( s )
        END IF
3       s = SMG$DELETE_VIRTUAL_DISPLAY( localdisplay )
        IF ( .NOT.s ) CALL ERROR( s )
        RETURN   
C       This handles errors caused when opening the data base files
4       s = SMG$PUT_CHARS( localdisplay,
        1       'There was an error opening the database file.', 10, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'Press any key to continue ...'
        1       , 11, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF ( .NOT.s ) CALL ERROR( s )
        GOTO 3
5       s = SMG$PUT_CHARS( localdisplay,
        1       'There was an error opening the fax file '//string(1:20)
        1       , 10, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'Press any key to continue ...'
        1       , 11, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF ( .NOT.s ) CALL ERROR( s )
        GOTO 3
6       s = SMG$PUT_CHARS( localdisplay,
        1       'The short code '//string(1:7)//' does not exist.'
        1       , 10, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'Press any key to continue ...'
        1       , 11, 1 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF ( .NOT.s ) CALL ERROR( s )
        GOTO 3
	END
