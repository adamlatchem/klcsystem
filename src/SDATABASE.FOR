C+
C
C VERSION:      1.2-001
C
C FACILITY:     KLC
C
C MODULE:       SDATABASE
C
C ABSTRACT:     This performs all the database functions
C
C ENVIRONMENT:  Requires a function code 1=add to database
C                                        2=edit database entry
C                                        3=Find a database entry
C
C               dbfile should be set to CUSTOMER or SUPPLIER.  This
C               varaible is used to determine the prompts and file name
C               requires a pasteboard id and keyboard id
C               It is an indexed file with a primary key that corresponds to
C               the short cut name of the supplier/customer. The short code 
C               can be up to 7 characters in length and must be unique
C               The format of each record is as follows:
C
C                       byte            Desciption
C
C                       1-7             Short code of supplier(primary key)
C                       8-47            company name
C                       48-107          Contacts name( 2 entries )
C                       108-127         Fax number
C                       128-147         Telephone number
C                       148-387         Postal address
C                       388-417         Title of contact 1
C                       418-447         Title of contact 2
C               The file is opened on logical unit 1
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  20-MAR-1993
C
C MODIFIED BY:
C
C       Below is the format of 1 record in the supplier file
C-
CDEC$   IDENT 'KLC SYSTEM: SDATABASE v1.2'
        SUBROUTINE SDATABASE( function, keyboard, paste, dbfile, errorc )
        IMPLICIT INTEGER (s-z)
C
1       FORMAT( A7, A40, A30, A30, A20, A20, A240, A30, A30 )
        INTEGER*4       function, localdisplay, keyboard, d1, a, status, line
        INTEGER*4       errorc
        CHARACTER*8     dbfile
        CHARACTER*240   string, short*7, company*40, contact1*30, fax*20
        CHARACTER*20    tel, address*240, contact2*30, title1*30, title2*30
        EXTERNAL        KLC_NORMAL
        erroc = %LOC(KLC_NORMAL)
C       Set all fields to spaces
        DO i=1,30
                contact1(i:i)=' '
                title1(i:i)=' '
                title2(i:i)=' '
                contact2(i:i)=' '
        END DO
        DO i=1, 20
                fax(i:i)=' '
                tel(i:i)=' ' 
                company(i*2-1:i*2)='  '
                address(i*12-11:i*12)='            '
        END DO
        short='       '
C+
C       Open the database file
C-
        OPEN( UNIT=1, FILE='KLC$PATH:'//dbfile(1:3)//'DATA.KLC', 
        1       STATUS='UNKNOWN', 
        1       ACCESS='KEYED', DISPOSE='KEEP', FORM='FORMATTED',
        1       KEY=(1:7:CHARACTER:ASCENDING), ORGANIZATION='INDEXED',
        1       RECL=447, RECORDTYPE='FIXED', SHARED )
C+
C       If function = 1 then we need to add data to the file
C-
        IF (function.EQ.1) THEN
                CALL INITSCREEN( localdisplay,
        1               'KLC - Add '//dbfile//' Information', paste, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                s = SMG$PUT_CHARS( localdisplay,
        1       'Please enter data as requested or type QUIT to abort'
        1       , 3, 1 )
                IF (.NOT.s) CALL ERROR( s )
2       CALL GDBASE ( localdisplay, short, company, contact1, contact2,
        1               fax, tel, address, title1, title2, keyboard, paste, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        IF (short(1:4).EQ.'QUIT') GOTO 10
C-
C       Everything okay so write out the new record
C       string(8:8) lets the error handler know where the error came from
C+
                string(8:8)='0'
                WRITE (UNIT=1, FMT=1, ERR=11 ) short, company, contact1,
        1                contact2, fax, tel, address, title1, title2
               
        ELSE
C       If function = 3 then we are searching for a person in the database
                IF (function.EQ.3) THEN
                        CALL INITSCREEN( localdisplay,
        1                       'KLC - Find '//dbfile//' Information',
        2                       paste, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay,
        1               'Please enter data as requested or type QUIT to abort'
        1               , 3, 1 )
                        IF (.NOT.s) CALL ERROR( s )
3                       s = SMG$PUT_CHARS( localdisplay, '   Enter short code:'
        1               , 5, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay, '( * To list All.)'
        1               , 5, 29 )
                        IF (.NOT.s) CALL ERROR( s )
                        DO i=1, 7
                                string (i:i) = ' '
                        END DO
4                       CALL RSTRING( localdisplay, string, 80, 21, 5, 7, 
        1                       keyboard, paste, status, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        IF (status.NE.13) GOTO 4
                        CALL UCASE( string(1:7), s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        IF (string(1:4).EQ.'QUIT') GOTO 10
C+
C       The value in string is used by the error routine to work out
C       where the error came from
C-
                        string(8:8)='0'
C       If we entered a * then we do a different read in a loop
                        IF (string(1:1).EQ.'*') THEN
                                short='       '
                                s = SMG$ERASE_LINE( localdisplay, 5, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$ERASE_LINE( localdisplay, 6, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$PUT_CHARS( localdisplay, 
        1       'Short Code  Company' , 6, 1 )
                                IF (.NOT.s) CALL ERROR( s )
5                               line = 0
                                s = SMG$ERASE_DISPLAY( localdisplay,8,1,19,80)
                                IF (.NOT.s) CALL ERROR( s )
                                DO WHILE(line.LT.10)
                                READ( UNIT=1, FMT=1, KEYNXTNE=short, ERR=6 )
        1                       short, company, contact1, contact2, fax, tel,
        1                       address, title1, title2
                                UNLOCK( UNIT=1 )
                                line = line + 1
                                s = SMG$PUT_CHARS( localdisplay, short//
        1       '     '//company, 7+line, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                END DO
                                s = SMG$PUT_CHARS( localdisplay,
        1       'Press "Q" to quit, or any key to view more records.' , 20, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$SET_PHYSICAL_CURSOR( paste, 21, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$READ_KEYSTROKE( keyboard, a )
                                IF (.NOT.s) CALL ERROR( s )
                                IF ((a.EQ.81).OR.(a.EQ.113)) GOTO 10
                                GOTO 5
6                               s = SMG$PUT_CHARS( localdisplay,
        1       'Press any key to continue ...                      ' , 20, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$SET_PHYSICAL_CURSOR( paste, 21, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$READ_KEYSTROKE( keyboard, a )
                                IF (.NOT.s) CALL ERROR( s )
                        ELSE
C       Read the record (If not found an error will be caused i.e. GOTO 11 )
                        READ( UNIT=1, FMT=1, KEY=string(1:7), ERR=11 )
        1               short, company, contact1, contact2, fax, tel, address,
        1               title1, title2
                        UNLOCK( UNIT=1 )
C       The next line calls the routine to display found data
                        CALL DATADISPLAY( localdisplay, 7, short, contact1, 
        1               contact2, company, address, fax, tel, title1,
        1               title2, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay,
        1               'Press any key to continue ...', 21, 26 )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$READ_KEYSTROKE( keyboard, s )
                        IF (.NOT.s) CALL ERROR( s )
                        END IF
           ELSE
C       We get here if user wants to edit an entry in the database
                IF (function.EQ.2) THEN
                        CALL INITSCREEN( localdisplay,
        1                       'KLC - Edit '//dbfile//' Information',
        1                       paste, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay,
        1               'Please enter data as requested or type QUIT to abort'
        1               , 3, 1 )
                        IF (.NOT.s) CALL ERROR( s )
7                       s = SMG$PUT_CHARS( localdisplay, dbfile//' short code:'
        1               , 5, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        DO i=1, 7
                                string (i:i) = ' '
                        END DO
8                       CALL RSTRING( localdisplay, string, 80, 21, 5, 7, 
        1                       keyboard, paste, status, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        IF (status.NE.13) GOTO 8
                        CALL UCASE( string(1:7), s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        IF (string(1:4).EQ.'QUIT') GOTO 10
C+
C       The value in string is used by the error routine to work out
C       where the error came from
C-
                        string(8:8)='1'
C       Read the record (If not found an error will be caused i.e. GOTO 11 )
                        READ (UNIT=1, FMT=1, KEY=string(1:7), ERR=11 )
        1               short, company, contact1, contact2, fax, tel, address,
        1               title1, title2 
                        s = SMG$ERASE_LINE ( localdisplay, 4, 1 )
                        IF (.NOT.s) CALL ERROR( s )
C       If found then display data
                        CALL DATADISPLAY( localdisplay, 7, short, contact1, 
        1               contact2, company, address, fax, tel, title1,
        1               title2, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay,
        1               'Press "E" For Edit, "D" For Delete Or "Q" To Exit :'
        1               , 5, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$READ_KEYSTROKE( keyboard, a )
                        IF (.NOT.s) CALL ERROR( s )
                        IF ((a.EQ.81).OR.(A.EQ.113)) GOTO 10
                        IF ((a.EQ.68).OR.(a.EQ.100)) THEN
                                s = SMG$PUT_CHARS( localdisplay,
        1       'Are you sure you want to delete this record (Y/N): ', 5, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$READ_KEYSTROKE( keyboard, a )
                                IF (.NOT.s) CALL ERROR( s )
                                IF ((a.EQ.89).OR.(a.EQ.121)) THEN
                                        DELETE( UNIT=1 )
                                        s = SMG$PUT_CHARS( localdisplay,
        1       'Record has been deleted.                           ', 5, 1 )
                                        IF (.NOT.s) CALL ERROR( s )
                                        s = SMG$PUT_CHARS( localdisplay,
        1       'Press any key to continue ...', 21, 1 )
                                        IF (.NOT.s) CALL ERROR( s )
                                        s = SMG$READ_KEYSTROKE( keyboard, a )
                                        IF (.NOT.s) CALL ERROR( s )
                                END IF
                                GOTO 10
                        END IF
                        s = SMG$ERASE_LINE( localdisplay, 5, 1 )
                        IF (.NOT.s) CALL ERROR( s )
C       Now edit the record
C       Get all the information for 1 record
9                       CALL GDBASE( localdisplay, short, company, contact1,
        1               contact2, fax, tel, address, title1, title2,
        1               keyboard, paste, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        IF (short(1:4).EQ.'QUIT') GOTO 10 
C-
C       Everything okay so write out the new record
C       string(8:8) lets the error handler know where the error came from
C+
                        string(8:8)='1'
C       First delete the old version
                        DELETE ( UNIT=1 )
                        WRITE (UNIT=1, FMT=1, ERR=11 ) short, company, 
        1                       contact1, contact2, fax, tel, address, 
        1                       title1, title2
                        ENDIF
                ENDIF
        ENDIF
10      s = SMG$DELETE_VIRTUAL_DISPLAY( localdisplay )
        IF (.NOT.s) CALL ERROR( s )
        CLOSE (UNIT=1)
        RETURN
C+
C       if there is an error writing the record we come here
C       we get the error number and if it is 50 then we tried to duplicate a key
C       This routine rings the bell to bring the users attention
C-
11      s = SMG$RING_BELL( localdisplay )
        IF (.NOT.s) CALL ERROR( s )
        CALL ERRSNS( s, d1, d1, d1, d1 )
        IF (s.EQ.50) THEN
                s = SMG$PUT_CHARS( localdisplay,
        1       'The Short Code Must Be Unique - Please Re-enter', 4, 1
        1       ,,SMG$M_REVERSE )
                IF (.NOT.s) CALL ERROR( s )
                IF (string(8:8).EQ.'0') GOTO 2
                GOTO 9
        END IF
        IF (s.EQ.36) THEN
                s = SMG$PUT_CHARS( localdisplay,
        1       'The Short Code Was Not Found', 4, 1,, SMG$M_REVERSE )
                IF (.NOT.s) CALL ERROR( s )
                IF (string(8:8).EQ.'0') GOTO 3
                GOTO 7
        END IF
	END
