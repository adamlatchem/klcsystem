C+
C
C VERSION:      1.2-005
C
C FACILITY:     KLC
C
C MODULE:       REPORT
C
C ABSTRACT:     This is the last module I wrote.  It is entitled REPORT and
C               it produces a documents.  Well it needs the enquiry number
C               to decide which data to use and it uses the enquiry file etc.
C               option is used to decide what is produced.
C                       option = 1  Supplier enquiry
C                       option = 2  Quote
C                       option = 3  Supplier order
C                       option = 4  Invoice
C                       option = 5  Packing list
C
C ENVIRONMENT:  Called by KLC with the enquiry number, keyboard id and a
C               pasteboard id
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  8-APR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: REPORT v1.2'
        SUBROUTINE REPORT( enquiry, keyboard, paste, option, errorc )
        IMPLICIT INTEGER( a-z )
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
C
        RECORD /line/ eline, keline
C
1       FORMAT( A10, A5, A5, A18, A30, 5A7, A7, A18, A5, A5, A10, A5, A5 )
2       FORMAT( A7, A40, A30, A30, A20, A20, A240, A30, A30 )
100     FORMAT( BNF10.2 )
C
        CHARACTER*10    enquiry, suplist(5005)*7, short*7
        CHARACTER*30    title1, title2, contact1, contact2, dat*9, formats*80
        CHARACTER*240   address, company*40, tel*20, fax*20, msg(5)*16
        CHARACTER*10    file(5)*8, string*240, price*10, tot*10
        CHARACTER*80    flags*5 /'SQOIP'/, cmd*1, SPACE*80
        INTEGER*4       paste, keyboard, localdisplay, a, firstused
        INTEGER*4       qty, option, size(5), lastused, flag
        REAL            uprice, total(5005), discount, profit, temp
        DATA    file(1)/ 'SUPORDER' /, size(1)/8/, msg(1)/'Supplier Enquiry'/
        DATA    file(2)/ 'QUOTE' /   , size(2)/5/, msg(2)/'Customer Quote'/
        DATA    file(3)/ 'ORDER' /   , size(3)/5/, msg(3)/'Supplier Order'/
        DATA    file(4)/ 'INVOICE' / , size(4)/7/, msg(4)/'Customer Invoice'/
        DATA    file(5)/ 'PACKING' / , size(5)/7/, msg(5)/'Packing List'/
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
        DO i=1,80
                SPACE(i:i)=' '
        ENDDO
        temp = 0.0
C
        DO i=1,240
                string(i:i)=' '
        END DO         
C       Create a local screen for use by this routine
        CALL INITSCREEN( localdisplay,
        1       'KLC - Produce '//msg(option), paste, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'Current Enquiry Number :'//enquiry,
        1               3, 1 )
        IF (.NOT.s) CALL ERROR( s )
C       Ask if they really want to continue or not
        s = SMG$PUT_CHARS( localdisplay,
        1       'Do You Want To Produce The '//msg(option)//
        1       ' For This Enquiry [Y/N]:', 4, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF ((a.NE.ICHAR('Y')).AND.(a.NE.ICHAR('y'))) GOTO 8
C       Open the enquiry file and the customer/supplier database file
        s = SMG$PUT_CHARS( localdisplay, 'Opening Enquiry File ...', 20, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
        IF (.NOT.s) CALL ERROR( s )
        OPEN( UNIT=1, FILE='KLC$PATH:ENQUIRY.KLC', STATUS='UNKNOWN', 
        1       ACCESS='KEYED', DISPOSE='KEEP', FORM='FORMATTED',
        1       KEY=(1:15:CHARACTER:ASCENDING),
        1       ORGANIZATION='INDEXED', RECL=158, RECORDTYPE='FIXED', SHARED )
        IF ((option.EQ.1).OR.(option.EQ.3)) THEN
                s = SMG$PUT_CHARS( localdisplay, 'Opening Supplier File ...'
        1               , 20, 1 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
                IF (.NOT.s) CALL ERROR( s )
                OPEN( UNIT=2, FILE='KLC$PATH:SUPDATA.KLC', 
        1       STATUS='UNKNOWN', 
        1       ACCESS='KEYED', DISPOSE='KEEP', FORM='FORMATTED',
        1       KEY=(1:7:CHARACTER:ASCENDING), ORGANIZATION='INDEXED',
        1       RECL=447, RECORDTYPE='FIXED', SHARED )
        ENDIF
C+
C       We have to be able to keep track of the different suppliers.
C       This is done by opening a new file each time a new supplier name is
C       encountered in the enquiry file.  These files are created by using the
C       curretn enquiry number followed by the short code of the supplier as
C       both these entities are unique. lastunit is lastunit used for a file
C-
        firstused = 4
        lastused = 3
C       Find first enquiry line, if it does not exist then tell the user.       
        READ( UNIT=1, KEY=enquiry//'     ', ERR=12, FMT=1 )
        1       keline.enqnum, keline.item, keline.qty, keline.partnum,
        1       keline.descr, keline.potsupp(1), keline.potsupp(2),
        1       keline.potsupp(3), keline.potsupp(4), keline.potsupp(5),
        1       keline.suppused, keline.revpartnum, keline.discount,
        1       keline.cdiscount, keline.unitprice, keline.profit,
        1       keline.numshipped
C       Tell user we are starting
        s = SMG$PUT_CHARS( localdisplay, 'Starting '//msg(option)//
        1       ' Production.', 20, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 
        1       'For Customer '//keline.partnum(1:7), 20, 1 )
        IF (.NOT.s) CALL ERROR( s )
        short = keline.partnum(1:7)
        s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
        IF (.NOT.s) CALL ERROR( s )
C+
C       The quote, invoice and packing list are all produced in the same way
C       We therefore have their code here
C       Put the header tag file first
C-
        IF ((option.NE.1).AND.(option.NE.3)) THEN
                suplist(1) = keline.partnum(1:7)
                s = SMG$PUT_CHARS( localdisplay, 'Opening Customer file ...'
        1               , 20, 1 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
                IF (.NOT.s) CALL ERROR( s )
C       Open customer database file.
                OPEN( UNIT=3, FILE='KLC$PATH:CUSDATA.KLC',
        1       STATUS='UNKNOWN',
        1       ACCESS='KEYED', DISPOSE='KEEP', FORM='FORMATTED',
        1       KEY=(1:7:CHARACTER:ASCENDING), ORGANIZATION='INDEXED',
        1       RECL=447, RECORDTYPE='FIXED', SHARED )
                s = SMG$PUT_CHARS( localdisplay, 'Opening '//msg(option)//
        1       ' File.', 20, 1 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
                IF (.NOT.s) CALL ERROR( s )
C       Create output document file.
                OPEN( UNIT=5, STATUS='UNKNOWN', CARRIAGECONTROL='LIST',
        1       DISPOSE='KEEP', FILE='KLC$PATH:'//file(option)(1:size(option))//
        1       '1.LIS', ERR=3 )
                OPEN( UNIT=4, STATUS='NEW', CARRIAGECONTROL='LIST', ERR=13,
        1       DISPOSE='KEEP', FILE='KLC$PATH:'//enquiry//file(option)
        1       (1:size(option))//'.LIS' )
                s = SMG$PUT_CHARS( localdisplay, 'Writing Head Tag File.'
        1       , 20, 1 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
                IF (.NOT.s) CALL ERROR( s )
                CALL PARSE( 5, 4, 3, short, enquiry, s )
                IF (s.NE.%LOC(KLC_NORMAL)) GOTO 10
C       Get field format information.
                CALL GETFORMAT( 5, formats, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                CLOSE( UNIT= 5)
                total(1) = 0.0
                lastused = 4
        ENDIF
C       Now loop around getting all the enquiry lines
3       s = SMG$PUT_CHARS( localdisplay, 'Processing Enquiry Lines ...', 20
        1       ,1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
        IF (.NOT.s) CALL ERROR( s )
        eline.item='     '
        flag = 0
        DO WHILE(flag.EQ.0)
C       Read next enquiry line from enquiry file.
                READ( UNIT=1, KEYNXTNE=enquiry//eline.item, FMT=1, ERR=5 )
        1       eline.enqnum, eline.item, eline.qty, eline.partnum,
        1       eline.descr, eline.potsupp(1), eline.potsupp(2),
        1       eline.potsupp(3), eline.potsupp(4), eline.potsupp(5),
        1       eline.suppused, eline.revpartnum, eline.discount,
        1       eline.cdiscount, eline.unitprice, eline.profit,
        1       eline.numshipped
C       If we have reached the end of the enquiry then exit.
                IF (enquiry.NE.eline.enqnum) THEN
                        flag = 1
                        GOTO 4
                ENDIF
C       If creating a customer document the processing is simple.
                IF ((option.NE.1).AND.(option.NE.3)) THEN
                        CALL WRITELINE( eline, formats, lastused, temp, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                        total(1) = total(1) + temp
                ELSE
C       Check for supplier names for this enquiry line
                        IF (option.EQ.1) THEN
                                j = 0
                                DO i=1, 5
                                        IF( eline.potsupp(i).EQ.'       ')
        1                                       j = j + 1
                                END DO
                                IF (j.EQ.5) THEN
                                        s = SMG$PUT_CHARS( localdisplay,
        1                               'No Supplier(s) Specified for item '//
        1                               eline.item//'.  Please Edit Enquiry.',
        1                               20, 1 )
                                        IF (.NOT.s) CALL ERROR( s )
                                        s = SMG$SCROLL_DISPLAY_AREA( 
        1                               localdisplay, 10, 1, 11,,, 1 )
                                        IF (.NOT.s) CALL ERROR( s )
                                        CONTINUE
                                ENDIF
                        ELSE
                                IF (eline.suppused.EQ.'       ') THEN
                                        s = SMG$PUT_CHARS( localdisplay,
        1                       'No Supplier Specified for item '//eline.item//
        1                       '.  Please Edit Enquiry.', 20, 1 )
                                        IF (.NOT.s) CALL ERROR( s )
                                        s = SMG$SCROLL_DISPLAY_AREA( 
        1                               localdisplay, 10, 1, 11,,, 1 )
                                        IF (.NOT.s) CALL ERROR( s )
                                        CONTINUE
                                ENDIF
                        ENDIF
C+
C       Now check each potential supplier against the list we have
C       The list is stored in suplist and each time a new supplier
C       is encountered the short code is added to the list and a new file is
C       opened with a new unit number.
C       If we find a supplier then send the information to their file else
C       open a new supplier file.
C-
                        IF (option.EQ.3) THEN
                                n = 1
                                eline.potsupp(1) = eline.suppused
                        ELSE
                                n = 5
                        END IF      
                        DO i=1,n
                                IF (eline.potsupp(i).EQ.'       ') GOTO 4
C       Check the specified supplier exists on the database
                                l = 0
                                DO j=1,1+(lastused-firstused)
                                        IF (eline.potsupp(i).EQ.suplist(j)) l=j
                                END DO
C       If l=1 then we need to create a new supplier file.
                                IF (l.EQ.0) THEN
                                        READ( UNIT=2, KEY=eline.potsupp(i), 
        1                               ERR=14, FMT=2 ) short, company, 
        1                               contact1, contact2, fax, tel, address,
        1                               title1, title2
                                        UNLOCK( UNIT=2 )
                                        lastused = lastused + 1
                                        suplist(lastused-firstused+1)=
        1                                       eline.potsupp(i)
                                        supunit = lastused
                                        s = SMG$PUT_CHARS( localdisplay,
        1               'Creating File For Supplier '//eline.potsupp(i)
        1               //' - '//company, 20, 1 )
                                        IF (.NOT.s) CALL ERROR( s )
                                        s = SMG$SCROLL_DISPLAY_AREA( 
        1                               localdisplay, 10, 1, 11,,, 1 )
                                        IF (.NOT.s) CALL ERROR( s )
C+
C       To create the new file we copy the template file called 
C       KLC$PATH:SUPORDER1.LIS to the new file name we want.  Note that there
C       are a number of cmds that may appear in the file as given in PARSE
C-
                                        OPEN( UNIT=supunit, STATUS='NEW',
        1                       CARRIAGECONTROL='LIST', DISPOSE='KEEP',
        1                       FILE='KLC$PATH:'//enquiry//file(option)(1:3)//
        1                       eline.potsupp(i)(1:len(eline.potsupp(1)))
        1                       //'.LIS' )
                                        OPEN( UNIT=supunit+1, STATUS='UNKNOWN'
        1                       ,CARRIAGECONTROL='LIST', DISPOSE='KEEP',
        1                       FILE='KLC$PATH:'//file(option)(1:size(option))
        1                       //'1.LIS' )
                                        CALL PARSE( supunit+1, supunit, 2, 
        1                               eline.potsupp(i), enquiry, s )
                                        IF (s.NE.%LOC(KLC_NORMAL)) GOTO 14
                                        CALL GETFORMAT( supunit+1, formats, s )
                                        IF (s.NE.%LOC(KLC_NORMAL)) 
        1                               CALL ERROR( s )
                                        CLOSE( UNIT=supunit+1 )
                                        total(supunit-3) = 0.0
                                ELSE
C+
C       If we find the supplier has already got an open file then get unit
C       the number for that supplier.
C-
                                        supunit = l+3
                                END IF
C       Now write out the information required
                                CALL WRITELINE( eline, formats, supunit, temp
        1                       ,s )
                                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                                total(supunit-3) = total(supunit-3) + temp
4                       END DO
                ENDIF
        END DO
C+
C       If a T appears in the format string we want a total.
C       The T specifioer is followed by a number to say which field the total
C       should line up with.  We also prompt the user to edit and print
C-
5       DO i=lastused, firstused, -1
                b = INDEX(formats,'T')
                IF (b.NE.0) THEN
                        s = SMG$PUT_CHARS( localdisplay, 'Writing Total ...',
        1               20, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11
        1               ,,, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        ENCODE(10, 100, tot) total(i-firstused+1)
C       Write total out to file
                        WRITE( UNIT=i, FMT='(A)' ) ' '
                        WRITE( UNIT=i, FMT='(A)' ) SPACE(1:b-1)//
        1                       'TOTAL EX. WORKS : '//tot
                ENDIF
C       Put out the end tag file
                s = SMG$PUT_CHARS( localdisplay, 'Writing Tail Tag File ...'
        1               , 20, 1 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
                IF (.NOT.s) CALL ERROR( s )
                OPEN( UNIT=i+1, STATUS='UNKNOWN', CARRIAGECONTROL='LIST',
        1       DISPOSE='KEEP', FILE='KLC$PATH:'//file(option)(1:size(option))
        1       //'2.LIS' )
                IF ((option.NE.1).AND.(option.NE.3)) THEN
                        CALL PARSE( i+1, i, 3, suplist(i-3), enquiry, s )
                ELSE
                        CALL PARSE( i+1, i, 2, suplist(i-3), enquiry, s )
                ENDIF
                IF (s.NE.%LOC(KLC_NORMAL)) GOTO 10
                CLOSE( UNIT=i+1 )
                CLOSE( UNIT=i )
                IF ((option.NE.1).AND.(option.NE.3)) THEN
                        string = 'KLC$PATH:'//enquiry//file(option)
        1                       (1:size(option))//'.LIS'
                ELSE
                        string = 'KLC$PATH:'//enquiry//file(option)
        1                       (1:3)//suplist(i-3)
        1                       (1:INDEX(suplist(i-3),' ')-1)//'.LIS'
                ENDIF
                s = SMG$PUT_CHARS( localdisplay,
        1       'Do You Wish To Edit The Document For '//suplist(i-3)
        1       //' [Y/N]:', 21, 1 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$READ_KEYSTROKE( keyboard, a )
                IF (.NOT.s) CALL ERROR( s )
                IF ((a.EQ.121).OR.(a.EQ.89)) THEN
                        s = SMG$ERASE_LINE( localdisplay, 21, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay,
        1       'Please Wait ...', 21, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$ERASE_LINE( localdisplay, 22, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        q = INDEX(string,'.')+3
                        s = TPU$EDIT( string(1:q)//
        1       ' /SECTION=KLC$PATH:KLC_EDIT/INIT=KLC$PATH:EDITINIT' )
                        s = SMG$REPAINT_SCREEN( paste )
                END IF
                s = SMG$PUT_CHARS( localdisplay, 
        1               'Do You Wish To Print Document For '
        1               //suplist(i-3)//' [Y/N]:     ', 21, 1 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$READ_KEYSTROKE( keyboard, a )
                IF (.NOT.s) CALL ERROR( s )
                IF ((a.EQ.89).OR.(a.EQ.121)) THEN
                        OPEN( UNIT=i, STATUS='UNKNOWN', CARRIAGECONTROL='LIST',
        1               DISPOSE='PRINT', FILE=string )
                        CLOSE( UNIT=i )
                ENDIF
                suplist(i-1)='       '
        ENDDO
C       Flag which operation has been performed
6       READ( UNIT=1, FMT=1, KEY=enquiry//'     ' )
        keline.potsupp(1)(option:option) = flags(option:option)
        REWRITE( UNIT=1, FMT=1 )
        1       keline.enqnum, keline.item, keline.qty, keline.partnum,
        1       keline.descr, keline.potsupp(1), keline.potsupp(2),
        1       keline.potsupp(3), keline.potsupp(4), keline.potsupp(5),
        1       keline.suppused, keline.revpartnum, keline.discount,
        1       keline.cdiscount, keline.unitprice, keline.profit,
        1       keline.numshipped
        CLOSE( UNIT=1 )
7       s = SMG$ERASE_LINE( localdisplay, 23, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'Press any key to continue ...'
        1       , 23, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF (.NOT.s) CALL ERROR( s )
8       s = SMG$DELETE_VIRTUAL_DISPLAY( localdisplay )
        IF (.NOT.s) CALL ERROR( s )
        RETURN
C+
C       ERROR ROUTINES
C
C       This next bit deals with arithmetic overflows
C-
9       s = SMG$PUT_CHARS( localdisplay,
        1       'A value for item '//eline.item//' is too large.'
        1       , 21, 1 )
        keline.potsupp(i)(2:2)=' '
        GOTO 5
C       This is the routine called if the customer short code is not found
10      s = SMG$PUT_CHARS( localdisplay, 
        1       'Customer Not Found In Database - Use Add Customer Option'
        2       , 20, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
        IF (.NOT.s) CALL ERROR( s )
11      s = SMG$PUT_CHARS( localdisplay, 'Press Any Key To Continue ...'
        1       , 23, 1)
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF (.NOT.s) CALL ERROR( s )
        GOTO 8
C       Come here if inital invoice/quote/packing list file can't be opened
12      s = SMG$PUT_CHARS( localdisplay,
        1       'The Enquiry Selected Does Not Exist', 20, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$RING_BELL( localisplay )
        IF (.NOT.s) CALL ERROR( s )
        GOTO 11
13      s = SMG$PUT_CHARS( localdisplay, 'There Was An Error Opening '
        1       //msg(option)//'File.', 20, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$SCROLL_DISPLAY_AREA( localdisplay, 10, 1, 11,,, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$RING_BELL( localisplay )
        IF (.NOT.s) CALL ERROR( s )
        GOTO 11
C       This is where we get to if the supplier cannot be found on the database
14      s = SMG$PUT_CHARS( localdisplay,
        1       'The Supplier Short Code "'//eline.potsupp(i)//
        1       '" Was Not Found In The Database.', 24, 1 )
        IF (.NOT.s) CALL ERROR( s )
        GOTO 7
	END
