C+
C
C VERSION:      1.2-004
C
C FACILITY:     KLC
C
C MODULE:       CENQUIRE
C
C ABSTRACT:     This is the customer enquiry input screen
C               The format of each enquiry line is as follows:
C               Each record has two keys - the enquiry number
C                                                  the item number
C               So each record key is made from enquiry//item
C               The rest of the record is as follows
C                       10      Enquiry number
C                       5       Item number
C                       5       Quantity of item
C                       18      Part number
C                       30      Description
C                       35      Potential suppliers
C                       7       Supplier used(used later on)
C                       18      Revised part number
C                       5       Discount to KLC from supplier (%age)
C                       5       Discount KLC gives to the customer (%age)
C                       10      Unit price
C                       5       Profit margin (%age)
C                       5       Number of item shippped(used later)
C               Record length = 158
C               There is a special record defined by the enquiry number and a
C               string "     " which will hold data on the customer etc as
C               detailed below.
C                       1:10       Enquiry number
C                       11:15      "     "
C                       parntnum   Customer shortcode
C                       potsupp(1) status codes S-Supplier enquiry done
C                                               Q-Quote produced
C                                               I-Invoice produced
C                                               P-Packing list produced
C
C ENVIRONMENT:  This needs a valid enquiry number in enquiry - note enquiry is
C               a string that contains a number in character form
C               This makes use of the CALULATE Module to work out the 
C               profit and cost to customer
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  28-MAR-1993
C
C MODIFIED BY:
C
C       The next bit merrly sets up a record structure wow!
C-
CDEC$   IDENT 'KLC SYSTEM: CENQUIRE v1.2'
        SUBROUTINE CENQUIRE( enquiry, keyboard, paste, errorc )
        IMPLICIT INTEGER ( a-z )
        INCLUDE '($SMGDEF)'
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
        RECORD /line/ eline(1001)
C
100     FORMAT( A10, A5, A5, A18, A30, 5A7, A7, A18, A5, A5, A10, A5, A5 )
200     FORMAT( A7, A40, A30, A30, A20, A20, A240, A30, A30 )
C
        CHARACTER*10    enquiry, short*7, string*240, date*12, title1*30
        CHARACTER*30    title2, contact1, contact2, address*240, company*40
        CHARACTER*240   datastring, odstring, temp, temp3, tel*20, fax*20
        CHARACTER*240   fieldheader / 'ITEM  QTY   PART NO.           
        1DESCRIPTION                    UNIT PRICE SHIPPED DISCOUNT% CUST 
        1DISCOUNT% REV. PART NO.      PROFIT% POTENTIAL SUPPLIERS
        1                     SUPPLIER PRICE TO CUSTOMER PROFIT MADE   
        1                         '/
        REAL            costklc( 1000 ), costcust( 1000 ), temp1, temp2, total
        REAL            profit, tempprofit
        INTEGER*4       keyboard, paste, localdisplay, a, datascreen, dataview
        INTEGER*4       numused, fieldxpos( 18 ), fieldsize( 18 ), x, y, fnum
        INTEGER*4       vx, vy, endstring, fieldval( 18 ), status, p
        INTEGER*4       fieldend( 18 ), pf3, pf3y, usedflag, errorc
C
        DATA            (fieldxpos( i ), i=1,18) / 1, 7, 13, 32, 63, 74, 82,
        1               92, 107, 126, 134, 142, 150, 158, 166, 174, 183, 201 /
        DATA            (fieldsize( i ), i=1,18) / 5, 5, 18, 30, 10, 5, 5, 5,
        1               18, 5, 7, 7, 7, 7, 7, 7, 10, 10 /
        DATA            (fieldval( i ), i=1,18) / 4, 2, 1, 1, 3, 5, 3, 3, 1, 3,
        1               1, 1, 1, 1, 1, 1, 2, 2 /
        DATA            (fieldend( i ), i=1,18) / 6, 12, 31, 62, 73, 59, 87, 97
        1               ,125, 131, 141, 149, 157, 165, 173, 181, 193, 211 /
C
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
C       Clear all the totals incase any are left over from the last call.
        DO i=1,1000
                costklc( i )=0.0
                costcust( i )=0.0
        END DO
C       Set the variable string to all spaces so we get a solid field for input
        DO i=1,240
                string( i:i )=' '
        END DO
C       Get a new screen
        CALL INITSCREEN( localdisplay, 'KLC - Enquiry Screen', paste, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
C       Lets open the file YEAH !!
        OPEN( UNIT=1, FILE='KLC$PATH:ENQUIRY.KLC', STATUS='UNKNOWN', 
        1       ACCESS='KEYED', DISPOSE='KEEP', FORM='FORMATTED',
        1       KEY=( 1:15:CHARACTER:ASCENDING ),
        1       ORGANIZATION='INDEXED', RECL=158, RECORDTYPE='FIXED', SHARED )
C       Try to get the info field - if not found then create it
        READ ( UNIT=1, KEY=enquiry//'     ', ERR=16, FMT=100 )
        1       eline(1001).enqnum, eline(1001).item, eline(1001).qty,
        1       eline(1001).partnum, eline(1001).descr, eline(1001).potsupp(1),
        1       eline(1001).potsupp(2), eline(1001).potsupp(3),
        1       eline(1001).potsupp(4), eline(1001).potsupp(5),
        1       eline(1001).suppused, eline(1001).revpartnum, 
        1       eline(1001).discount, eline(1001).cdiscount,
        1       eline(1001).unitprice, eline(1001).profit,
        1       eline(1001).numshipped
        UNLOCK( UNIT=1 )
C       having read the info record we want to get the customer address
1       OPEN( UNIT=2, FILE='KLC$PATH:CUSDATA.KLC', 
        1       STATUS='UNKNOWN', 
        1       ACCESS='KEYED', DISPOSE='KEEP', FORM='FORMATTED',
        1       KEY=(1:7:CHARACTER:ASCENDING), ORGANIZATION='INDEXED',
        1       RECL=447, RECORDTYPE='FIXED', SHARED )
        READ (UNIT=2, KEY=eline(1001).partnum(1:7), ERR=20, FMT=200 )
        1       short, company, contact1, contact2, fax, tel, address,
        1       title1, title2
        UNLOCK( UNIT=2 )
C       Clear display and set up for editing
        s = SMG$ERASE_DISPLAY( localdisplay )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS_WIDE( localdisplay, 'KLC Enquiry : '//enquiry, 1, 1 )
        IF (.NOT.s) CALL ERROR( s )
C       Display customer information
        s = SMG$PUT_CHARS( localdisplay,    'Customer : '//company, 3, 3 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, address(1:40), 4 ,14 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, address(41:80), 5, 14 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, address(81:120), 6, 14 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, address(121:160), 7, 14 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, address(161:200), 8, 14 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, address(201:240), 9, 14 )
        IF (.NOT.s) CALL ERROR( s )
C       To show the order we use a virtual display and a view port
        s = SMG$CREATE_VIRTUAL_DISPLAY( 1000, 230, datascreen )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$CREATE_VIEWPORT( datascreen, 1, 1, 10, 80 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, fieldheader(1:80), 10, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$DRAW_LINE( localdisplay, 11, 1, 11, 80 )
        IF(.NOT.s) CALL ERROR( s )
C+
C       Now read in any records that may already be defined for this enquiry
C       and put them in the virtual display
C-
        eline(1).item = '     '
        DO i=1,1000
                READ ( UNIT=1, KEYNXTNE=enquiry//eline(i).item, ERR=2, FMT=100 )
        1       eline(i).enqnum, eline(i).item, eline(i).qty, eline(i).partnum,
        1       eline(i).descr, eline(i).potsupp(1), eline(i).potsupp(2),
        1       eline(i).potsupp(3), eline(i).potsupp(4), eline(i).potsupp(5),
        1       eline(i).suppused, eline(i).revpartnum, eline(i).discount,
        1       eline(i).cdiscount, eline(i).unitprice, eline(i).profit,
        1       eline(i).numshipped
                UNLOCK( UNIT=1 )
                IF (eline(i).enqnum.NE.enquiry) GOTO 2
C       The next line sets the next item so that a search is performed
                IF (i.NE.1000) eline(i+1).item=eline(i).item
                s = SMG$PUT_CHARS( datascreen,  eline(i).item//' '//
        1               eline(i).qty//' '//eline(i).partnum//' '//
        1               eline(i).descr//' '//eline(i).unitprice//' '//
        1               eline(i).numshipped//'   '//eline(i).discount//'     '//
        1               eline(i).cdiscount//'          '//
        1               eline(i).revpartnum//' '//eline(i).profit//'   '//
        1               eline(i).potsupp(1)//' '//eline(i).potsupp(2)//' '//
        1               eline(i).potsupp(3)//' '//eline(i).potsupp(4)//' '//
        1               eline(i).potsupp(5)//' '//eline(i).suppused, i, 1 )
                IF (.NOT.s) CALL ERROR( s )
                CALL CALCULATE( localdisplay, datascreen, i, costklc(i), 
        1               costcust(i), paste, keyboard, s )
        END DO
C       When we get to here i holds the number of entries
2       IF (i.EQ.1000) THEN
                s = SMG$PUT_CHARS( localdisplay, 
        1       'No more items may be added to this enquiry. ', 25, 1)
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$RING_BELL( localdisplay )
                IF (.NOT.s) CALL ERROR( s )
        ENDIF
C       We don't need the customer database anymore so close the file
        CLOSE( UNIT=2 )
        numused = i-1
        IF (numused.GT.0) THEN
                usedflag = 1 
        ELSE 
                numused = 1
                usedflag = 0
        END IF
C       Draw line to show field delimitations
        DO j=2,18
                s = SMG$DRAW_LINE( datascreen, 1, fieldxpos(j)-1, 1000,
        1               fieldxpos(j)-1 )
                IF (.NOT.s) CALL ERROR( s )
        END DO
C       Finally show the virtual display on the screen
        s = SMG$PASTE_VIRTUAL_DISPLAY( datascreen, paste, 12, 1 )
        IF (.NOT.s) CALL ERROR( s )
C       Put up some info for the user
        s = SMG$PUT_CHARS( localdisplay, 
        1       'Use arrow keys to select field then enter data.',
        1       24, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 
        1       'DEL - Delete Line   RETURN - Insert Line   PF1 - Quit',
        1       23, 1 )
        IF (.NOT.s) CALL ERROR( s )
C       init all values for the editing routine. Print totals out.
        CALL TOTAL( costklc, costcust, localdisplay, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        fnum = 1
        x = 1
        vx = 1
        vy = 1
        y = 1
        pf3 = 0
        pf3y = 0
3       s = SMG$ERASE_LINE( localdisplay, 10, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, fieldheader(vx:vx+80), 10, 1 )
        IF (.NOT.s) CALL ERROR( s )
C       Save the new value of vx so we know if it changes in the next loop
4       s = SMG$CHANGE_RENDITION( datascreen, y, fieldxpos(fnum), 1,
        1       fieldsize(fnum), SMG$M_REVERSE )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$CHANGE_VIEWPORT( datascreen, y, vx, 10, 80 )
        IF (.NOT.s) CALL ERROR( s )
C+
C       M   M  AAA  IIIII N   N
C       MM MM A   A   I   NN  N
C       M M M AAAAA   I   N N N
C       M   M A   A   I   N  NN   EDITING   LOOP
C       M   M A   A IIIII N   N
C
C       Variables:
C               a               - Used to hold last keypress
C               vx              -
C               y               - Item line number
C               datascreen      - SMG identifier to data area
C               fnum            - Current fieldnumber
C               numused         - Number of lines entered into the enquiry
C               fieldxpos(n)    - Position of field n along screen
C               fieldsize(n)    - Length of field number n
C               fieldval(n)     - Holds the validation code for field n
C               datastring      - Used to hold entire enquiry line
C               usedflag        - Set to 1 if the current line has data in it
C-
5       s = SMG$SET_CURSOR_ABS( datascreen, y, fieldxpos(fnum) )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF (.NOT.s) CALL ERROR( s )
C       Un-highlight the previous field
        s = SMG$CHANGE_RENDITION( datascreen, y, fieldxpos(fnum), 1,
        1       fieldsize(fnum), 0 )
        IF (.NOT.s) CALL ERROR( s )
C       PF3 allows us to swap between movement and numbers on the keypad
        IF (a.EQ.SMG$K_TRM_PF3) THEN
                IF (pf3.EQ.0) THEN
                        pf3 = 1
                ELSE
                        pf3 = 0
                END IF
        END IF
C-
C       To aid editing we need to be able to move around by a number
C       of lines greater than 1 at a time so use the number keys on the keypad
C       Must press PF3 first
C       If user pressed keypad 1 or 4 then shift the entries by 20
C       If user pressed keypad 2 or 5 then shift by 50
C       If user pressed keypad 3 or 6 then shift by 100
C-
        IF (pf3.EQ.1) THEN
                pf3y = y
                IF (a.EQ.SMG$K_TRM_KP1) y = y + 20
                IF (a.EQ.SMG$K_TRM_KP4) y = y - 20
                IF (a.EQ.SMG$K_TRM_KP2) y = y + 50
                IF (a.EQ.SMG$K_TRM_KP5) y = y - 50
                IF (a.EQ.SMG$K_TRM_KP3) y = y + 100
                IF (a.EQ.SMG$K_TRM_KP6) y = y - 100
                IF (pf3y.NE.y) pf3 = 0
                IF (y.GT.numused) y = numused
        ELSE
                IF (a.EQ.SMG$K_TRM_KP0) a = SMG$K_TRM_ZERO
                IF (a.EQ.SMG$K_TRM_KP1) a = SMG$K_TRM_ONE
                IF (a.EQ.SMG$K_TRM_KP2) a = SMG$K_TRM_TWO
                IF (a.EQ.SMG$K_TRM_KP3) a = SMG$K_TRM_THREE
                IF (a.EQ.SMG$K_TRM_KP4) a = SMG$K_TRM_FOUR
                IF (a.EQ.SMG$K_TRM_KP5) a = SMG$K_TRM_FIVE
                IF (a.EQ.SMG$K_TRM_KP6) a = SMG$K_TRM_SIX
                IF (a.EQ.SMG$K_TRM_KP7) a = SMG$K_TRM_SEVEN
                IF (a.EQ.SMG$K_TRM_KP8) a = SMG$K_TRM_EIGHT
                IF (a.EQ.SMG$K_TRM_KP9) a = SMG$K_TRM_NINE
        END IF
C+
C       we must convert the keypad keys
C-
        IF (a.EQ.SMG$K_TRM_MINUS) a = SMG$K_TRM_DASH
        IF (a.EQ.SMG$K_TRM_PERIOD) a = SMG$K_TRM_DOT
        IF (a.EQ.SMG$K_TRM_COMMA) a = SMG$K_TRM_COMMA_CHAR
C       Check for the various keypresses
6       IF (a.EQ.SMG$K_TRM_RIGHT) fnum = fnum+1
        IF (a.EQ.SMG$K_TRM_LEFT) fnum = fnum-1
        IF (a.EQ.SMG$K_TRM_UP) y = y-1
        IF (a.EQ.SMG$K_TRM_DOWN) y = y+1
        IF (y.GT.numused) y = numused
        IF ((a.EQ.SMG$K_TRM_PF2).OR.(a.EQ.SMG$K_TRM_HELP)) THEN
                CALL HELP( 'Enquiry_Editing', keyboard, paste, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        END IF
C       If user pressed DEL then delete the current line
        IF (a.EQ.SMG$K_TRM_DELETE) THEN
                IF (y.GT.0) THEN
                        s = SMG$DELETE_LINE( datascreen, y, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        DO i=2, 18
                                s = SMG$DRAW_CHAR( datascreen, SMG$M_UP, 1000,
        1                               fieldxpos(i)-1 )
                                IF (.NOT.s) CALL ERROR( s )
                        END DO
                        IF (y.LT.numused) numused = numused-1
                        IF (numused.GT.0) usedflag = 1
                ELSE
                        s = SMG$RING_BELL( datascreen )
                        IF (.NOT.s) CALL ERROR( s )
                ENDIF
        ENDIF
C       If user pressed a key then let them edit the field they have chosen
        IF ((a.GT.31).AND.(a.LT.127)) THEN
                IF (y.EQ.numused) usedflag = 1
                s = SMG$READ_FROM_DISPLAY( datascreen, datastring,, y )
                IF (.NOT.s) CALL ERROR( s )
                odstring = datastring
                datastring(1:fieldsize(fnum))=
        1       datastring(fieldxpos(fnum):fieldxpos(fnum)+fieldsize(fnum))
                datastring(1:1)=CHAR(a)
C       The next bit makes sure we don't put a string in an empty space
                IF (y.GT.numused) THEN
                        numused = numused + 1
                        y = numused
                        usedflag = 1
                ENDIF
C+
C       We must set the end of the string to a Y so RSTRING moves the
C       cursor along so it does not delete the character the user
C       has just typed in.  If we are changing a numeric field clear it first.
C-
                datastring(240:240)='Y'
                IF ((((fnum.EQ.5).OR.(fnum.EQ.7)).OR.
        1       (fnum.EQ.8)).OR.(fnum.EQ.10)) datastring(2:10)='        '
                CALL RSTRING( datascreen, datastring, 200, fieldxpos(fnum),
        1               y, fieldsize(fnum), keyboard, paste, status, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                s = SMG$CHANGE_RENDITION( datascreen, y, fieldxpos(fnum), 1,
        1               fieldsize(fnum), 0 )
                IF (.NOT.s) CALL ERROR( s )
C       If user altering a supplier field make it upper case
                IF (fnum.GE.11) THEN
                        CALL UCASE(datastring(1:7), s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                END IF
C       Now we must validate the string to see if it is okay
                temp = datastring(1:fieldsize(fnum))
                CALL VALIDATE( datastring(1:fieldsize(fnum)),
        1               fieldval(fnum), s )
C+
C       We do not need to check the validate error as it will not be fatal
C       If we changed the price make sure it has a decimal point in it
C-
                IF (fnum.EQ.5) THEN
                        CALL DECPOINT( datastring, 10, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ENDIF
C+
C       If we changed the item field we have to make sure that it has leading
C       zeros so that the items appear in the correct order on the screen.
C-
                IF (fnum.EQ.1) THEN
                        temp3 = datastring(1:5)
                        datastring(1:5)='0000 '
                        p = 1
                        DO i=1,4
                                IF ((temp3(i:i).LT.':').AND.(temp3(i:i).NE.' '))
        1                       p = i
                        END DO
                        IF (p.EQ.5) p=4
                        datastring(5-p:5) = temp3
                        odstring = datastring
                ENDIF
C+
C       if we have changed the discounts or profit, the field must have 
C       a decimal point in it.  The profit must not be above 99.99% otherwise
C       we will get a division by zero error from the calculate routine.
C+
                IF (((fnum.EQ.7).OR.(fnum.EQ.8)).OR.(fnum.EQ.10)) THEN
                        CALL DECPOINT( datastring, 5, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                ENDIF
                IF (fnum.EQ.10) THEN
                        DECODE(5, '(BNF5.2)', datastring(1:fieldsize(fnum))
        1                       , ERR=7 ) tempprofit
7                       IF ( tempprofit.LT.0 ) THEN
                                tempprofit = 0
                        ENDIF
                        IF ( tempprofit.GT.99.99 ) THEN
                                tempprofit = 99.99
                        ENDIF
                        ENCODE(5, '(BNF5.2)', datastring(1:fieldsize(fnum)))
        1                       tempprofit
                ENDIF
                s = SMG$PUT_CHARS( datascreen, 
        1       datastring(1:fieldsize(fnum)), y, fieldxpos(fnum) )
                IF (.NOT.s) CALL ERROR( s )
C+
C       If we are editing the shipped field then check we have not tried to 
C       enter more items shipped than there are in the qty field
C-
                IF (fnum.EQ.6) THEN
                        IF ((INDEX('S', datastring(1:fieldsize(6))).EQ.0).AND.
        1                   (INDEX('s', datastring(1:fieldsize(6))).EQ.0)) THEN
                                DECODE( fieldsize(6), '(I10)', 
        1                       datastring(1:fieldsize(6)), ERR=8 ) b
                                DECODE( fieldsize(2), '(I10)', 
        1                       odstring(fieldxpos(2):fieldxpos(2)+fieldsize(2))
) c
                                IF (b.GT.c) THEN
                                        s = SMG$RING_BELL( datascreen )
                                        IF (.NOT.s) CALL ERROR( s )
                                        s = SMG$PUT_CHARS( localdisplay,
        1               'You are shipping more than the QTY ordered.    ', 
        1                               24, 1 )
                                        IF (.NOT.s) CALL ERROR( s )
                                        status = 0
                                ELSE
                                        s = SMG$PUT_CHARS( localdisplay, 
        1               'Use arrow keys to select field then enter data.',
        1                               24, 1 )
                                        IF (.NOT.s) CALL ERROR( s )
                                ENDIF
                        ENDIF
                ENDIF
C       Finally we update the costing fields if we have just changed them.
8               temp1 = costklc(y)
                temp2 = costcust(y)
                CALL CALCULATE( localdisplay, datascreen, y, costklc(y), 
        1               costcust(y), paste, keyboard, s )
                IF ((temp1.NE.costklc(y)).OR.(temp2.NE.costcust(y))) THEN
                        CALL TOTAL( costklc, costcust, localdisplay, s )
                        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
                END IF
C+
C       The key that caused the exit is in status so do the action it requests
C       If the validation did not change the field then allow movement
C-
                IF (temp.EQ.datastring(1:fieldsize(fnum))) THEN
                        a = status
                        IF (a.EQ.SMG$K_TRM_CTRLM) THEN
                                a=0
                                fnum = fnum + 1
                                IF (fnum.GT.16) fnum=16
                        ENDIF
                ELSE
                        a = 0
                ENDIF
                GOTO 6
        ENDIF
C       If press tab then go to next field
        IF (a.EQ.SMG$K_TRM_HT) fnum = fnum + 1
C+
C       If user pressed keypad 8 then copy current field all the way down the
C       order.  We have to recalculate each line as the field is copied down.
C-
        IF ((a.EQ.SMG$K_TRM_KP8).AND.(pf3.EQ.1)) THEN
                pf3 = 0
                s = SMG$READ_FROM_DISPLAY( datascreen, string,, y )
                IF (.NOT.s) CALL ERROR( s )
                DO t=y+1,numused
                        s = SMG$PUT_CHARS( datascreen, 
        1               string(fieldxpos(fnum):fieldxpos(fnum)+
        1               fieldsize(fnum)), t, fieldxpos(fnum) )
                        IF (.NOT.s) CALL ERROR( s )
                        CALL CALCULATE( localdisplay, datascreen, t,
        1               costklc(t), costcust(t), paste, keyboard, s )
                END DO
                CALL TOTAL( costklc, costcust, localdisplay, s )
                IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        ENDIF
C       If user pressed RETURN then insert a line
        IF (a.EQ.SMG$K_TRM_CTRLM) THEN
                IF (numused.LT.1000) THEN
                        IF (y.GT.numused) y=numused
C       Check to see if there is anything on the last line used
                        IF (usedflag.EQ.1) THEN
                        usedflag = 0
                        s = SMG$INSERT_LINE( datascreen, y+1, ,SMG$M_DOWN )
                        IF (.NOT.s) CALL ERROR( s )
                        DO i=2, 18
                                s = SMG$DRAW_CHAR( datascreen, SMG$M_UP, y+1,
        1                               fieldxpos(i)-1 )
                                IF (.NOT.s) CALL ERROR( s )
                        END DO
                        numused = numused + 1
                        usedflag = 0
                        fnum = 1
                        s = SMG$CHANGE_VIEWPORT( datascreen, y, vx, 10, 80 )
                        IF (.NOT.s) CALL ERROR( s )
                        vy = y
                        y = y + 1
                        END IF
                ELSE
                        s = SMG$RING_BELL( datascreen )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay, 
        1               '1000 lines have been used.', 24, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                END IF
        END IF
C       Check that pointers have valid values
        IF (numused.LT.1) numused=1
        IF (y.LT.1) y=1
        IF (y.GT.1000) y=1000
        IF (fnum.LT.1) fnum=1
        IF (fnum.GT.16) fnum=16
        IF (fieldxpos(fnum)+fieldsize(fnum).GT.vx+70) vx = fieldxpos(fnum)
        IF (fieldxpos(fnum).LT.vx) vx = vx-70
        IF (vx.LT.1) vx=1
        IF (y.LT.vy) vy = y
        IF (y.GT.vy+9) vy=y
        IF (vy.LT.1) vy = 1
        IF (vy.GT.1000) vy = 1000
C       Set the viewport accordingly
        s = SMG$CHANGE_VIEWPORT( datascreen, vy, vx, 10, 80 )
        IF (.NOT.s) CALL ERROR( s )
C       Highlight new field
        s = SMG$CHANGE_RENDITION( datascreen, y, fieldxpos(fnum), 1,
        1       fieldsize(fnum), SMG$M_REVERSE )
        IF (.NOT.s) CALL ERROR( s )
C       Update titles for fields if neccessary
        IF (x.NE.vx) THEN
                s = SMG$ERASE_LINE( localdisplay, 10, 1 )
                IF (.NOT.s) CALL ERROR( s )
                s = SMG$PUT_CHARS( localdisplay, fieldheader(vx:vx+80), 10, 1 )
                IF (.NOT.s) CALL ERROR( s )
        END IF
C       Save the new value of vx so we know if it changes in the next loop
        x = vx
C       If the user did not press PF1 for quit then loop
        IF (a.NE.SMG$K_TRM_PF1) GOTO 5
C+
C       This is the end of the editing loop
C-
        s = SMG$CHANGE_RENDITION( datascreen, y, fieldxpos(fnum), 1,
        1       fieldsize(fnum), 0 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$ERASE_LINE( localdisplay, 23, 1 )
        IF (.NOT.s) CALL ERROR( s )
C       Check the user wants to save the changes
        s = SMG$PUT_CHARS( localdisplay, 
        1 'Press "Y" to save changes, "D" to delete this enquiry, "C" to
        1 continue'
        1       , 23 ,1 )
        IF (.NOT.s) CALL ERROR( s )
9       s = SMG$READ_KEYSTROKE( keyboard, a )
        IF (.NOT.s) CALL ERROR( s )
C       If user pressed Y then save
10      IF ((a.EQ.SMG$K_TRM_UPPERCASE_Y).OR.
        1       (a.EQ.SMG$K_TRM_LOWERCASE_Y)) THEN
C+
C       We have finished editing so write out the new set of records
C       This is done by reading lines from the virtual display
C       first we delete all the old records
C-
        eline(1001).item='     '
        DO i=1,1000
                READ( UNIT=1, KEYNXTNE=enquiry//eline(1001).item, ERR=11, 
        1       FMT=100 ) eline(1001).enqnum, eline(1001).item
                IF(enquiry.NE.eline(1001).enqnum) GOTO 11
                DELETE( UNIT=1 )
        END DO
11      IF (eline(1).item.EQ.'     ') eline(1).item='1    '
        eline(1001).enqnum=enquiry
        IF (numused.NE.0) THEN
        DO i=1,numused
                s = SMG$READ_FROM_DISPLAY( datascreen, datastring, ,i )
                IF (.NOT.s) CALL ERROR( s )
                eline(i).enqnum = enquiry
                eline(i).item = datastring(fieldxpos(1):fieldend(1))
                eline(i).qty = datastring(fieldxpos(2):fieldend(2))
                eline(i).partnum =  datastring(fieldxpos(3):fieldend(3))
                eline(i).descr =  datastring(fieldxpos(4):fieldend(4))
                eline(i).unitprice = datastring(fieldxpos(5):fieldend(5))
                eline(i).numshipped = datastring(fieldxpos(6):fieldend(6))
                eline(i).discount = datastring(fieldxpos(7):fieldend(7))
                eline(i).cdiscount = datastring(fieldxpos(8):fieldend(8))
                eline(i).revpartnum = datastring(fieldxpos(9):fieldend(9))
                eline(i).profit = datastring(fieldxpos(10):fieldend(10))
                eline(i).potsupp(1) = datastring(fieldxpos(11):fieldend(11))
                eline(i).potsupp(2) = datastring(fieldxpos(12):fieldend(12)) 
                eline(i).potsupp(3) = datastring(fieldxpos(13):fieldend(13))
                eline(i).potsupp(4) = datastring(fieldxpos(14):fieldend(14))
                eline(i).potsupp(5) = datastring(fieldxpos(15):fieldend(15))
                eline(i).suppused = datastring(fieldxpos(16):fieldend(16))
C       If the item field is blank then go back to editor
                IF (eline(i).item.EQ.'     ') THEN
                        DO j=15,158
                        IF (eline(i).all(j:j).GT.' ') THEN
                                s = SMG$ERASE_LINE( localdisplay, 23, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$PUT_CHARS( localdisplay,
        1                       'Field must not be blank.',23, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$RING_BELL( datascreen )
                                IF (.NOT.s) CALL ERROR( s )
                                y = i
                                fnum = 1
                                x = 1
                                vx = 1
                                vy = i
                                GOTO 3
                                END IF
                        END DO
C       If we get here then we have a blank record so continue
                        GOTO 12
                END IF
C       Any blank fields should be filled with default values.
                IF (eline(i).qty.EQ.'     ') eline(i).qty='1    '
                IF (eline(i).unitprice.EQ.'          ') eline(i).unitprice=
        1       '0.00'
                IF (eline(i).numshipped.EQ.'     ') eline(i).numshipped='0    '
                IF (eline(i).discount.EQ.'     ') eline(i).discount=' 0.00'
                IF (eline(i).cdiscount.EQ.'     ') eline(i).cdiscount=' 0.00'
                IF (eline(i).profit.EQ.'     ') eline(i).profit=' 0.00'         
                IF (eline(i).revpartnum.EQ.'          ')
        1               eline(i).revpartnum=eline(i).partnum
C+
C       We must have a supplier to use so the first one in the list of
C       potential suppliers is used as a dafault
C-
                DO t=1,5
                IF (eline(i).suppused.EQ.'       ') 
        1               eline(i).suppused=eline(i).potsupp(t)
                END DO
                WRITE ( UNIT=1, FMT=100, ERR=21 )
        1       eline(i).enqnum, eline(i).item, eline(i).qty, eline(i).partnum,
        1       eline(i).descr, eline(i).potsupp(1), eline(i).potsupp(2),
        1       eline(i).potsupp(3), eline(i).potsupp(4), eline(i).potsupp(5),
        1       eline(i).suppused, eline(i).revpartnum, eline(i).discount,
        1       eline(i).cdiscount, eline(i).unitprice, eline(i).profit,
        1       eline(i).numshipped
12      END DO
        END IF
        ELSE
C+
C       If user pressed D then delete
C       Note the key record for each enquiry is never deleted once created
C       This is so that you can never get two enquiry numebrs which are
C       identical
C-
                IF ((a.EQ.SMG$K_TRM_UPPERCASE_D).OR.
        1               (a.EQ.SMG$K_TRM_LOWERCASE_D)) THEN
                        s = SMG$ERASE_LINE( localdisplay, 23, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$PUT_CHARS( localdisplay, 'Are you sure that 
        1you want to delete this enquiry [Y/N]:', 23, 1 )
                        IF (.NOT.s) CALL ERROR( s )
                        s = SMG$READ_KEYSTROKE( keyboard, a )
                        IF (.NOT.s) CALL ERROR( s )
                        IF ((a.EQ.SMG$K_TRM_UPPERCASE_Y).OR.
        1                       (a.EQ.SMG$K_TRM_LOWERCASE_Y)) THEN
C+
C       Only allow deletion if today is a date after the date of the enquiry
C       This way you can never produce a duplicate oreder number
C       So lets get the date ...
C-
                                CALL IDATE( t, u, v )
                                ENCODE( 12, '(3I4)', date ) v, t, u
C       Replace ' 's with '0's in enquiry number
                                DO j=1,12
                                        IF (date(j:j).EQ.' ') date(j:j)='0'
                                END DO
                                IF (((enquiry(1:2).NE.date(3:4)).OR.
        1                       (enquiry(3:4).NE.date(7:8))).OR.
        2                       (enquiry(5:6).NE.date(11:12)))THEN
                                eline(1001).item='     '
                                READ( UNIT=1, KEY=enquiry//eline(1001).item,
        1                       ERR=14, FMT=100 )
                                DELETE( UNIT=1 )
                                s = SMG$ERASE_LINE( localdisplay, 23, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$PUT_CHARS( localdisplay,
        1       'DELETING, Please Wait ....', 23, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                DO i=1,1000
                                        READ( UNIT=1, 
        1                               KEYNXTNE=enquiry//eline(1001).item, 
        1                               ERR=14, FMT='(A158)' ) eline(1001).all
                                        IF (eline(1001).enqnum.EQ.enquiry) THEN
                                                DELETE( UNIT=1 )
                                        ELSE
                                                GOTO 13
                                        ENDIF
                                END DO
C+
C       We have deleteed all the enquiry info from the enquiry file
C       Now we need to delete any .LIS files that have been created for this
C       enquiry.
C-
13                              CALL LIB$SPAWN( 'DELETE KLC$PATH:'//enquiry//
        1               '*.LIS;*',,'KLC$PATH:ERROR.LOG;1' )
14                              s = SMG$ERASE_LINE( localdisplay, 23, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$ERASE_LINE( localdisplay, 24, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$PUT_CHARS( localdisplay,
        1       'ENQUIRY DELETED.  Press any key to continue ...', 23, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$READ_KEYSTROKE( keyboard, a )
                                IF (.NOT.s) CALL ERROR( s )
                                ELSE
                                s = SMG$ERASE_LINE( localdisplay, 23, 1)
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$PUT_CHARS( localdisplay,
        1                       'Can not delete this record today, try again 
        1 tomorrow.  Press any key...', 23, 1 )
                                IF (.NOT.s) CALL ERROR( s )
                                s = SMG$READ_KEYSTROKE( keyboard, a )
                                IF (.NOT.s) CALL ERROR( s )
                                END IF
                        ELSE
C       Else save the file correctly
                                a = SMG$K_TRM_UPPERCASE_Y
                                GOTO 10
                        END IF
                ELSE
C       If user pressed C then contiue else go back to get a keypress
                        IF ((a.NE.67).AND.(a.NE.99)) THEN
                                s = SMG$RING_BELL( localdisplay )
                                IF (.NOT.s) CALL ERROR( s )
                                GOTO 9
                        END IF
                END IF
        END IF

        s = SMG$UNPASTE_VIRTUAL_DISPLAY( datascreen, paste )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$DELETE_VIRTUAL_DISPLAY( datascreen )
        IF (.NOT.s) CALL ERROR( s )

15      CLOSE( UNIT=1 )
        s = SMG$UNPASTE_VIRTUAL_DISPLAY( localdisplay, paste )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$DELETE_VIRTUAL_DISPLAY( localdisplay )
        IF (.NOT.s) CALL ERROR( s )
        RETURN
C+
C       The rest of this module contains the error handling routines.
C       Jump to here if there is no info record for the selected enquiry.
C-
16      s = SMG$PUT_CHARS( localdisplay, 'Current Enquiry Number :'//enquiry,
        1               2, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 
        1       'Create New Enquiry [Y/N]:', 4, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF (.NOT.s) CALL ERROR( s )
        IF (.NOT.((a.EQ.SMG$K_TRM_UPPERCASE_Y).OR.
        1       (a.EQ.SMG$K_TRM_LOWERCASE_Y))) GOTO 15
17      s = SMG$PUT_CHARS( localdisplay, 'Enter Customer Short Code For 
        1This Enquiry :                      ', 5, 1 )
        IF (.NOT.s) CALL ERROR(s )
        s = SMG$PUT_CHARS( localdisplay, 'Enter data followed by RETURN or QUIT
        1 to end.                        ', 4, 1 )
        IF (.NOT.s) CALL ERROR( s )
C       Get customer short code
18      CALL RSTRING( localdisplay, string, 80, 45, 5, 7, keyboard, paste,
        1       status, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        CALL UCASE( string(1:7), s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        IF (status.NE.13) GOTO 18
        short(1:7)=string(1:7)
        IF (short(1:4).EQ.'QUIT') GOTO 15
        eline(1001).partnum(1:7)=short
        eline(1001).item='     '
        eline(1001).potsupp(1)(1:4)='    '
19      CALL CRENEWNUM( enquiry, s )
        IF (s.NE.%LOC(KLC_NORMAL)) CALL ERROR( s )
        DO i=1,1001
                eline(i).enqnum=enquiry
        END DO
        WRITE( UNIT=1, FMT=100, ERR=19 )
        1       enquiry, eline(1001).item, eline(1001).qty,
        1       eline(1001).partnum, eline(1001).descr, eline(1001).potsupp(1), 
        1       eline(1001).potsupp(2), eline(1001).potsupp(3),
        1       eline(1001).potsupp(4), eline(1001).potsupp(5),
        1       eline(1001).suppused, eline(1001).revpartnum,
        1       eline(1001).discount, eline(1001).cdiscount,
        1       eline(1001).unitprice, eline(1001).profit, 
        1       eline(1001).numshipped
        GOTO 1
C+
C       The next routine is called if you try to specify a customer short
C       code that does not exist
C-
20      s = SMG$RING_BELL( localdisplay )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'The customer short code '//
        1       eline(1001).partnum(1:7)//' Does not exist.', 4, 1 )
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$PUT_CHARS( localdisplay, 'Press Any Key to enter new short
        1 code ...                       ', 5, 1)
        IF (.NOT.s) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF (.NOT.s) CALL ERROR( s )
        GOTO 17
C       If two item codes are the same then tell user and go back to edit
21      s = SMG$PUT_CHARS( localdisplay, 
        1       'You can not have two items the same.'
        1       , 22, 1 )
        IF (.NOT.s) CALL ERROR( s )
        y = i
        fnum = 1
        x = 1
        vx = 1
        vy = i
        GOTO 3
	END
