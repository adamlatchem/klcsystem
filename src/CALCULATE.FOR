C+
C
C VERSION:      1.2-006
C
C FACILITY:     KLC
C
C MODULE:       CALCULATE
C
C ABSTRACT:     This routine assumes that the display is set up as per
C               CENQUIRE etc. It will calculate the profit and customer prices
C               for costing purposes.
C
C ENVIRONMENT:  Called by CENQUIRE from KLC system.  'display' Is the virtual
C               display identifier to which the calculations are to be
C               performed.  'msgdisp' Is the virtual display on which output can
C               be produced if there are any errors produced during the
C               calculation run.  'y' Is the line of the screen on which the
C               calculation is to be performed.  'costklc' Is a variable into
C               which is returned the total cost for the specified line to
C               the company (KLC).  'costcust' Is the total cost to the customer
C               due to the order made.  These values are stored in arrays on
C               return so that overall statistics calculations may be
C               performed.
C
C AUTHOR:       ADAM LATCHEM                    CREATION DATE:  7-APR-1993
C
C MODIFIED BY:
C
C-
CDEC$   IDENT 'KLC SYSTEM: CALCULATE v1.2'
        SUBROUTINE CALCULATE( msgdisp, display, y, costklc, costcust,
        1       pasteboard, keyboard, errorc )
        IMPLICIT INTEGER(a-z)
        INCLUDE '($SMGDEF)'
C
        INTEGER*4       display, y, qty, msgdisp, errin, errdsp, pasteboard
        INTEGER*4       keyboard, errorc
        REAL            price, prof, costklc, costcust, discount, cdiscount
        REAL            profit
        CHARACTER*200   datastring, temp, item*5, field*14, title(5)*14
        DATA            title(1) / 'Discount' /, title(2) / 'Cust. Discount' /
        DATA            title(3) / 'Profit' /, title(4) / 'Quantity' /
        DATA            title(5) / 'Unit Price' /
        EXTERNAL        KLC_NORMAL, KLC_CARITHE
C+
C       The routine works by taking a line of characters from the display.
C       The reason that this is performed is because SMG$ requires a copy
C       of the entire display in memory in order to work.  It is thus rather
C       silly to have two copies of the same data i.e. one in the program and
C       one made by SMG$.  Firstly we get the character data off the screen.
C-
        errorc = %LOC(KLC_NORMAL)
        s = SMG$READ_FROM_DISPLAY( display, datastring,, y )
        IF ( .NOT.s ) CALL ERROR( s )
        item = datastring( 1:5 )
C+
C       now we convert the character data to numeric values in variables
C       First get the percentages.  We know the absolute charcater reference
C       values so we can just substitute these in a sub-string specifier in
C       order to get all the relevant information.  The format command tells 
C       the computer that we want to ignore all blank spaces ( BN ) and we want
C       to get an integer of upto 10 chacrters in length with at least 1 
C       numeric in ( I10.1 ).  The 'errin' variable is used to tell the user
C       what caused the error if an error does occur during calculation.
C-
1       FORMAT( BNI10.1 )
        errin = 1
        DECODE( 5, 2, datastring( 82:86 ), ERR=4 ) discount
        errin = 2
        DECODE( 5, 2, datastring( 92:96 ), ERR=4 ) cdiscount
        errin = 3
        DECODE( 5, 2, datastring( 126:128 ), ERR=4 ) profit
        errin = 4
        DECODE( 5, 1, datastring( 7:11 ), ERR=4 ) qty
C+
C       Now we get the price which is a real value.  This is specified by the
C       F10.2 format parameter.  This says that we can have upto 10 characters
C       and 2 decimal places.
C-
2       FORMAT( BNF10.2 )
        errin = 5
        DECODE( 10, 2, datastring( 63:72 ), ERR=4 ) price
C+
C       Now do the only bit of worthy FORTRAN calculation in the application !
C       We find the cost to KLC by taking the discount value off the price.
C-
        costklc = price - ( ( discount/100.0 ) * price )
C       Make costklc only correct to 2 d.p. .
        ENCODE( 10, 2, temp( 1:10 ), ERR=4 ) costklc
        DECODE( 10, 2, temp( 1:10 ), ERR=4 ) costklc
C+
C       The final cost to the company KLC is calculated by multipling the price
C       by the number of that item that are required.
C-
        errin = 4
        costklc = costklc * qty
        costcust = price - ( ( cdiscount/100.0 ) * price )
        costcust = costcust / ( ( 100.0-profit ) / 100.0 )
C       Make costcust only correct to 2 d.p.
        ENCODE( 10, 2, temp( 1:10 ), ERR=4 ) costcust
        DECODE( 10, 2, temp( 1:10 ), ERR=4 ) costcust
C       Again we calculate the cost to the customer.
        costcust = costcust * qty
        prof = costcust - costklc
C+
C       Now print the customer cost out at the end of the line that is
C       being processed currently.
C-
3       errin = 4
        ENCODE( 10, 2, datastring( 1:10 ), ERR=4 ) costcust
        s = SMG$PUT_CHARS( display, datastring( 1:10 ), y, 183 )
        IF ( .NOT.s ) CALL ERROR( s )
C       Now print the profit margin.
        errin = 0
        ENCODE( 10, 2, datastring( 1:10 ), ERR=4 ) prof
        s = SMG$PUT_CHARS( display, datastring( 1:10 ), y, 201 )
        IF ( .NOT.s ) CALL ERROR( s )
        RETURN
C+
C       In every one of the ENCODE/DECODE statements there is an ERR=4
C       switch.  This causes a jump to the label 4 on errors. The following 
C       routine deals with overflows in data conversion.  It sets up default
C       values, displays an error message and jumps to the part of the code 
C       that prints the results upon the screen.
C-
4       costklc = 0
        costcust = 0
        prof = 0
        s = SMG$RING_BELL( display )
        IF ( .NOT.s ) CALL ERROR( s )
C       We create a display to put the error information in
        s = SMG$CREATE_VIRTUAL_DISPLAY( 6, 32, errdsp, SMG$M_BORDER )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PASTE_VIRTUAL_DISPLAY( errdsp, pasteboard, 3, 3 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( errdsp, 'ARITHMETIC ERROR !', 1, 8 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( errdsp,
        1       'Arithmetic error  occurred for', 3, 2 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$PUT_CHARS( errdsp, 'item '//item//'.', 4, 2 )
        IF ( .NOT.s ) CALL ERROR( s )
        IF ( errin.NE.0 ) THEN
                field = title( errin )
                s = SMG$PUT_CHARS( errdsp, ' - '//field, 4, 11 )
                IF ( .NOT.s ) CALL ERROR( s )
                s = SMG$PUT_CHARS( errdsp, 'Reduce magnitude of this field', 
        1               5, 2 )
                IF ( .NOT.s ) CALL ERROR( s )
        ELSE
                s = SMG$PUT_CHARS( errdsp, 'Reduce   price  and  quantity.',
        1               5, 2 )
                IF ( .NOT.s ) CALL ERROR( s )
        ENDIF
        s = SMG$PUT_CHARS( errdsp, 'Press  any  key  to  continue.', 6, 2 )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$READ_KEYSTROKE( keyboard, a )
        IF ( .NOT.s ) CALL ERROR( s )
        s = SMG$DELETE_VIRTUAL_DISPLAY( errdsp )
        IF ( .NOT.s ) CALL ERROR( s )
C       Unfortunately we must use a GOTO statement arrghh !
        errorc = %LOC(KLC_CARITHE)
        GOTO 3
	END
