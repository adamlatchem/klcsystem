C+
C       The next routine will write out a line of information using the field
C       formatting read in by GETFORMAT.  The routine returns the total written
C       to the open file.
C-
CDEC$   IDENT 'KLC SYSTEM: WRITELINE v1.2'
        SUBROUTINE WRITELINE( eline, formats, nunit, total, errorc )
C
10      FORMAT(BNI5.1)
12      FORMAT(BNF10.2)
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
        RECORD /line/ eline
        CHARACTER*80    formats, line*100, cmd*1
        INTEGER*4       nunit, errorc, qty
        REAL            discount, profit, uprice, cdiscount, ctotal, stotal
        REAL            cuprice, suprice, total
        EXTERNAL        KLC_NORMAL
        errorc = %LOC(KLC_NORMAL)
        temp = '          '
        DO i=0,100
                line(i:i)=' '
        ENDDO
C       Make sure there are no strange charaters in the datafields
        DO i=1,158
                IF (eline.all(i:i).LT.' ') eline.all(i:i)=' '
        ENDDO
C       Convert characters to numeric data
        DECODE(5, 12, eline.discount) discount
        DECODE(5, 12, eline.cdiscount) cdiscount
        DECODE(5, 12, eline.profit) profit
        DECODE(5, 10, eline.qty) qty
        DECODE(10, 12, eline.unitprice) uprice
C       perform calculations to find unit price for customer and supplier.
        cuprice = uprice-((cdiscount/100.0)*uprice)
        cuprice = cuprice/((100.0-profit)/100.0)
        suprice = uprice-((discount/100.0)*uprice)
        ENCODE(10, 12, eline.unitprice) cuprice
        DECODE(10, 12, eline.unitprice) cuprice
        ENCODE(10, 12, eline.unitprice) suprice
        DECODE(10, 12, eline.unitprice) suprice
C       calculate totals for line
        ctotal = cuprice * qty
        stotal = suprice * qty
C       Now read the format string and copy information to line varaible
        DO i=1, 80
                cmd = formats(i:i)
                IF (cmd.EQ.'0') line(i-1:i+5)=' '//eline.item
                IF (cmd.EQ.'1') line(i-1:i+30)=' '//eline.descr
                IF (cmd.EQ.'2') line(i-1:i+18)=' '//eline.partnum
                IF (cmd.EQ.'3') line(i-1:i+18)=' '//eline.revpartnum
                IF (cmd.EQ.'4') line(i-1:i+5)=' '//eline.qty
                IF (cmd.EQ.'5') THEN
                        ENCODE( 10, 12, eline.unitprice, ERR=1234 ) suprice
                        line(i-1:i+10)=' '//eline.unitprice
                ENDIF
                IF (cmd.EQ.'6') THEN
                        ENCODE( 10, 12, eline.unitprice, ERR=1234 ) cuprice
                        line(i-1:i+10)=' '//eline.unitprice
                ENDIF
                IF (cmd.EQ.'7') THEN
                        ENCODE( 10, 12, eline.unitprice, ERR=1234 ) stotal
                        total = stotal
                        line(i-1:i+10)=' '//eline.unitprice
                ENDIF
                IF (cmd.EQ.'8') THEN
                        ENCODE( 10, 12, eline.unitprice, ERR=1234 ) ctotal
                        total = ctotal
                        line(i-1:i+10)=' '//eline.unitprice
                ENDIF
                IF (cmd.EQ.'9') line(i-1:i+5)=' '//eline.numshipped
30      END DO
C       Now write out the line to the file
        WRITE( UNIT=nunit, FMT='(A)' ) line(1:80)
C       If the revised partnumber flag is set then display revised part numbers
        IF ((INDEX(formats,'R').NE.0).AND.(eline.partnum.NE.eline.revpartnum))
        1       WRITE( UNIT=nunit, FMT='(A)') '    REVISED PART NUMBER - '//
        1       eline.revpartnum
        RETURN
1234    s = SMG$PUT_CHARS( localdisplay,
        1       'A value for item '//eline.item//' is too large.'
        1       , 21, 1 )
        GOTO 30
	END
