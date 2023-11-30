
SUBROUTINE mplprt
   IMPLICIT NONE
   INTEGER H1(32) , H2(32) , H3(32) , Junk1(6) , Junk2(2) , Line , Lmpl , Mpl(1) , Mplpnt , Nb , Nfist , Nlpp , No , Npfist , T1(32)&
         & , T2(32) , T3(32)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL X(2,1)
   DOUBLE PRECISION Xx(1)
   COMMON /output/ T1 , T2 , T3 , H1 , H2 , H3
   COMMON /system/ Nb , No , Junk1 , Nlpp , Junk2 , Line
   COMMON /xfist / Nfist
   COMMON /xgpi2 / Lmpl , Mplpnt , Mpl
   COMMON /xgpi2x/ Xx
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /xpfist/ Npfist
   INTEGER add(2) , flag , flagb , flags , flgtot , h1x(32) , h2x(32) , h3x(32) , i , i0 , i1 , i2 , ip , j1 , j2 , kp(6) , l , l1 ,&
         & l2 , m , m1 , m2 , mplid , np , npad , tot
!
!     PRINTS MPL TABLE FOR DOCUMENTATION PURPOSES
!     AND CHECKS VALIDITY OF MANY ITEMS.
!
!
   !>>>>EQUIVALENCE (Xx(1),X(1,1))
!
   DATA kp/1 , 1 , 2 , 2 , 2 , 4/ , add/4HADD  , 4H    /
   DATA flagb/1H / , flags/4H ***/
   DATA h1x/4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H   M , 4H O D , 4H U L , 4H E   , 4H P R ,    &
       &4H O P , 4H E R , 4H T I , 4H E S , 4H   L , 4H I S , 4H T   , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
   DATA h2x/4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     , 4H     ,    &
       &4H   - , 4H - - , 4H - - , 4H - - , 4H P A , 4H R A , 4H M E , 4H T E , 4H R S , 4H - - , 4H - - , 4H - - , 4H - - ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
   DATA h3x/4H  MP , 4HLID  , 4HNWDS , 4H  WD , 4H1  M , 4HOD-N , 4HAME  , 4HTYP  , 4H IN  , 4H OUT , 4H  SC , 4HR  T , 4HOT   ,    &
       &4H   I , 4HD TY , 4HP    , 4HP    , 4H   D , 4HEFAU , 4HLT ( , 4HIF A , 4HNY)  , 4H     , 4H   W , 4H1-W2 , 4H FLG ,        &
      & 4H     , 4H     , 4H     , 4H     , 4H     , 4H    /
!
!     INITIALIZATION
!
   CALL page
   mplid = 0
   npad = 0
   i2 = 0
   DO i = 1 , 32
      H1(i) = h1x(i)
      H2(i) = h2x(i)
      H3(i) = h3x(i)
   ENDDO
   CALL page
   DO
!
!     PROCESS NEXT ENTRY
!
      IF ( i2<Lmpl ) THEN
         i0 = i2
         i1 = i2 + 1
         i2 = i0 + Mpl(i1)
         mplid = mplid + 1
!
!     TEST FOR MODULE TYPE
!
         IF ( Mpl(i1)<1 ) THEN
!
            WRITE (No,99001) Swm
99001       FORMAT (A27,' 68, ILLEGAL WORD COUNT.')
            GOTO 400
         ELSEIF ( Mpl(i1)/=1 ) THEN
            IF ( Mpl(i1+1)/=0 ) THEN
               IF ( Mpl(i0+4)>=3 ) THEN
!
!     EXECUTIVE MODULE
!
                  CALL page2(-2)
                  l1 = i0 + 2
                  l2 = l1 + 2
                  WRITE (No,99002) mplid , Mpl(i1) , i1 , (Mpl(l),l=l1,l2)
99002             FORMAT (7H0      ,3I5,2X,2A4,I3)
                  CYCLE
!
!     FUNCTIONAL MODULE
!
               ELSEIF ( Mpl(i1)>7 ) THEN
!
!     PARAMETERS EXIST FOR THIS FUNCTIONAL MODULE
!
!
!     DETERMINE THE NUMBER OF PARAMETERS FOR FUNCTIONAL MODULE
!
                  np = 0
                  i = i0 + 8
                  DO
                     IF ( (i-1)<(i2) ) THEN
                        ip = iabs(Mpl(i))
                        IF ( ip>6 ) GOTO 200
                        IF ( Mpl(i)<0 ) THEN
                           np = np + 1
                           i = i + 1
                        ELSEIF ( Mpl(i)==0 ) THEN
                           GOTO 200
                        ELSE
                           np = np + 1
                           i = i + 1 + kp(ip)
                        ENDIF
                     ELSEIF ( (i-1)==(i2) ) THEN
                        IF ( np<=0 ) GOTO 300
!
                        CALL page2(-2-np)
                        l1 = i0 + 5
                        l2 = l1 + 2
                        tot = 0
                        DO l = l1 , l2
                           tot = tot + Mpl(l)
                        ENDDO
                        flgtot = flagb
                        IF ( tot>Nfist-Npfist ) flgtot = flags
                        l1 = i0 + 2
                        WRITE (No,99003) mplid , Mpl(i1) , i1 , (Mpl(l),l=l1,l2) , tot , flgtot
!
99003                   FORMAT (7H0      ,3I5,2X,2A4,I3,4I5,10X,A3)
!
!     PRINT PARAMETERS
!
                        np = 0
                        i = i0 + 8
                        j2 = 0
                        DO
                           np = np + 1
                           j1 = j2 + 1
                           IF ( (i-1)<(i2) ) THEN
                              ip = iabs(Mpl(i))
                              IF ( ip>6 ) GOTO 200
                              IF ( Mpl(i)<0 ) THEN
!
!     PARAMETER HAS NO DEFAULT VALUE
!
                                 j2 = j1
                                 IF ( ip==2 ) THEN
!
!     REAL SINGLE-PRECISION
!
                                    WRITE (No,99004) np , i , j1
99004                               FORMAT (59X,I2,5H. RSP,I5,7X,16H-- NO DEFAULT --,6X,I2)
                                 ELSEIF ( ip==3 ) THEN
!
!     ALPHANUMERIC (BCD)
!
                                    j2 = j2 + 1
                                    WRITE (No,99005) np , i , j1 , j2
99005                               FORMAT (59X,I2,5H. BCD,I5,7X,16H-- NO DEFAULT --,6X,I2,1H-,I2)
                                 ELSEIF ( ip==4 ) THEN
!
!     REAL DOUBLE-PRECISION
!
                                    j2 = j2 + 1
                                    flag = flagb
                                    IF ( mod(j1,2)==0 ) flag = flags
                                    WRITE (No,99006) np , i , j1 , j2 , flag
99006                               FORMAT (59X,I2,5H. RDP,I5,7X,16H-- NO DEFAULT --,6X,I2,1H-,I2,A4)
                                 ELSEIF ( ip==5 ) THEN
!
!     COMPLEX SINGLE-PRECISION
!
                                    j2 = j2 + 1
                                    flag = flagb
                                    IF ( mod(j1,2)==0 ) flag = flags
                                    WRITE (No,99007) np , i , j1 , j2 , flag
99007                               FORMAT (59X,I2,5H. CSP,I5,7X,16H-- NO DEFAULT --,6X,I2,1H-,I2,A4)
                                 ELSEIF ( ip==6 ) THEN
!
!     COMPLEX DOUBLE-PRECISION
!
                                    j2 = j2 + 3
                                    flag = flagb
                                    IF ( mod(j1,2)==0 ) flag = flags
                                    WRITE (No,99008) np , i , j1 , j2 , flag
99008                               FORMAT (59X,I2,5H. CDP,I5,7X,16H-- NO DEFAULT --,6X,I2,1H-,I2,A4)
                                 ELSE
!
!     INTEGER
!
                                    WRITE (No,99009) np , i , j1
99009                               FORMAT (59X,I2,5H. INT,I5,7X,16H-- NO DEFAULT --,6X,I2)
                                 ENDIF
                                 i = i + 1
                              ELSEIF ( Mpl(i)==0 ) THEN
                                 GOTO 200
!
!     PARAMETER HAS A DEFAULT VALUE
!
                              ELSEIF ( ip==2 ) THEN
!
!     REAL SINGLE-PRECISION
!
                                 j2 = j1
                                 m = Mpl(i+1)
                                 WRITE (No,99010) np , i , X(1,m) , j1
99010                            FORMAT (59X,I2,5H. RSP,I5,1P,E20.4,9X,I2)
                                 i = i + 2
                              ELSEIF ( ip==3 ) THEN
!
!     ALPHANUMERIC (BCD)
!
                                 j2 = j1 + 1
                                 WRITE (No,99011) np , i , Mpl(i+1) , Mpl(i+2) , j1 , j2
99011                            FORMAT (59X,I2,5H. BCD,I5,11X,2A4,10X,I2,1H-,I2)
                                 i = i + 3
                              ELSEIF ( ip==4 ) THEN
!
!     REAL DOUBLE-PRECISION
!
                                 j2 = j1 + 1
                                 m = Mpl(i+1)
                                 flag = flagb
                                 IF ( mod(j1,2)==0 ) flag = flags
                                 WRITE (No,99012) np , i , Xx(m) , j1 , j2 , flag
99012                            FORMAT (59X,I2,5H. RDP,I5,1P,D20.4,9X,I2,1H-,I2,A4)
                                 i = i + 3
                              ELSEIF ( ip==5 ) THEN
!
!     COMPLEX SINGLE-PRECISION
!
                                 j2 = j1 + 1
                                 m = Mpl(i+1)
                                 flag = flagb
                                 IF ( mod(j1,2)==0 ) flag = flags
                                 WRITE (No,99013) np , i , X(1,m) , X(2,m) , j1 , j2 , flag
99013                            FORMAT (59X,I2,5H. CSP,I5,3H  (,1P,E11.4,1H,,1P,E11.4,3H   ,I2,1H-,I2,A4)
                                 i = i + 3
                              ELSEIF ( ip==6 ) THEN
!
!     COMPLEX DOUBLE-PRECISION
!
                                 j2 = j1 + 3
                                 m1 = Mpl(i+1)
                                 m2 = Mpl(i+3)
                                 flag = flagb
                                 IF ( mod(j1,2)==0 ) flag = flags
                                 WRITE (No,99014) np , i , Xx(m1) , Xx(m2) , j1 , j2 , flag
99014                            FORMAT (59X,I2,5H. CDP,I5,3H  (,1P,D11.4,1H,,1P,D11.4,3H)  ,I2,1H-,I2,A4)
                                 i = i + 5
                              ELSE
!
!     INTEGER
!
                                 j2 = j1
                                 WRITE (No,99015) np , i , Mpl(i+1) , j1
99015                            FORMAT (59X,I2,5H. INT,I5,I15,14X,I2)
                                 i = i + 2
                              ENDIF
                           ELSEIF ( (i-1)==(i2) ) THEN
!
                              IF ( Mpl(l1)==add(1) .AND. Mpl(l1+1)==add(2) ) THEN
                                 CALL page2(-2)
                                 WRITE (No,99016)
99016                            FORMAT (10X,'NOTE - THE ABOVE PARAMETER DEFAULTS WILL BE CHANGED',                                 &
                                   &' TO ALL ZEROS BY THE ADD MODULE.  HOWEVER, IF ALL 4 PARAMETERS',' ARE NOT',/10X,               &
                                   &'SPECIFIED, THEY WILL BE CHANGED TO 2*(1.,0.),',                                                &
                                   &' 2*(0.D0,0.D0), OR 2*(0.,0.), 2*(1.D0,0.D0) DEPENDING ON ','MATRICES INVOLVED')
                              ENDIF
                              GOTO 100
                           ELSE
                              GOTO 300
                           ENDIF
                        ENDDO
                     ELSE
                        GOTO 300
                     ENDIF
                  ENDDO
               ELSE
!
!     NO PARAMETERS EXIST FOR THIS FUNCTIONAL MODULE
!
                  CALL page2(-2)
                  l1 = i0 + 5
                  l2 = l1 + 2
                  tot = 0
                  DO l = l1 , l2
                     tot = tot + Mpl(l)
                  ENDDO
                  flgtot = flagb
                  IF ( tot>Nfist-Npfist ) flgtot = flags
                  l1 = i0 + 2
                  WRITE (No,99017) mplid , Mpl(i1) , i1 , (Mpl(l),l=l1,l2) , tot , flgtot
99017             FORMAT (7H0      ,3I5,2X,2A4,I3,4I5,10X,50H----- N O   P A R A M E T E R S   E X I S T ----- ,10X,A3)
                  CYCLE
               ENDIF
            ENDIF
         ENDIF
!
!     PAD SPACE
!
         CALL page2(-2)
         WRITE (No,99018) mplid , Mpl(i1) , i1
99018    FORMAT (7H0      ,3I5,2X,8H (NONE) )
         npad = npad + 1
      ELSEIF ( i2==Lmpl ) THEN
!
!     TERMINATION
!
         CALL page2(-4)
         WRITE (No,99019)
99019    FORMAT ('0*** END OF MPL PRINTOUT')
         WRITE (No,99020) mplid , npad
99020    FORMAT ('0*** THE MPL CONTAINS ',I3,' ENTRYS.  OF THESE, ',I3,' ARE PAD ENTRYS.')
!
         RETURN
      ELSE
!
!     ERROR MESSAGES
!
         WRITE (No,99021) Swm , i2 , Lmpl
99021    FORMAT (A27,' 65, POINTER I2 =',I10,' DOES NOT AGREE WITH LMPL =',I11)
         GOTO 400
      ENDIF
 100  ENDDO
!
 200  WRITE (No,99022) Swm
99022 FORMAT (A27,' 66, ILLEGAL PARAMETER TYPE CODE.')
   GOTO 400
!
 300  WRITE (No,99023) Swm
99023 FORMAT (A27,' 67, ERROR IN PARAMETER SEQUENCE.')
!
!
 400  CALL page2(4)
   WRITE (No,99024)
99024 FORMAT (5X,'MPL TABLE LISTING CANCELLED.')
!
END SUBROUTINE mplprt
