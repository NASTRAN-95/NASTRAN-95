!*==mplprt.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mplprt
   USE c_output
   USE c_system
   USE c_xfist
   USE c_xgpi2
   USE c_xgpi2x
   USE c_xmssg
   USE c_xpfist
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: add
   INTEGER :: flag , flgtot , i , i0 , i1 , i2 , ip , j1 , j2 , l , l1 , l2 , m , m1 , m2 , mplid , np , npad , tot
   INTEGER , SAVE :: flagb , flags
   INTEGER , DIMENSION(32) , SAVE :: h1x , h2x , h3x
   INTEGER , DIMENSION(6) , SAVE :: kp
   REAL , DIMENSION(2,1) :: x
   EXTERNAL page , page2
!
! End of declarations rewritten by SPAG
!
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
      h1(i) = h1x(i)
      h2(i) = h2x(i)
      h3(i) = h3x(i)
   ENDDO
   CALL page
   SPAG_Loop_1_1: DO
!
!     PROCESS NEXT ENTRY
!
      IF ( i2<lmpl ) THEN
         i0 = i2
         i1 = i2 + 1
         i2 = i0 + mpl(i1)
         mplid = mplid + 1
!
!     TEST FOR MODULE TYPE
!
         IF ( mpl(i1)<1 ) THEN
!
            WRITE (no,99001) swm
99001       FORMAT (A27,' 68, ILLEGAL WORD COUNT.')
            CALL spag_block_2
            RETURN
         ELSEIF ( mpl(i1)/=1 ) THEN
            IF ( mpl(i1+1)/=0 ) THEN
               IF ( mpl(i0+4)>=3 ) THEN
!
!     EXECUTIVE MODULE
!
                  CALL page2(-2)
                  l1 = i0 + 2
                  l2 = l1 + 2
                  WRITE (no,99002) mplid , mpl(i1) , i1 , (mpl(l),l=l1,l2)
99002             FORMAT (7H0      ,3I5,2X,2A4,I3)
                  CYCLE
!
!     FUNCTIONAL MODULE
!
               ELSEIF ( mpl(i1)>7 ) THEN
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
                        ip = iabs(mpl(i))
                        IF ( ip>6 ) EXIT SPAG_Loop_1_1
                        IF ( mpl(i)<0 ) THEN
                           np = np + 1
                           i = i + 1
                        ELSEIF ( mpl(i)==0 ) THEN
                           EXIT SPAG_Loop_1_1
                        ELSE
                           np = np + 1
                           i = i + 1 + kp(ip)
                        ENDIF
                     ELSEIF ( (i-1)==(i2) ) THEN
                        IF ( np<=0 ) THEN
                           CALL spag_block_1
                           RETURN
                        ENDIF
!
                        CALL page2(-2-np)
                        l1 = i0 + 5
                        l2 = l1 + 2
                        tot = 0
                        DO l = l1 , l2
                           tot = tot + mpl(l)
                        ENDDO
                        flgtot = flagb
                        IF ( tot>nfist-npfist ) flgtot = flags
                        l1 = i0 + 2
                        WRITE (no,99003) mplid , mpl(i1) , i1 , (mpl(l),l=l1,l2) , tot , flgtot
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
                              ip = iabs(mpl(i))
                              IF ( ip>6 ) EXIT SPAG_Loop_1_1
                              IF ( mpl(i)<0 ) THEN
!
!     PARAMETER HAS NO DEFAULT VALUE
!
                                 j2 = j1
                                 IF ( ip==2 ) THEN
!
!     REAL SINGLE-PRECISION
!
                                    WRITE (no,99004) np , i , j1
99004                               FORMAT (59X,I2,5H. RSP,I5,7X,16H-- NO DEFAULT --,6X,I2)
                                 ELSEIF ( ip==3 ) THEN
!
!     ALPHANUMERIC (BCD)
!
                                    j2 = j2 + 1
                                    WRITE (no,99005) np , i , j1 , j2
99005                               FORMAT (59X,I2,5H. BCD,I5,7X,16H-- NO DEFAULT --,6X,I2,1H-,I2)
                                 ELSEIF ( ip==4 ) THEN
!
!     REAL DOUBLE-PRECISION
!
                                    j2 = j2 + 1
                                    flag = flagb
                                    IF ( mod(j1,2)==0 ) flag = flags
                                    WRITE (no,99006) np , i , j1 , j2 , flag
99006                               FORMAT (59X,I2,5H. RDP,I5,7X,16H-- NO DEFAULT --,6X,I2,1H-,I2,A4)
                                 ELSEIF ( ip==5 ) THEN
!
!     COMPLEX SINGLE-PRECISION
!
                                    j2 = j2 + 1
                                    flag = flagb
                                    IF ( mod(j1,2)==0 ) flag = flags
                                    WRITE (no,99007) np , i , j1 , j2 , flag
99007                               FORMAT (59X,I2,5H. CSP,I5,7X,16H-- NO DEFAULT --,6X,I2,1H-,I2,A4)
                                 ELSEIF ( ip==6 ) THEN
!
!     COMPLEX DOUBLE-PRECISION
!
                                    j2 = j2 + 3
                                    flag = flagb
                                    IF ( mod(j1,2)==0 ) flag = flags
                                    WRITE (no,99008) np , i , j1 , j2 , flag
99008                               FORMAT (59X,I2,5H. CDP,I5,7X,16H-- NO DEFAULT --,6X,I2,1H-,I2,A4)
                                 ELSE
!
!     INTEGER
!
                                    WRITE (no,99009) np , i , j1
99009                               FORMAT (59X,I2,5H. INT,I5,7X,16H-- NO DEFAULT --,6X,I2)
                                 ENDIF
                                 i = i + 1
                              ELSEIF ( mpl(i)==0 ) THEN
                                 EXIT SPAG_Loop_1_1
!
!     PARAMETER HAS A DEFAULT VALUE
!
                              ELSEIF ( ip==2 ) THEN
!
!     REAL SINGLE-PRECISION
!
                                 j2 = j1
                                 m = mpl(i+1)
                                 WRITE (no,99010) np , i , x(1,m) , j1
99010                            FORMAT (59X,I2,5H. RSP,I5,1P,E20.4,9X,I2)
                                 i = i + 2
                              ELSEIF ( ip==3 ) THEN
!
!     ALPHANUMERIC (BCD)
!
                                 j2 = j1 + 1
                                 WRITE (no,99011) np , i , mpl(i+1) , mpl(i+2) , j1 , j2
99011                            FORMAT (59X,I2,5H. BCD,I5,11X,2A4,10X,I2,1H-,I2)
                                 i = i + 3
                              ELSEIF ( ip==4 ) THEN
!
!     REAL DOUBLE-PRECISION
!
                                 j2 = j1 + 1
                                 m = mpl(i+1)
                                 flag = flagb
                                 IF ( mod(j1,2)==0 ) flag = flags
                                 WRITE (no,99012) np , i , xx(m) , j1 , j2 , flag
99012                            FORMAT (59X,I2,5H. RDP,I5,1P,D20.4,9X,I2,1H-,I2,A4)
                                 i = i + 3
                              ELSEIF ( ip==5 ) THEN
!
!     COMPLEX SINGLE-PRECISION
!
                                 j2 = j1 + 1
                                 m = mpl(i+1)
                                 flag = flagb
                                 IF ( mod(j1,2)==0 ) flag = flags
                                 WRITE (no,99013) np , i , x(1,m) , x(2,m) , j1 , j2 , flag
99013                            FORMAT (59X,I2,5H. CSP,I5,3H  (,1P,E11.4,1H,,1P,E11.4,3H   ,I2,1H-,I2,A4)
                                 i = i + 3
                              ELSEIF ( ip==6 ) THEN
!
!     COMPLEX DOUBLE-PRECISION
!
                                 j2 = j1 + 3
                                 m1 = mpl(i+1)
                                 m2 = mpl(i+3)
                                 flag = flagb
                                 IF ( mod(j1,2)==0 ) flag = flags
                                 WRITE (no,99014) np , i , xx(m1) , xx(m2) , j1 , j2 , flag
99014                            FORMAT (59X,I2,5H. CDP,I5,3H  (,1P,D11.4,1H,,1P,D11.4,3H)  ,I2,1H-,I2,A4)
                                 i = i + 5
                              ELSE
!
!     INTEGER
!
                                 j2 = j1
                                 WRITE (no,99015) np , i , mpl(i+1) , j1
99015                            FORMAT (59X,I2,5H. INT,I5,I15,14X,I2)
                                 i = i + 2
                              ENDIF
                           ELSEIF ( (i-1)==(i2) ) THEN
!
                              IF ( mpl(l1)==add(1) .AND. mpl(l1+1)==add(2) ) THEN
                                 CALL page2(-2)
                                 WRITE (no,99016)
99016                            FORMAT (10X,'NOTE - THE ABOVE PARAMETER DEFAULTS WILL BE CHANGED',                                 &
                                   &' TO ALL ZEROS BY THE ADD MODULE.  HOWEVER, IF ALL 4 PARAMETERS',' ARE NOT',/10X,               &
                                   &'SPECIFIED, THEY WILL BE CHANGED TO 2*(1.,0.),',                                                &
                                   &' 2*(0.D0,0.D0), OR 2*(0.,0.), 2*(1.D0,0.D0) DEPENDING ON ','MATRICES INVOLVED')
                              ENDIF
                              CYCLE SPAG_Loop_1_1
                           ELSE
                              CALL spag_block_1
                              RETURN
                           ENDIF
                        ENDDO
                     ELSE
                        CALL spag_block_1
                        RETURN
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
                     tot = tot + mpl(l)
                  ENDDO
                  flgtot = flagb
                  IF ( tot>nfist-npfist ) flgtot = flags
                  l1 = i0 + 2
                  WRITE (no,99017) mplid , mpl(i1) , i1 , (mpl(l),l=l1,l2) , tot , flgtot
99017             FORMAT (7H0      ,3I5,2X,2A4,I3,4I5,10X,50H----- N O   P A R A M E T E R S   E X I S T ----- ,10X,A3)
                  CYCLE
               ENDIF
            ENDIF
         ENDIF
!
!     PAD SPACE
!
         CALL page2(-2)
         WRITE (no,99018) mplid , mpl(i1) , i1
99018    FORMAT (7H0      ,3I5,2X,8H (NONE) )
         npad = npad + 1
      ELSEIF ( i2==lmpl ) THEN
!
!     TERMINATION
!
         CALL page2(-4)
         WRITE (no,99019)
99019    FORMAT ('0*** END OF MPL PRINTOUT')
         WRITE (no,99020) mplid , npad
99020    FORMAT ('0*** THE MPL CONTAINS ',I3,' ENTRYS.  OF THESE, ',I3,' ARE PAD ENTRYS.')
!
         RETURN
      ELSE
!
!     ERROR MESSAGES
!
         WRITE (no,99021) swm , i2 , lmpl
99021    FORMAT (A27,' 65, POINTER I2 =',I10,' DOES NOT AGREE WITH LMPL =',I11)
         CALL spag_block_2
         RETURN
      ENDIF
   ENDDO SPAG_Loop_1_1
!
   WRITE (no,99022) swm
99022 FORMAT (A27,' 66, ILLEGAL PARAMETER TYPE CODE.')
   CALL spag_block_2
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
!
      WRITE (No,99001) Swm
99001 FORMAT (A27,' 67, ERROR IN PARAMETER SEQUENCE.')
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!
      CALL page2(4)
      WRITE (No,99001)
99001 FORMAT (5X,'MPL TABLE LISTING CANCELLED.')
   END SUBROUTINE spag_block_2
!
END SUBROUTINE mplprt
