!*==cf2fbs.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cf2fbs(Tpose,Xout,Iobuf)
   USE c_feeraa
   USE c_feerxc
   USE c_names
   USE c_system
   USE c_zntpkx
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   LOGICAL , DIMENSION(1) :: Tpose
   REAL(REAL64) , DIMENSION(1) :: Xout
   INTEGER , DIMENSION(1) :: Iobuf
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: dtemp , unidum
   INTEGER :: i , i1 , i2 , ii1 , ii2 , in1 , in2 , intchn , ioff , iscr6 , j , j1 , j2 , junk , k , mcsave
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL bckrec , close , gopen , intpk , mesage , skprec , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!*******
!     CF2FBS PERFORMS THE DOUBLE-PRECISION FORWARD AND BACKWARD SWEEPS
!     FOR THE COMPLEX FEER METHOD. THESE SWEEPS CONSTITUTE THE
!     OPERATIONAL INVERSE (MATRIX INVERSION).
!*******
!     DEFINITION OF INPUT AND OUTPUT PARAMETERS
!*******
!     TPOSE    = .FALSE. --- PERFORM OPERATION L * U
!              = .TRUE.  --- PERFORM OPERATION U-TRANSPOSE * L-TRANSPOSE
!     XOUT     = INPUT VECTOR GETS TRANSFORMED TO OUTPUT VECTOR
!     IOBUF    = INPUT  GINO BUFFER
!*******
   !>>>>EQUIVALENCE (Aadum(42),Iscr6)
   DATA name/4HCF2F , 4HBS  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( qpr ) WRITE (nout,99001) Tpose(1) , symmet , nswp , iscr6
99001    FORMAT (1H0,12HENTER CF2FBS,8X,11HTRANSPOSE =,L2,L9,2I10)
         junk = 0
         IF ( Tpose(1) .AND. .NOT.symmet ) THEN
!*******
!     BELOW FOR OPERATION U-TRANSPOSE * L-TRANSPOSE
!     (LOGIC COPIED FROM SUBROUTINE CDIFBS)
!*******
!     BEGIN THE FORWARD PASS USING THE UPPER TRIANGLE
!*******
            ioff = mcbut(7) - 1
            IF ( qpr ) WRITE (nout,99002) ioff
99002       FORMAT (1H ,30X,6HIOFF =,I10)
            mcsave = mcbut(1)
            mcbut(1) = iscr6
            CALL gopen(mcbut(1),Iobuf(1),rdrew)
            DO i = 1 , nswp
               IF ( qpr ) WRITE (nout,99010) i
               j = i + i
               CALL intpk(*10,mcbut(1),0,cdp,0)
               SPAG_Loop_2_1: DO
                  CALL zntpki
                  IF ( qpr ) WRITE (nout,99011) ii , eol , da
                  IF ( ii<i ) THEN
!*******
!     SUBTRACT OFF NORMAL TERM
!*******
                     i2 = ii + ii
                     i1 = i2 - 1
                     j1 = j - 1
                     Xout(j1) = Xout(j1) - Xout(i1)*da(1) + Xout(i2)*da(2)
                     Xout(j) = Xout(j) - Xout(i1)*da(2) - Xout(i2)*da(1)
                  ELSEIF ( ii==i ) THEN
!*******
!     DIVIDE BY THE DIAGONAL
!*******
                     i1 = j - 1
                     unidum = 1.D0/(da(1)**2+da(2)**2)
                     dtemp = (Xout(i1)*da(1)+Xout(j)*da(2))*unidum
                     Xout(j) = (Xout(j)*da(1)-Xout(i1)*da(2))*unidum
                     Xout(i1) = dtemp
                     IF ( qpr ) WRITE (nout,99013)
                  ELSE
!*******
!     SUBTRACT OFF ACTIVE COLUMN TERMS
!*******
                     k = (i-ioff)*2
                     junk = 1
                     in1 = k
                     IF ( in1<=0 ) GOTO 40
                     i2 = ii + ii
                     i1 = i2 - 1
                     j1 = k - 1
                     Xout(i1) = Xout(i1) - Xout(j1)*da(1) + Xout(k)*da(2)
                     Xout(i2) = Xout(i2) - Xout(k)*da(1) - Xout(j1)*da(2)
                  ENDIF
                  IF ( eol/=0 ) EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
 10         ENDDO
            CALL close(mcbut(1),rew)
            mcbut(1) = mcsave
!*******
!     BEGIN BACKWARD PASS USING THE LOWER TRIANGLE
!*******
            CALL gopen(mcblt(1),Iobuf(1),rdrew)
            CALL skprec(mcblt(1),nswp)
            DO i = 1 , nswp
               IF ( qpr ) WRITE (nout,99010) i
               CALL bckrec(mcblt(1))
               intchn = 0
               CALL intpk(*15,mcblt(1),0,cdp,0)
               j = (nswp-i+1)*2
               SPAG_Loop_2_2: DO
                  CALL zntpki
                  IF ( qpr ) WRITE (nout,99011) ii , eol , da
                  IF ( ii/=nswp-i+1 ) THEN
                     j1 = j - 1
                     i2 = ii + ii
                     i1 = i2 - 1
                     Xout(j1) = Xout(j1) - Xout(i1)*da(1) + Xout(i2)*da(2)
                     Xout(j) = Xout(j) - Xout(i1)*da(2) - Xout(i2)*da(1)
                  ELSE
                     IF ( ii<j/2 ) THEN
                        spag_nextblock_1 = 9
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
!*******
!     PERFORM THE INTERCHANGE
!*******
                     intchn = ifix(sngl(da(1)))*2
                     IF ( qpr ) WRITE (nout,99003) intchn
99003                FORMAT (1H ,4X,11HINTERCHANGE,I6)
                  ENDIF
                  IF ( eol/=0 ) THEN
                     IF ( intchn>0 ) THEN
                        in1 = j + intchn
                        IF ( qpr ) WRITE (nout,99004) j , intchn , in1
99004                   FORMAT (1H ,15X,3I6)
                        dtemp = Xout(j)
                        Xout(j) = Xout(in1)
                        Xout(in1) = dtemp
                        j1 = j - 1
                        i1 = in1 - 1
                        dtemp = Xout(j1)
                        Xout(j1) = Xout(i1)
                        Xout(i1) = dtemp
                     ENDIF
                     EXIT SPAG_Loop_2_2
                  ENDIF
               ENDDO SPAG_Loop_2_2
 15            CALL bckrec(mcblt(1))
            ENDDO
            CALL close(mcblt(1),rew)
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ELSE
!*******
!     BELOW FOR OPERATION L * U
!     (LOGIC COPIED FROM SUBROUTINE CINFBS)
!*******
!     BEGIN FORWARD PASS USING THE LOWER TRIANGLE
!*******
            CALL gopen(mcblt(1),Iobuf(1),rdrew)
            j = 1
            CALL intpk(*20,mcblt(1),0,cdp,0)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         DO WHILE ( eol==0 )
            CALL zntpki
            IF ( qpr ) WRITE (nout,99012) da , ii , eol , j
            IF ( j<ii ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( j==ii ) THEN
!*******
!     PERFORM THE REQUIRED ROW INTERCHANGE
!*******
               in1 = (j+ifix(sngl(da(1))))*2 - 1
               IF ( qpr ) WRITE (nout,99005) in1 , eol
99005          FORMAT (1H ,3X,5HIN1 =,I6,4X,5HEOL =,I2)
               in2 = in1 + 1
               j2 = 2*j
               unidum = Xout(j2)
               Xout(j2) = Xout(in2)
               Xout(in2) = unidum
               j2 = j2 - 1
               unidum = Xout(j2)
               Xout(j2) = Xout(in1)
               Xout(in1) = unidum
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 9
      CASE (3)
         IF ( eol/=0 ) GOTO 20
         CALL zntpki
         IF ( qpr ) WRITE (nout,99012) da , ii , eol , j
         spag_nextblock_1 = 4
      CASE (4)
         ii2 = 2*ii
         ii1 = ii2 - 1
         j2 = 2*j
         j1 = j2 - 1
         Xout(ii1) = Xout(ii1) - da(1)*Xout(j1) + da(2)*Xout(j2)
         Xout(ii2) = Xout(ii2) - da(2)*Xout(j1) - da(1)*Xout(j2)
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 20      j = j + 1
         IF ( j<nswp ) THEN
            CALL intpk(*20,mcblt(1),0,cdp,0)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL close(mcblt(1),rew)
!*******
!     BEGIN BACKWARD PASS USING THE UPPER TRIANGLE
!*******
            ioff = mcbut(7) - 1
            IF ( qpr ) WRITE (nout,99006) ioff , mcblt , mcbut
99006       FORMAT (1H ,15(1X,I7))
            CALL gopen(mcbut(1),Iobuf(1),rdrew)
            j = nswp
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         CALL intpk(*40,mcbut(1),0,cdp,0)
         IF ( eol/=0 ) GOTO 40
         spag_nextblock_1 = 6
      CASE (6)
         CALL zntpki
         IF ( qpr ) WRITE (nout,99012) da , ii , eol , j
         i = nswp - ii + 1
         IF ( i/=j ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!*******
!     DIVIDE BY THE DIAGONAL
!*******
         i2 = 2*i
         i1 = i2 - 1
         unidum = 1.D0/(da(1)**2+da(2)**2)
         dtemp = (da(1)*Xout(i1)+da(2)*Xout(i2))*unidum
         Xout(i2) = (da(1)*Xout(i2)-da(2)*Xout(i1))*unidum
         Xout(i1) = dtemp
         IF ( qpr ) WRITE (nout,99013)
         spag_nextblock_1 = 7
      CASE (7)
!*******
!     SUBTRACT OFF REMAINING TERMS
!*******
         IF ( i>j ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( eol/=0 ) THEN
            j = j - 1
            IF ( j>0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL close(mcbut(1),rew)
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL zntpki
            IF ( qpr ) WRITE (nout,99012) da , ii , eol , j
            i = nswp - ii + 1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         in1 = i
         in2 = j
         IF ( i>=j ) THEN
            k = in1
            in1 = in2 - ioff
            junk = 1
            IF ( in1<=0 ) GOTO 40
            in2 = k
         ENDIF
         in1 = 2*in1
         in2 = 2*in2
         ii1 = in1 - 1
         ii2 = in2 - 1
         IF ( qpr ) WRITE (nout,99007) i , j , ii1 , ii2
99007    FORMAT (1H ,3HI =,I6,6X,3HJ =,I6,6X,5HII1 =,I6,6X,5HII2 =,I6)
         Xout(ii1) = Xout(ii1) - da(1)*Xout(ii2) + da(2)*Xout(in2)
         Xout(in1) = Xout(in1) - da(2)*Xout(ii2) - da(1)*Xout(in2)
         spag_nextblock_1 = 7
      CASE (9)
         j = mcblt(1)
         spag_nextblock_1 = 10
         CYCLE SPAG_DispatchLoop_1
 40      j = mcbut(1)
         spag_nextblock_1 = 10
      CASE (10)
         CALL mesage(-5,j,name)
         spag_nextblock_1 = 11
      CASE (11)
         IF ( qpr .AND. junk==0 ) WRITE (nout,99008)
99008    FORMAT (1H0,30X,13HIOFF NOT USED,/1H )
         IF ( qpr .AND. junk/=0 ) WRITE (nout,99009)
99009    FORMAT (1H0,30X,13HIOFF WAS USED,/1H )
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99010 FORMAT (1H ,12HLOOP INDEX =,I6)
99011 FORMAT (1H ,4HII =,I14,6X,5HEOL =,I2,8X,4HDA =,2D16.8)
99012 FORMAT (1H ,4HDA =,2D16.8,4X,4HII =,I6,4X,5HEOL =,I2,4X,3HJ =,I4)
99013 FORMAT (1H ,6X,8HDIAGONAL)
END SUBROUTINE cf2fbs
