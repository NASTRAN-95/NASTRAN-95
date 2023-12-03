!*==detck.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE detck(Jarg,Ifgpst,Npvt)
   USE c_ma1xx
   USE c_system
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Jarg
   INTEGER :: Ifgpst
   INTEGER :: Npvt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: back , i , i1 , iarg , igoto , ii , inc1 , inc2 , inc3 , iorder , ip , ipoint , isave , itemp , j , j1 , jj , k1 ,    &
            & k2 , kk , kount , ll , nwds , tnwds
   REAL(REAL64) :: const , det , dtol , fm , fr , temp
   INTEGER , DIMENSION(8) :: iarray
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , SAVE :: neor
   EXTERNAL mesage , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     COMMENTS FROM G.CHAN/UNISYS, 5/1991,
!     THIS ROUTINE WAS NAMED DETCKX BEFORE, WHICH HAD NOT BEEN TESTED.
!     THE ONE THAT USED TO BE DETCK APPEARS TO BE AN OLDER VERSION, AND
!     SHOULD BE REPLACED BY THIS ONE, IF THIS ONE WORKS
!
!     THIS ROUTINE GENERATES THE GRID POINT SINGULARITY TABLE BY
!     EXAMINING THE TRANSLATIONAL AND DIAGONAL 3 X 3 SUBMATRICES OF THE
!     KGG MATRIX.
!     IF JARG = 0, THE PIVOT POINT HAS ELEMENTS ATTACHED TO IT.
!     IF JARG =-1, THE PIVOT IS A SCALAR POINT AND NO ELEMENTS ARE
!                  CONNECTED TO IT.
!     IF JARG = 1, THE PIVOT POINT IS A GRID POINT AND NO ELEMENTS ARE
!                  CONNECTED TO IT.
!
   !>>>>EQUIVALENCE (Iz(1),Dz(1)) , (iarray(1),iorder) , (iarray(2),nwds)
   DATA name/4HDETC , 4HK   / , neor/0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         dtol = tolel
         iarg = Jarg
         IF ( iarg<0 ) THEN
            iorder = 1
            nwds = 1
            iarray(3) = Npvt
            CALL write(Ifgpst,iarray(1),3,neor)
            RETURN
         ELSEIF ( iarg==0 ) THEN
         ENDIF
!
!     AT THIS POINT, BOTH TRANSLATIONAL AND ROTATIONAL DIAGONAL 3X3 S
!     ARE STORED IN THE D ARRAY.  HENCE WE PROCESS THEM.
!
         ip = Npvt - 1
         ASSIGN 60 TO igoto
         IF ( iarg/=1 ) THEN
            ASSIGN 20 TO back
            DO i = 1 , 9
               b(i) = d(i)
            ENDDO
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            ASSIGN 40 TO back
            GOTO 40
         ENDIF
 20      DO i = 1 , 9
            b(i) = d(i+9)
         ENDDO
!
!     INSURE THE SYMMETRY OF THE B MATRIX
!
         IF ( b(2)/=0.0D0 .AND. b(4)/=0.0D0 ) THEN
            temp = (b(2)+b(4))/2.0D0
            b(2) = temp
            b(4) = temp
         ELSE
            b(2) = 0.0D0
            b(4) = 0.0D0
         ENDIF
         IF ( b(3)/=0.0D0 .AND. b(7)/=0.0D0 ) THEN
            temp = (b(3)+b(7))/2.0D0
            b(3) = temp
            b(7) = temp
         ELSE
            b(3) = 0.0D0
            b(7) = 0.0D0
         ENDIF
         IF ( b(6)/=0.0D0 .AND. b(8)/=0.0D0 ) THEN
            temp = (b(6)+b(8))/2.0D0
         ELSE
            b(6) = 0.0D0
            b(8) = 0.0D0
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     SCALE THE MATRIX BY DIVIDING EACH ELEMENT OF B BY THE LARGEST
!     ELEMENT. IF THE LARGEST ELEMENT IS NON-POSITIVE, THE SINGULARITY
!     IS OF ORDER 3.
!
         temp = b(1)
         DO i = 2 , 9
            IF ( b(i)>temp ) temp = b(i)
         ENDDO
         IF ( temp<=0.0D0 ) GOTO 40
         DO i = 1 , 9
            b(i) = b(i)/temp
         ENDDO
!
!     FIND THE SQUARES OF THE MAGNITUDES OF THE VECTORS OF THE ROWS OF
!     THE B MATRIX.
!
         iorder = 0
         j = 0
         DO i = 1 , 9 , 3
            j = j + 1
            fl(j) = b(i)**2 + b(i+1)**2 + b(i+2)**2
            IF ( fl(j)==0.0D0 ) iorder = iorder + 1
         ENDDO
         IF ( iorder==2 ) THEN
!
!     AT THIS POINT 2 ROWS OF THE B MATRIX ARE IDENTICALLY ZERO.
!
            nwds = 2
            tnwds = 4
            ipoint = 2
            DO i = 1 , 3
               IF ( fl(i)==0.0D0 ) THEN
                  ipoint = ipoint + 1
                  iarray(ipoint) = ip + i
               ENDIF
            ENDDO
            spag_nextblock_1 = 5
         ELSE
            IF ( iorder==0 ) THEN
!
!     AT STATEMENT NO. 250, WE HAVE THAT ALL THE FL(I) ARE .GT. 0.0D0,
!     SO THAT THE DETERMINANT, DET, OF B MUST BE COMPUTED.
!
               det = b(1)*(b(5)*b(9)-b(6)*b(8)) - b(2)*(b(4)*b(9)-b(6)*b(7)) + b(3)*(b(4)*b(8)-b(5)*b(7))
               const = 0.05D0*dtol*fl(1)*fl(2)*fl(3)
               IF ( det>const ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!     COMPUTE M(I) AND R(I)
!
               m(1) = b(5)*b(9) - b(6)*b(8)
               m(2) = b(1)*b(9) - b(3)*b(7)
               m(3) = b(1)*b(5) - b(2)*b(4)
               r(1) = dsqrt(b(5)**2+b(6)**2)*dsqrt(b(8)**2+b(9)**2)
               r(2) = dsqrt(b(1)**2+b(3)**2)*dsqrt(b(7)**2+b(9)**2)
               r(3) = dsqrt(b(1)**2+b(2)**2)*dsqrt(b(4)**2+b(5)**2)
!
!     FIND I1, J1, K1
!     SUCH THAT M(I1)/R(I1) .GE. M(J1)/R(J1) .GE. M(K1)/R(K1)
!
               i1 = 1
               j1 = 2
               k1 = 3
               IF ( m(1)*r(2)<m(2)*r(1) ) THEN
                  i1 = 2
                  j1 = 1
               ENDIF
               IF ( m(i1)*r(k1)<m(k1)*r(i1) ) THEN
                  itemp = i1
                  i1 = k1
                  k1 = itemp
               ENDIF
               IF ( m(j1)*r(k1)<m(k1)*r(j1) ) THEN
                  itemp = j1
                  j1 = k1
                  k1 = itemp
               ENDIF
               IF ( m(i1)>=r(i1)*dtol ) THEN
!
!     AT THIS POINT THE SINGULARITY IS OF ORDER 1.
!
                  iorder = 1
                  nwds = 1
                  tnwds = 3
                  iarray(3) = ip + i1
                  IF ( m(j1)>=r(j1)*dtol ) THEN
                     nwds = 2
                     tnwds = 4
                     iarray(4) = ip + j1
                     IF ( m(k1)>=r(k1)*dtol ) THEN
                        nwds = 3
                        tnwds = 5
                        iarray(5) = ip + k1
                     ENDIF
                  ENDIF
               ELSE
!
!     HERE THE SINGULARITY IS OF ORDER 2.
!
                  nwds = 0
                  tnwds = 2
                  iorder = 2
!
!     FIND II, JJ, KK SUCH THAT B(II) .GE. B(JJ) .GE. B(KK)
!
                  ii = 1
                  jj = 5
                  kk = 9
                  IF ( b(1)<b(5) ) THEN
                     ii = 5
                     jj = 1
                  ENDIF
                  IF ( b(ii)<b(kk) ) THEN
                     itemp = ii
                     ii = kk
                     kk = itemp
                  ENDIF
                  IF ( b(jj)<b(kk) ) THEN
                     itemp = jj
                     jj = kk
                     kk = itemp
                  ENDIF
                  ll = ii
                  kount = 0
                  ipoint = 3
                  SPAG_Loop_1_1: DO WHILE ( b(ll)>0.0D0 )
                     nwds = nwds + 2
                     tnwds = tnwds + 2
                     IF ( ll<5 ) THEN
                        inc1 = 2
                        inc2 = 3
                     ELSEIF ( ll==5 ) THEN
                        inc1 = 1
                        inc2 = 3
                     ELSE
                        inc1 = 1
                        inc2 = 2
                     ENDIF
                     iarray(ipoint) = ip + inc1
                     iarray(ipoint+1) = ip + inc2
                     ipoint = ipoint + 2
                     kount = kount + 1
                     IF ( kount<2 ) THEN
                        ll = jj
                     ELSEIF ( kount==2 ) THEN
                        ll = kk
                     ELSE
                        EXIT SPAG_Loop_1_1
                     ENDIF
                  ENDDO SPAG_Loop_1_1
               ENDIF
               spag_nextblock_1 = 5
            ELSE
!
!     AT THIS POINT ONE AND ONLY ONE FL(I) IS ZERO.
!
               DO i = 1 , 3
                  isave = i
                  IF ( fl(i)==0.0D0 ) THEN
                     IF ( isave==1 ) GOTO 25
                     IF ( isave==2 ) GOTO 30
                     IF ( isave==3 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
               ENDDO
               CALL mesage(-30,26,name)
 25            fm = b(5)*b(9) - b(6)*b(8)
               fr = dsqrt((b(5)**2+b(6)**2)*(b(8)**2+b(9)**2))
               spag_nextblock_1 = 4
            ENDIF
            CYCLE
 30         fm = b(1)*b(9) - b(3)*b(7)
            fr = dsqrt((b(1)**2+b(3)**2)*(b(7)**2+b(9)**2))
            spag_nextblock_1 = 4
         ENDIF
      CASE (3)
         fm = b(1)*b(5) - b(2)*b(4)
         fr = dsqrt((b(1)**2+b(2)**2)*(b(4)**2+b(5)**2))
         spag_nextblock_1 = 4
      CASE (4)
         IF ( fm/=0.0D0 ) THEN
            IF ( fr<=0.0D0 .OR. fm/fr>=dtol ) THEN
!
!     AT THIS POINT WE HAVE THAT ONE AND ONLY ONE FL IS ZERO BUT THAT
!     ORDER OF THE SINGULARITY IS 1.
!
               iorder = 1
               nwds = 1
               tnwds = 3
               iarray(3) = ip + isave
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     HERE WE HAVE THAT THE ORDER OF THE SINGULARITY IS 2.
!
         iorder = 2
         nwds = 0
         tnwds = 2
         IF ( isave==2 ) THEN
            k1 = 1
            k2 = 9
            inc1 = 2
            inc2 = 3
            inc3 = 1
         ELSEIF ( isave==3 ) THEN
            k1 = 1
            k2 = 5
            inc1 = 3
            inc2 = 2
            inc3 = 1
         ELSE
            k1 = 5
            k2 = 9
            inc1 = 1
            inc2 = 3
            inc3 = 2
         ENDIF
         IF ( b(k1)>0.0D0 .OR. b(k2)>0.0D0 ) THEN
            IF ( b(k1)<=0.0D0 ) THEN
               ipoint = 3
            ELSE
               nwds = 2
               tnwds = 4
               iarray(3) = ip + inc1
               iarray(4) = ip + inc2
               ipoint = 5
            ENDIF
            IF ( b(k2)>0.0D0 ) THEN
               nwds = nwds + 2
               tnwds = tnwds + 2
               iarray(ipoint) = ip + inc1
               iarray(ipoint+1) = ip + inc3
            ENDIF
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     THE SINGULARITY IS OF ORDER 3
!
 40      iorder = 3
         nwds = 3
         tnwds = 5
         iarray(3) = ip + 1
         iarray(4) = ip + 2
         iarray(5) = ip + 3
         spag_nextblock_1 = 5
      CASE (5)
!
!     WRITE IARRAY ON THE GPST FILE.
!
         CALL write(Ifgpst,iarray(1),tnwds,neor)
         spag_nextblock_1 = 6
      CASE (6)
         GOTO igoto
 60      ASSIGN 99999 TO igoto
         ip = ip + 3
         GOTO back
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99999 END SUBROUTINE detck