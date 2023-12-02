!*==ta1ca.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ta1ca(Koz)
   USE c_names
   USE c_ta1com
   USE c_tac1ax
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Koz
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(6) :: b , s
   REAL(REAL64) , DIMENSION(42) :: d
   REAL(REAL64) , DIMENSION(30) :: dd
   REAL(REAL64) :: det
   REAL(REAL64) , DIMENSION(25) :: dl , du
   REAL(REAL64) , DIMENSION(18) :: e , index
   INTEGER , SAVE :: eor , neor
   INTEGER :: file , i , ibeg , ibgpdt , icstm , ierow , ind , indxz , irank , irow , ising , j , jj , k , kk , l , left , lim ,    &
            & lima , lk , ll , nbgpdt , ncstm
   INTEGER , DIMENSION(6) :: icol
   INTEGER , DIMENSION(5) :: lrow
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(6) :: ssp
   REAL(REAL64) , DIMENSION(9) :: t
   REAL(REAL64) , DIMENSION(3) :: v
   REAL , DIMENSION(1) :: z
   EXTERNAL close , dmfgr , fwdrec , gmmatd , inverd , mesage , open , pretrd , read , transd , write
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE, CALLED BY SUBROUTINE TA1C, COMPUTES THE S MATRIX OF A
! GENERAL ELEMENT FROM INFORMATION IN THE CSTM AND BGPDT DATA BLOCKS.
! SEE FMMS-57 FOR EQUATIONS.
!*****
!
!
!
!
!
!
!
!
!
!
! OPEN CORE
!
!
!
!
!
!
!
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
!
!
!
   DATA eor , neor/1 , 0/
   DATA name(1)/4HTA1C/ , name(2)/4HA   /
!
! INITIALIZE
!
   ncstm = 0
   icstm = izzz
   left = bufr3 - icstm
!
! ATTEMPT TO OPEN THE CSTM
!
   file = cstm
   CALL open(*200,cstm,z(bufr3),inrw)
   CALL fwdrec(*500,cstm)
   CALL read(*500,*100,cstm,z(icstm+1),left,eor,ncstm)
   CALL mesage(-8,0,name(1))
 100  CALL close(cstm,clsrw)
!
! PRETRD SETS UP SUBSEQUENT CALLS TO TRANSD
!
   CALL pretrd(z(icstm+1),ncstm)
   left = left - ncstm
!
! READ THE BGPDT INTO CORE
!
 200  ibgpdt = icstm + ncstm
   file = bgpdt
   CALL open(*400,bgpdt,z(bufr3),inrw)
   CALL fwdrec(*500,bgpdt)
   CALL read(*500,*300,bgpdt,z(ibgpdt+1),left,eor,nbgpdt)
   CALL mesage(-8,0,name(1))
 300  CALL close(bgpdt,clsrw)
!
! ZERO OUT THE E MATRIX
!
   DO i = 1 , 18
      e(i) = 0.0D0
   ENDDO
   e(1) = 1.0D0
   e(8) = 1.0D0
   e(15) = 1.0D0
   ind = 0
   SPAG_Loop_1_1: DO
      ind = ind + 1
!*****
! IF IND = 1, THE D MATRIX IS FORMED IN THE DO 200 LOOP.
! IF IND = 2, THE S MATRIX IS FORMED AND OUTPUT A ROW AT A TIME IN THE
! DO LOOP.
!*****
      IF ( ind<2 ) THEN
!
!     IF STIFFNESS IS INPUT,CALCULATE LIM
!
         IF ( Koz==1 ) THEN
            lim = (nud-iud)/4 + 1
            lima = lim
            ibeg = iud
         ELSE
            lim = 6
            lima = 6
            ibeg = iud
         ENDIF
      ELSEIF ( ind==2 ) THEN
         lim = (nui-iui)/4 + 1
         ibeg = iui
         irow = 37
      ELSE
         EXIT SPAG_Loop_1_1
      ENDIF
      j = ibeg - 2
      i = 1
      SPAG_Loop_2_2: DO
         IF ( ind==1 ) irow = 6*i - 5
         j = j + 4
         jj = iz(j+1)
         k = ibgpdt + 4*(iz(j)-1)
!
! COMPUTE THE V VECTOR
!
         v(1) = 0.0D0
         v(2) = 0.0D0
         v(3) = 0.0D0
         kk = jj
         IF ( jj>3 ) kk = jj - 3
         IF ( iz(k+1)==0 ) THEN
            v(kk) = 1.0D0
         ELSE
            CALL transd(iz(k+1),t)
            IF ( kk==2 ) THEN
               v(1) = t(2)
               v(2) = t(5)
               v(3) = t(8)
            ELSEIF ( kk==3 ) THEN
               v(1) = t(3)
               v(2) = t(6)
               v(3) = t(9)
            ELSE
               v(1) = t(1)
               v(2) = t(4)
               v(3) = t(7)
            ENDIF
         ENDIF
!
! FORM THE E MATRIX IF THE DEGREE OF FREEDOM IS A TRANSLATION.
!
         IF ( jj>3 ) THEN
!
! THE DEGREE OF FREEDOM IS A ROTATION.
!
            ll = irow
            DO l = 1 , 6
               d(ll) = 0.0D0
               ll = ll + 1
            ENDDO
            IF ( iz(k+1)==0 ) THEN
               ll = irow + jj - 1
               d(ll) = 1.0D0
            ELSE
               d(irow+3) = v(1)
               d(irow+4) = v(2)
               d(irow+5) = v(3)
            ENDIF
         ELSE
            e(5) = z(k+4)
            e(6) = -z(k+3)
            e(10) = -z(k+4)
            e(12) = z(k+2)
            e(16) = z(k+3)
            e(17) = -z(k+2)
            IF ( iz(k+1)==0 ) THEN
               ierow = 6*jj - 5
               d(irow) = e(ierow)
               d(irow+1) = e(ierow+1)
               d(irow+2) = e(ierow+2)
               d(irow+3) = e(ierow+3)
               d(irow+4) = e(ierow+4)
               d(irow+5) = e(ierow+5)
            ELSE
               CALL gmmatd(v,3,1,1,e,3,6,0,d(irow))
            ENDIF
         ENDIF
!
! IF IND = 2 FORM A ROW OF THE S MATRIX AND WRITE IT OUT.
!
         IF ( ind/=1 ) THEN
!
!     IF STIFFNESS MATRIX INPUT AND LESS THAN 6 RIGID BODY DEGREES OF
!     FREEDOM, BRANCH
!
            IF ( Koz==1 .AND. lima<6 ) THEN
!
!     REARRANGE COLUMNS TO AGREE WITH ORDER OF DD AFTER MATRIX FACTOR-
!     IZATION
!
               DO l = 1 , 6
                  lk = icol(l)
                  b(l) = d(36+lk)
               ENDDO
!
!     MULTIPLY DI BY THE EXPANDED INVERSE OF DD
!
               CALL gmmatd(b(1),1,6,0,d(1),6,lima,0,s(1))
!
!     WRITE OUT THIS ROW OF THE S MATRIX
!
               DO l = 1 , lima
                  ssp(l) = s(l)
               ENDDO
               CALL write(gei,ssp,lima,neor)
            ELSE
               CALL gmmatd(d(37),6,1,1,d(1),6,6,0,s(1))
               DO l = 1 , 6
                  ssp(l) = s(l)
               ENDDO
               CALL write(gei,ssp,6,neor)
            ENDIF
         ENDIF
         i = i + 1
         IF ( i>lim ) THEN
            IF ( ind/=1 ) EXIT SPAG_Loop_2_2
!
!     IF STIFFNESS MATRIX WAS INPUT AND LESS THAN 6 RIGID BODY DEGREES
!     OF FREEDOM, BRANCH
!
            IF ( Koz==1 .AND. lim<6 ) THEN
!
!     SWITCH FROM ROW STORED TO COLUMN STORED
!
               DO i = 1 , lim
                  DO j = 1 , 6
                     indxz = i + (j-1)*lim
                     dd(indxz) = d(6*i+j-6)
                  ENDDO
               ENDDO
!
!     DETERMINE RANK OF DD AND EXPRESS MATRIX OF MAXIMAL RANK AS A
!     PRODUCT OF TRIANGULAR FACTORS
!
               CALL dmfgr(dd,lim,6,1.05E-05,irank,lrow,icol)
               IF ( irank==lim ) THEN
!
!     EXTRACT LOWER AND UPPER TRIANGULAR FACTORS, FORM PRODUCT,RESTORE
!     ROWS TO THEIR POSITION BEFORE FACTORIZATION AND INVERT. THEN
!     EXPAND MATRIX TO BE OF DIMENSION  6 BY IRANK
!
                  IF ( irank==1 ) THEN
                     d(1) = 1.0D0/dd(1)
                  ELSE
                     DO i = 1 , 25
                        dl(i) = 0.0D0
                        du(i) = 0.0D0
                     ENDDO
                     DO i = 1 , irank
                        DO j = 1 , irank
                           IF ( i<=j ) THEN
                              IF ( i==j ) THEN
                                 indxz = i + (j-1)*irank
                                 dl(indxz) = 1.0D0
                              ELSE
                                 indxz = i + (j-1)*irank
                                 dl(indxz) = dd(i*irank+j-irank)
                                 CYCLE
                              ENDIF
                           ENDIF
                           indxz = i + (j-1)*irank
                           du(indxz) = dd(i*irank+j-irank)
                        ENDDO
                     ENDDO
                     CALL gmmatd(dl(1),lim,lim,0,du(1),lim,lim,0,dd)
                     DO i = 1 , lim
                        k = lrow(i)
                        DO j = 1 , lim
                           indxz = j + (k-1)*lim
                           d(indxz) = dd(i*lim+j-lim)
                        ENDDO
                     ENDDO
!     AGAIN NO NEED TO COMPUTE DETERMINANT
                     ising = -1
                     CALL inverd(lim,d(1),lim,b(1),0,det,ising,index(1))
                     IF ( ising/=1 ) THEN
                        nogo = 1
                        CALL mesage(30,153,idgenl)
                        EXIT SPAG_Loop_2_2
                     ENDIF
                  ENDIF
                  k = lim*lim + 1
                  j = lim*6
                  DO i = k , j
                     d(i) = 0.0D0
                  ENDDO
                  CYCLE SPAG_Loop_1_1
               ELSE
                  nogo = 1
                  CALL mesage(30,152,idgenl)
                  EXIT SPAG_Loop_2_2
               ENDIF
            ELSE
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
               ising = -1
               CALL inverd(6,d(1),6,b(1),0,det,ising,index(1))
               IF ( ising==1 ) CYCLE SPAG_Loop_1_1
               nogo = 1
               CALL mesage(30,82,idgenl)
               EXIT SPAG_Loop_2_2
            ENDIF
         ENDIF
      ENDDO SPAG_Loop_2_2
      EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
   RETURN
!
!     ERROR MESSAGES
!
 400  CALL mesage(-1,file,name(1))
 500  CALL mesage(-1,file,name(1))
END SUBROUTINE ta1ca
