
SUBROUTINE ta1ca(Koz)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bgpdt , Bufr1 , Bufr2 , Bufr3 , Clsrw , Cstm , Gei , Idgenl , Inrw , Iud , Iui , Iz(1) , Izzz , Nogo , Nud , Nui
   REAL Dum2(2) , Dum22(2) , Dum3(3) , Dummy1 , Dummy2 , Outrw , Z(1)
   COMMON /names / Dummy1 , Inrw , Dummy2 , Outrw , Clsrw
   COMMON /ta1com/ Dum3 , Bgpdt , Dum2 , Cstm , Dum22 , Gei
   COMMON /tac1ax/ Bufr1 , Bufr2 , Bufr3 , Iui , Nui , Iud , Nud , Izzz , Nogo , Idgenl
   COMMON /zzzzzz/ Iz
!
! Dummy argument declarations
!
   INTEGER Koz
!
! Local variable declarations
!
   DOUBLE PRECISION b(6) , d(42) , dd(30) , det , dl(25) , du(25) , e(18) , index(18) , s(6) , t(9) , v(3)
   INTEGER eor , file , i , ibeg , ibgpdt , icol(6) , icstm , ierow , ind , indxz , irank , irow , ising , j , jj , k , kk , l ,    &
         & left , lim , lima , lk , ll , lrow(5) , name(2) , nbgpdt , ncstm , neor
   REAL ssp(6)
!
! End of declarations
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
   EQUIVALENCE (Z(1),Iz(1))
!
!
!
   DATA eor , neor/1 , 0/
   DATA name(1)/4HTA1C/ , name(2)/4HA   /
!
! INITIALIZE
!
   ncstm = 0
   icstm = Izzz
   left = Bufr3 - icstm
!
! ATTEMPT TO OPEN THE CSTM
!
   file = Cstm
   CALL open(*200,Cstm,Z(Bufr3),Inrw)
   CALL fwdrec(*700,Cstm)
   CALL read(*700,*100,Cstm,Z(icstm+1),left,eor,ncstm)
   CALL mesage(-8,0,name(1))
 100  CALL close(Cstm,Clsrw)
!
! PRETRD SETS UP SUBSEQUENT CALLS TO TRANSD
!
   CALL pretrd(Z(icstm+1),ncstm)
   left = left - ncstm
!
! READ THE BGPDT INTO CORE
!
 200  ibgpdt = icstm + ncstm
   file = Bgpdt
   CALL open(*600,Bgpdt,Z(Bufr3),Inrw)
   CALL fwdrec(*700,Bgpdt)
   CALL read(*700,*300,Bgpdt,Z(ibgpdt+1),left,eor,nbgpdt)
   CALL mesage(-8,0,name(1))
 300  CALL close(Bgpdt,Clsrw)
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
 400  ind = ind + 1
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
         lim = (Nud-Iud)/4 + 1
         lima = lim
         ibeg = Iud
      ELSE
         lim = 6
         lima = 6
         ibeg = Iud
      ENDIF
   ELSEIF ( ind==2 ) THEN
      lim = (Nui-Iui)/4 + 1
      ibeg = Iui
      irow = 37
   ELSE
      GOTO 500
   ENDIF
   j = ibeg - 2
   i = 1
   DO
      IF ( ind==1 ) irow = 6*i - 5
      j = j + 4
      jj = Iz(j+1)
      k = ibgpdt + 4*(Iz(j)-1)
!
! COMPUTE THE V VECTOR
!
      v(1) = 0.0D0
      v(2) = 0.0D0
      v(3) = 0.0D0
      kk = jj
      IF ( jj>3 ) kk = jj - 3
      IF ( Iz(k+1)==0 ) THEN
         v(kk) = 1.0D0
      ELSE
         CALL transd(Iz(k+1),t)
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
         IF ( Iz(k+1)==0 ) THEN
            ll = irow + jj - 1
            d(ll) = 1.0D0
         ELSE
            d(irow+3) = v(1)
            d(irow+4) = v(2)
            d(irow+5) = v(3)
         ENDIF
      ELSE
         e(5) = Z(k+4)
         e(6) = -Z(k+3)
         e(10) = -Z(k+4)
         e(12) = Z(k+2)
         e(16) = Z(k+3)
         e(17) = -Z(k+2)
         IF ( Iz(k+1)==0 ) THEN
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
            CALL write(Gei,ssp,lima,neor)
         ELSE
            CALL gmmatd(d(37),6,1,1,d(1),6,6,0,s(1))
            DO l = 1 , 6
               ssp(l) = s(l)
            ENDDO
            CALL write(Gei,ssp,6,neor)
         ENDIF
      ENDIF
      i = i + 1
      IF ( i>lim ) THEN
         IF ( ind/=1 ) EXIT
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
                     Nogo = 1
                     CALL mesage(30,153,Idgenl)
                     EXIT
                  ENDIF
               ENDIF
               k = lim*lim + 1
               j = lim*6
               DO i = k , j
                  d(i) = 0.0D0
               ENDDO
               GOTO 400
            ELSE
               Nogo = 1
               CALL mesage(30,152,Idgenl)
               EXIT
            ENDIF
         ELSE
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
            ising = -1
            CALL inverd(6,d(1),6,b(1),0,det,ising,index(1))
            IF ( ising==1 ) GOTO 400
            Nogo = 1
            CALL mesage(30,82,Idgenl)
            EXIT
         ENDIF
      ENDIF
   ENDDO
 500  RETURN
!
!     ERROR MESSAGES
!
 600  CALL mesage(-1,file,name(1))
 700  CALL mesage(-1,file,name(1))
END SUBROUTINE ta1ca
