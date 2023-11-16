
SUBROUTINE viscs
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum(12) , Ecpt(14) , Heat , Skp , Xx(1)
   INTEGER Elid , Estid , Icmbar , Idamp , Idm , Iecpt(14) , Ielid , Iheat , Imass , Ioutpt , Iprec , Istif , Ixtra , Jcore ,       &
         & Ksystm(53) , Lcstm , Ldict , Lhmat , Lmat , Ncore , Ngrids
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /emgdic/ Idm , Ldict , Ngrids , Elid , Estid
   COMMON /emgest/ Ecpt
   COMMON /emgprm/ Ixtra , Jcore , Ncore , Dum , Istif , Imass , Idamp , Iprec , Nogo , Heat , Icmbar , Lcstm , Lmat , Lhmat
   COMMON /system/ Skp , Ioutpt , Ksystm , Iheat
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Xx
!
! Local variable declarations
!
   REAL b(144) , c1 , c2 , d(64) , dict5 , fl , ta(9) , tb(9) , vec(3)
   INTEGER dict(7) , i , i1 , i2 , ia , iab , ib , iba , ifile , indx(4) , ip , ipa , ipb , ix , j , jtj , kb , kbb , kbx(4) , kx(4)
   LOGICAL idbug
!
! End of declarations
!
!
!     THIS SUBROUTINE COMPUTES THE 12X12 MATRIX BGG FOR A VISCOUS
!     (DASHPOT) ELEMENT
!
!     SINGLE PRECISION VERSION
!
!     THE ECPT ENTRIES FOR THE VISC ELEMENT ARE
!
!         ECPT
!     ECPT( 1)   ELEMENT ID
!     ECPT( 2)   SIL NUMBER FOR GRID POINT A
!     ECPT( 3)   SIL NUMBER FOR GRID POINT B
!     ECPT( 4)   EXTENSIONAL DAMPING CONSTANT  - C1
!     ECPT( 5)   TORSIONAL DAMPING COEFFICIENT - C2
!     ECPT( 6)   COORD. SYSTEM ID FOR POINT A
!     ECPT( 7)   X1
!     ECPT( 8)   Y1
!     ECPT( 9)   Z1
!     ECPT(10)   COORD. SYSTEM ID FOR POINT B
!     ECPT(11)   X2
!     ECPT(12)   Y2
!     ECPT(13)   Z2
!     ECPT(14)   ELEMENT TEMPERATURE (NOT USED)
!
!
   EQUIVALENCE (Ecpt(1),Iecpt(1),Ielid) , (dict(5),dict5) , (indx(1),ia) , (indx(2),iab) , (indx(3),iba) , (indx(4),ib)
   DATA kx/1 , 7 , 73 , 79/
   DATA kbx/40 , 46 , 112 , 118/
!
!     INITIALIZE EMGOUT PARAMETERS
!
   idbug = .TRUE.
   Ngrids = 2
   Ldict = 5 + Ngrids
   dict(1) = Estid
   dict(2) = 1
   dict(3) = 12
   dict(4) = 63
   dict5 = 0.
   ifile = 3
   ip = Iprec
!
!     NOW COMPUTE THE LENGTH OF THE ROD AND NORMALIZE
!
   fl = 0.
   DO i = 1 , 3
      vec(i) = Ecpt(i+6) - Ecpt(i+10)
      fl = fl + vec(i)**2
   ENDDO
   fl = sqrt(fl)
!
   IF ( fl<=0 ) THEN
!
!     ERROR EXITS
!
      WRITE (Ioutpt,99001) Ufm , Ielid
99001 FORMAT (A23,' 31XX, ILLEGAL GEOMETRY OR CONNECTIONS FOR VISC ','ELEMENT',I10)
      Nogo = .TRUE.
      GOTO 99999
   ELSE
      DO i = 1 , 3
         vec(i) = vec(i)/fl
      ENDDO
!
!     SET UP THE N MATRIX
!
      DO i = 1 , 3
         DO j = 1 , 3
            ix = (i-1)*3 + j
            d(ix) = vec(i)*vec(j)
         ENDDO
      ENDDO
!
!     INITIALIZE THE B MATRIX
!
      DO i = 1 , 144
         b(i) = 0.
      ENDDO
!
!     SWAP INDICES A AND B IF NECESSARY SO MATRIX WILL BE ORDERED
!     BY INCREASING SIL VALUE
!
      ipa = 6
      ipb = 10
      IF ( Iecpt(2)>=Iecpt(3) ) THEN
         ix = ipa
         ipa = ipb
         ipb = ipa
      ENDIF
!
!     CONVERT GRID POINTS TO BASIC COORDINATES IF NECESSARY
!
      ia = 1
      iab = 1
      IF ( Iecpt(ipa)/=0 ) THEN
         ia = 19
         iab = 10
         CALL transs(Ecpt(ipa),ta(1))
         CALL gmmats(ta(1),3,3,1,d(1),3,3,0,d(10))
         CALL gmmats(d(10),3,3,0,ta(1),3,3,0,d(19))
      ENDIF
!
      ib = 1
      iba = 1
      IF ( Iecpt(ipb)/=0 ) THEN
         ib = 28
         iba = 37
         CALL transs(Ecpt(ipb),tb(1))
         CALL gmmats(tb(1),3,3,1,d(1),3,3,0,d(37))
         CALL gmmats(d(37),3,3,0,tb(1),3,3,0,d(28))
!
         CALL gmmats(d(iab),3,3,0,tb(1),3,3,0,d(46))
         iab = 46
      ENDIF
!
      IF ( Iecpt(ipa)/=0 ) THEN
         CALL gmmats(d(iba),3,3,0,ta(1),3,3,0,d(55))
         iba = 55
      ENDIF
   ENDIF
!
!     CALCULATE THE DAMPING MATRIX B
!
!                       ****                    ****
!                       *      /     /      /      *
!                       * C D  /   0 /-C D  /  0   *
!                       *  1 AA/     /  1 AB/      *
!                       *--------------------------*
!                       *  0   /C D  /   0  /-C D  *
!                       *      / 2 AA/      /  2 AB*
!         B    =        *--------------------------*
!                       *-C D  /   0 / C D  /  0   *
!                       *  1 BA/     /  1 BB/      *
!                       *------------/-------------*
!                       *  0   /-C D /   0  / C D  *
!                       *      /  2 BA      /  2 BB*
!                       *      /     /      /      *
!                       ****                    ****
!
   c1 = Ecpt(4)
   c2 = Ecpt(5)
!
   DO jtj = 1 , 4
      kb = kx(jtj)
      kbb = kbx(jtj)
      j = 0
      i1 = indx(jtj)
      i2 = i1 + 8
      IF ( mod(jtj,2)==0 ) THEN
         c1 = -c1
         c2 = -c2
      ENDIF
!
      DO i = i1 , i2
         b(kb) = c1*d(i)
         b(kbb) = c2*d(i)
         IF ( mod(i,3)==0 ) j = 9
         kb = kb + 1 + j
         kbb = kbb + 1 + j
         j = 0
      ENDDO
!
   ENDDO
!
!     OUTPUT THE MATRIX
!
   CALL emgout(b,b,144,1,dict,ifile,ip)
   RETURN
99999 RETURN
END SUBROUTINE viscs
