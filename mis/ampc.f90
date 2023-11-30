
SUBROUTINE ampc(Djh1,Djh2,Djh,Ajjl,Qjh,Qjho,Qjhua,Scr1,Scr2,Scr3,Scr4,Scr5,Scr6)
   IMPLICIT NONE
   INTEGER Ajjcol , Ib , Idjh , Ii , Ii1 , Incr , Incr1 , Iprec , Itc , Itc1 , Itc2 , Iz(1) , Jj , Jj1 , Mcbqhh(7) , Mcbqjh(7) ,    &
         & Ncolj , Ngp , Ngpd(2,30) , Noh , Nout , Nsub , Qhhcol , Sysbuf
   REAL Dum32(32) , Skp(52) , Xk , Xm
   COMMON /ampcom/ Ncolj , Nsub , Xm , Xk , Ajjcol , Qhhcol , Ngp , Ngpd , Mcbqhh , Mcbqjh , Noh , Idjh
   COMMON /cdcmpx/ Dum32 , Ib
   COMMON /packx / Itc1 , Itc2 , Ii1 , Jj1 , Incr1
   COMMON /system/ Sysbuf , Nout , Skp , Iprec
   COMMON /unpakx/ Itc , Ii , Jj , Incr
   COMMON /zzzzzz/ Iz
   INTEGER Ajjl , Djh , Djh1 , Djh2 , Qjh , Qjho , Qjhua , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Scr6
   REAL block(11)
   INTEGER file , i , iblock(11) , ibuf1 , ibuf2 , idjha , ionce , iopt , ip1 , k , mcb(7) , name(2) , nclold , ncolth , ngps ,     &
         & nth , qjhth
   INTEGER korsz
!
!     THE PURPOSE OF THIS ROUTINE IS TO COMPUTE (OR RETRIEVE QJH)
!
!     IF QJH MUST BE COMPUTED
!
!     1. FORM DJH FOR THIS K (IF IDJH.EQ.0)
!        DJH = DJH1 + I*K*DJH2
!     2. FOR EACH CONSTANT THEORY
!        A. RETRIEVE AJJ PORTION = AJJTH
!        B. PERFORM THEORY FOR QJH
!           1) DOUBLET LATTICE
!              A) DECOMPOSE AJJTH
!              B) FIND PROPER DJH PORTION DJHTH
!              C) FBS FOR QJHTH
!              D) ADD TO BOTTOM OF QJHUA(CYCLE)
!           6) COMPRESSOR BLADES  (IONCE = 1).
!              A) COMPUTE QJHTH = (AJJ)*DJH.
!              B) QJHUA = QJHTH SINCE ONLY ONE BLADE AND GROUP (NGP = 1)
!           7) SWEPT TURBOPROPS   (IONCE = 1).
!              A) COMPUTE QJHTH = (AJJ)*DJH.
!              B) QJHUA = QJHTH SINCE ONLY ONE BLADE AND GROUP (NGP = 1)
!
   !>>>>EQUIVALENCE (iblock(1),block(1))
   DATA name/4HAMPC , 4H    /
   DATA iblock(1) , iblock(7) , block(2) , block(3) , block(8)/3 , 3 , 1.0 , 0. , 0./
!
!     INITIALIZE
!
   ibuf1 = korsz(Iz) - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   Itc = Mcbqhh(5)
   Incr = 1
   Itc1 = Itc
   Itc2 = Itc1
   Incr1 = Incr
   Ii1 = 1
!
!     IS QJH ON SAVE FILE
!
   IF ( Qhhcol==0 ) THEN
!
!     COMPUTE QJH
!
!
!     HAS DJH ALREADY BEEN COMPUTED
!
      IF ( Idjh==0 ) THEN
         block(9) = Xk
         CALL ssg2c(Djh1,Djh2,Djh,1,block)
      ENDIF
!
!     POSITION AJJL
!
      CALL gopen(Ajjl,Iz(ibuf1),0)
      k = Ajjcol - 1
      IF ( k/=0 ) THEN
         file = Ajjl
         DO i = 1 , k
            CALL fwdrec(*600,Ajjl)
         ENDDO
      ENDIF
      CALL close(Ajjl,2)
!
!     SET UP TO LOOP ON CONSTANT THEORY
!
      ngps = 1
      nth = Ngpd(1,ngps)
      ncolth = 0
      nclold = ncolth + 1
      GOTO 200
!
!     COPY QJH FROM OLD FILE TO QJH
!
   ELSEIF ( Mcbqjh(1)>0 ) THEN
      CALL gopen(Qjho,Iz(ibuf1),0)
      CALL gopen(Qjh,Iz(ibuf2),3)
      k = Qhhcol - 1
      IF ( k/=0 ) THEN
         file = Qjho
         DO i = 1 , k
            CALL fwdrec(*600,Qjho)
         ENDDO
      ENDIF
      CALL cyct2b(Qjho,Qjh,Noh,Iz,Mcbqjh)
      CALL close(Qjho,1)
      CALL close(Qjh,3)
   ENDIF
 100  RETURN
 200  DO WHILE ( ngps<=Ngp )
      IF ( Ngpd(1,ngps)/=nth ) EXIT
      ncolth = ncolth + Ngpd(2,ngps)
      ngps = ngps + 1
   ENDDO
!
!     BRANCH ON THEORY
!
   ionce = 0
   IF ( nclold==1 .AND. ngps>Ngp ) ionce = 1
!
!     COPY AJJL TO SCR1
!
   CALL gopen(Ajjl,Iz(ibuf1),2)
   CALL gopen(Scr1,Iz(ibuf2),1)
   mcb(1) = Ajjl
   CALL rdtrl(mcb)
   mcb(1) = Scr1
   mcb(2) = 0
   mcb(3) = ncolth
   mcb(6) = 0
   mcb(7) = 0
   Ii = nclold
   Jj = ncolth
   Ii1 = 1
   Jj1 = ncolth - nclold + 1
   Itc = mcb(5)
   Itc1 = Itc
   Itc2 = Itc
   Incr = 1
   Incr1 = 1
   CALL ampc1(Ajjl,Scr1,ncolth,Iz,mcb)
   CALL close(Ajjl,2)
   CALL close(Scr1,1)
   CALL wrttrl(mcb)
   IF ( nth==1 ) GOTO 300
   IF ( nth==3 ) THEN
   ELSEIF ( nth==4 ) THEN
   ELSEIF ( nth==5 ) THEN
   ELSEIF ( nth==6 ) THEN
   ELSEIF ( nth/=7 ) THEN
      GOTO 300
   ENDIF
   GOTO 400
!
!     DOUBLET LATTICE WITH SLENDER BODIES
!
!
!     TRANSPOSE MATRIX
!
 300  CALL tranp1(Scr1,Scr4,4,Scr2,Scr3,Scr5,Scr6,0,0,0,0)
!
!     DECOMPOSE MATRIX
!
   Ib = 0
   CALL cfactr(Scr4,Scr2,Scr3,Scr1,Scr5,Scr6,iopt)
!
!     MACH BOX
!     PISTON
!
!
!     COMPRESSOR BLADE AND SWEPT TURBOPROP THEORIES -
!     ONE BLADE ALLOWED, ONE GROUP, USE WHOLE AJJ AND DJH MATRICES.
!
!
!     COPY PROPER ROWS OF DJH TO SCR4
!
 400  idjha = Djh
   IF ( ionce==0 ) THEN
      Ii = nclold
      Jj = ncolth
      Ii1 = 1
      Jj1 = ncolth - nclold + 1
      mcb(1) = Djh
      CALL rdtrl(mcb)
      Itc = mcb(5)
      Itc1 = Itc
      Itc2 = Itc
      Incr = 1
      Incr1 = 1
      mcb(2) = 0
      mcb(3) = Jj1
      mcb(6) = 0
      mcb(7) = 0
      mcb(1) = Scr4
      CALL gopen(Djh,Iz(ibuf1),0)
      CALL gopen(Scr4,Iz(ibuf2),1)
      CALL ampc1(Djh,Scr4,Noh,Iz,mcb)
      CALL close(Djh,1)
      CALL close(Scr4,1)
      CALL wrttrl(mcb)
      idjha = Scr4
   ENDIF
   qjhth = Scr5
   IF ( ionce/=0 ) qjhth = Qjhua
   IF ( nth==1 ) THEN
!
!     SOLVE FOR THIS PORTION OF QJH
!
      CALL cfbsor(Scr2,Scr3,idjha,qjhth,iopt)
      GOTO 500
   ELSEIF ( nth==3 ) THEN
   ELSEIF ( nth==4 ) THEN
   ELSEIF ( nth==5 ) THEN
   ELSEIF ( nth==6 ) THEN
   ELSEIF ( nth/=7 ) THEN
      CALL cfbsor(Scr2,Scr3,idjha,qjhth,iopt)
      GOTO 500
   ENDIF
!
!     COMPUTE THIS PORTION OF QJH  = AJJ*DJH
!
!
!     ALL GROUPS / THEORIES COMPLETE
!
   CALL ssg2b(Scr1,idjha,0,qjhth,0,Iprec,1,Scr6)
!
!     COPY ACCUMULATIVELY ONTO QJHUA
!
 500  IF ( ionce/=0 ) GOTO 100
   CALL ampc2(Scr5,Qjhua,Scr1)
   IF ( ngps>Ngp ) GOTO 100
   nclold = ncolth + 1
   GOTO 200
 600  DO
      ip1 = -2
!
!     ERROR MESSAGES
!
      CALL mesage(ip1,file,name)
   ENDDO
END SUBROUTINE ampc