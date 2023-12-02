!*==ampc.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampc(Djh1,Djh2,Djh,Ajjl,Qjh,Qjho,Qjhua,Scr1,Scr2,Scr3,Scr4,Scr5,Scr6)
   USE c_ampcom
   USE c_cdcmpx
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Djh1
   INTEGER :: Djh2
   INTEGER :: Djh
   INTEGER :: Ajjl
   INTEGER :: Qjh
   INTEGER :: Qjho
   INTEGER :: Qjhua
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Scr4
   INTEGER :: Scr5
   INTEGER :: Scr6
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(11) , SAVE :: block
   INTEGER :: file , i , ibuf1 , ibuf2 , idjha , ionce , iopt , ip1 , k , nclold , ncolth , ngps , nth , qjhth
   INTEGER , DIMENSION(11) , SAVE :: iblock
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL ampc1 , ampc2 , cfactr , cfbsor , close , cyct2b , fwdrec , gopen , korsz , mesage , rdtrl , ssg2b , ssg2c , tranp1 ,   &
          & wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         ibuf1 = korsz(iz) - sysbuf + 1
         ibuf2 = ibuf1 - sysbuf
         itc = mcbqhh(5)
         incr = 1
         itc1 = itc
         itc2 = itc1
         incr1 = incr
         ii1 = 1
!
!     IS QJH ON SAVE FILE
!
         IF ( qhhcol==0 ) THEN
!
!     COMPUTE QJH
!
!
!     HAS DJH ALREADY BEEN COMPUTED
!
            IF ( idjh==0 ) THEN
               block(9) = xk
               CALL ssg2c(Djh1,Djh2,Djh,1,block)
            ENDIF
!
!     POSITION AJJL
!
            CALL gopen(Ajjl,iz(ibuf1),0)
            k = ajjcol - 1
            IF ( k/=0 ) THEN
               file = Ajjl
               DO i = 1 , k
                  CALL fwdrec(*20,Ajjl)
               ENDDO
            ENDIF
            CALL close(Ajjl,2)
!
!     SET UP TO LOOP ON CONSTANT THEORY
!
            ngps = 1
            nth = ngpd(1,ngps)
            ncolth = 0
            nclold = ncolth + 1
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
!
!     COPY QJH FROM OLD FILE TO QJH
!
         ELSEIF ( mcbqjh(1)>0 ) THEN
            CALL gopen(Qjho,iz(ibuf1),0)
            CALL gopen(Qjh,iz(ibuf2),3)
            k = qhhcol - 1
            IF ( k/=0 ) THEN
               file = Qjho
               DO i = 1 , k
                  CALL fwdrec(*20,Qjho)
               ENDDO
            ENDIF
            CALL cyct2b(Qjho,Qjh,noh,iz,mcbqjh)
            CALL close(Qjho,1)
            CALL close(Qjh,3)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         RETURN
      CASE (3)
         SPAG_Loop_1_1: DO WHILE ( ngps<=ngp )
            IF ( ngpd(1,ngps)/=nth ) EXIT SPAG_Loop_1_1
            ncolth = ncolth + ngpd(2,ngps)
            ngps = ngps + 1
         ENDDO SPAG_Loop_1_1
!
!     BRANCH ON THEORY
!
         ionce = 0
         IF ( nclold==1 .AND. ngps>ngp ) ionce = 1
!
!     COPY AJJL TO SCR1
!
         CALL gopen(Ajjl,iz(ibuf1),2)
         CALL gopen(Scr1,iz(ibuf2),1)
         mcb(1) = Ajjl
         CALL rdtrl(mcb)
         mcb(1) = Scr1
         mcb(2) = 0
         mcb(3) = ncolth
         mcb(6) = 0
         mcb(7) = 0
         ii = nclold
         jj = ncolth
         ii1 = 1
         jj1 = ncolth - nclold + 1
         itc = mcb(5)
         itc1 = itc
         itc2 = itc
         incr = 1
         incr1 = 1
         CALL ampc1(Ajjl,Scr1,ncolth,iz,mcb)
         CALL close(Ajjl,2)
         CALL close(Scr1,1)
         CALL wrttrl(mcb)
         IF ( nth/=1 ) THEN
            IF ( nth==3 ) THEN
            ELSEIF ( nth==4 ) THEN
            ELSEIF ( nth==5 ) THEN
            ELSEIF ( nth==6 ) THEN
            ELSEIF ( nth/=7 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
!     DOUBLET LATTICE WITH SLENDER BODIES
!
!
!     TRANSPOSE MATRIX
!
         CALL tranp1(Scr1,Scr4,4,Scr2,Scr3,Scr5,Scr6,0,0,0,0)
!
!     DECOMPOSE MATRIX
!
         ib = 0
         CALL cfactr(Scr4,Scr2,Scr3,Scr1,Scr5,Scr6,iopt)
         spag_nextblock_1 = 5
      CASE (5)
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
         idjha = Djh
         IF ( ionce==0 ) THEN
            ii = nclold
            jj = ncolth
            ii1 = 1
            jj1 = ncolth - nclold + 1
            mcb(1) = Djh
            CALL rdtrl(mcb)
            itc = mcb(5)
            itc1 = itc
            itc2 = itc
            incr = 1
            incr1 = 1
            mcb(2) = 0
            mcb(3) = jj1
            mcb(6) = 0
            mcb(7) = 0
            mcb(1) = Scr4
            CALL gopen(Djh,iz(ibuf1),0)
            CALL gopen(Scr4,iz(ibuf2),1)
            CALL ampc1(Djh,Scr4,noh,iz,mcb)
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
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( nth==3 ) THEN
         ELSEIF ( nth==4 ) THEN
         ELSEIF ( nth==5 ) THEN
         ELSEIF ( nth==6 ) THEN
         ELSEIF ( nth/=7 ) THEN
            CALL cfbsor(Scr2,Scr3,idjha,qjhth,iopt)
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     COMPUTE THIS PORTION OF QJH  = AJJ*DJH
!
!
!     ALL GROUPS / THEORIES COMPLETE
!
         CALL ssg2b(Scr1,idjha,0,qjhth,0,iprec,1,Scr6)
         spag_nextblock_1 = 6
      CASE (6)
!
!     COPY ACCUMULATIVELY ONTO QJHUA
!
         IF ( ionce/=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL ampc2(Scr5,Qjhua,Scr1)
         IF ( ngps>ngp ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         nclold = ncolth + 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 20      DO
            ip1 = -2
!
!     ERROR MESSAGES
!
            CALL mesage(ip1,file,name)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ampc
