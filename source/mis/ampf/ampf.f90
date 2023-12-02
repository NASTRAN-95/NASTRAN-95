!*==ampf.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ampf(Skj,Gkh,Ajjl,Qhjl,Plan,Imax,Scr1,Scr2,Scr3,Scr4,Scr5,Scr6,Scr7,Scr8,Scr9,Scr10)
   IMPLICIT NONE
   USE C_AMPCOM
   USE C_CDCMPX
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Skj
   INTEGER :: Gkh
   INTEGER :: Ajjl
   INTEGER :: Qhjl
   INTEGER :: Plan
   INTEGER :: Imax
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Scr4
   INTEGER :: Scr5
   INTEGER :: Scr6
   INTEGER :: Scr7
   INTEGER :: Scr8
   INTEGER :: Scr9
   INTEGER :: Scr10
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ibuf1 , ibuf2 , iloop , ionce , iop , iopt , itf , itl , itmto , its , k , nclold , ncolth , ngps , nth , rjh
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL ampc1 , ampc2 , cfactr , cfbsor , close , cyct2b , fread , gopen , klock , korsz , makmcb , mesage , rdtrl , skprec ,   &
          & ssg2b , tmtogo , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     THE PURPOSE OF THIS ROUTINE IS TO SOLVE FOR QHJL
!
!     THE STEPS ARE AS FOLLOWS
!
!       I.  FOR EACH M-K PAIR
!
!           A. FIND SKJ FROM SKJ LIST
!                                      T
!           B.  COMPUTE  S(K) =  SKJ(K) *GKH
!
!           C.  FOR EACH GROUP
!                                                  G
!               1. BREAK  S(K) INTO GROUPS  =  S(K)
!
!               2. SOLVE FOR  RJH
!                                                        -1     G
!                      D.L. AND D.L. WITH BODIES RGH= AJJ  *S(K)
!                                                        T      G
!                      OTHERS                    RGH= AJJ  *S(K)
!
!               3. MERGE RESULTS
!
!                  1    G11
!                  1 RJH  1
!                  1------1  =   RJH(K)
!                  1    G21
!                  1 RJH  1
!                  1      1
!
!
!           D.  APPEND  RJH ONTO GROWING  QHJL
!                1       1       1
!                1RJH(K1)1RJH(K2)1  =  QHJL
!                1       1       1
!                1       1       1
!
   DATA name/4HAMPF , 1H /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE
!
         ibuf1 = korsz(Z) - Sysbuf + 1
         ibuf2 = ibuf1 - Sysbuf
         iop = 0
         itl = 0
         DO iloop = 1 , Imax
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL klock(its)
                  CALL gopen(Plan,Z(ibuf1),iop)
                  iop = 2
                  CALL fread(Plan,Xm,4,1)
                  CALL close(Plan,2)
!
!     FIND  SKJ(K) IN SKJL
!
                  CALL gopen(Skj,Z(ibuf1),0)
                  CALL gopen(Scr1,Z(ibuf2),1)
                  k = Ajjcol - 1
                  CALL skprec(Skj,k)
                  mcb(1) = Skj
                  CALL rdtrl(mcb)
                  CALL makmcb(mcb,Scr1,mcb(3),mcb(4),mcb(5))
                  Incr = 1
                  Itc = mcb(5)
                  CALL cyct2b(Skj,Scr1,Ncolj,Z,mcb)
                  CALL close(Skj,1)
                  CALL close(Scr1,1)
                  CALL wrttrl(mcb)
!                     T
!     MULTIPLY  SKJ(K) *GKH  ONTO SCR2
!
                  CALL ssg2b(Scr1,Gkh,0,Scr2,1,Iprec,1,Scr3)
!
!     POSITION AJJL
!
                  CALL gopen(Ajjl,Z(ibuf1),0)
                  k = Ajjcol - 1
                  CALL skprec(Ajjl,k)
                  CALL close(Ajjl,2)
!
!     SET UP TO LOOP ON CONSTANT THEORY
!
                  ngps = 1
                  nth = Ngpd(1,ngps)
                  ncolth = 0
                  spag_nextblock_2 = 2
               CASE (2)
                  nclold = ncolth + 1
                  SPAG_Loop_2_1: DO WHILE ( ngps<=Ngp )
                     IF ( Ngpd(1,ngps)/=nth ) EXIT SPAG_Loop_2_1
                     ncolth = ncolth + Ngpd(2,ngps)
                     ngps = ngps + 1
                  ENDDO SPAG_Loop_2_1
                  ionce = 0
                  IF ( nclold==1 .AND. ngps>Ngp ) ionce = 1
!                                 G
!     COPY AJJL(K) TO SCR1 (AJJ(K) )
!
                  CALL gopen(Ajjl,Z(ibuf1),2)
                  CALL gopen(Scr1,Z(ibuf2),1)
                  mcb(1) = Ajjl
                  CALL rdtrl(mcb)
                  CALL makmcb(mcb,Scr1,ncolth,mcb(4),mcb(5))
                  Ii = nclold
                  Jj = ncolth
                  Ii1 = 1
                  Jj1 = ncolth - nclold + 1
                  Itc = mcb(5)
                  Itc1 = Itc
                  Itc2 = Itc
                  Incr = 1
                  Incr1 = 1
                  CALL ampc1(Ajjl,Scr1,ncolth,Z,mcb)
                  CALL close(Ajjl,2)
                  CALL close(Scr1,1)
                  CALL wrttrl(mcb)
!                                   G
!     COPY SKJ(K)  ONTO SCR3 (SKJ(K) )
!
                  CALL gopen(Scr2,Z(ibuf1),0)
                  CALL gopen(Scr3,Z(ibuf2),1)
                  mcb(1) = Scr2
                  CALL rdtrl(mcb)
                  CALL makmcb(mcb,Scr3,ncolth,mcb(4),mcb(5))
                  Itc = mcb(5)
                  Itc1 = Itc
                  Itc2 = Itc
                  CALL ampc1(Scr2,Scr3,Noh,Z,mcb)
                  CALL close(Scr2,1)
                  CALL close(Scr3,1)
                  CALL wrttrl(mcb)
                  rjh = Scr10
                  IF ( ionce/=0 ) rjh = Scr9
!
!     BRANCH ON THEORY
!
                  IF ( nth/=1 ) THEN
                     IF ( nth==3 ) THEN
                     ELSEIF ( nth==4 ) THEN
                     ELSEIF ( nth/=5 ) THEN
                        spag_nextblock_2 = 3
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
!
!     OTHER THEORIES
!
                     CALL ssg2b(Scr1,Scr3,0,rjh,1,Iprec,1,Scr4)
                     spag_nextblock_2 = 4
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 3
               CASE (3)
!
!     DOUBLET LATTICE--D.L. WITH SLENDER BODIES
!
!                     G
!     DECOMPOSE AJJ(K)
!
                  Ib = 0
                  CALL cfactr(Scr1,Scr4,Scr5,Scr6,Scr7,Scr8,iopt)
                  CALL cfbsor(Scr4,Scr5,Scr3,rjh,iopt)
                  spag_nextblock_2 = 4
               CASE (4)
!
!     COPY ACCUMULATIVELY ONTO RJH(K)
!
                  IF ( ionce==0 ) THEN
                     CALL ampc2(rjh,Scr9,Scr1)
                     IF ( ngps<=Ngp ) THEN
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDIF
!
!     ALL GROUPS /THEORIES COMPLETE
!
!
!     COPY ONTO  QHJL
!
                  CALL gopen(Scr9,Z(ibuf1),0)
                  CALL gopen(Qhjl,Z(ibuf2),3)
                  mcb(1) = Qhjl
                  CALL rdtrl(mcb(1))
                  Itc = mcb(5)
                  Incr = 1
                  CALL cyct2b(Scr9,Qhjl,Noh,Z,mcb)
                  CALL close(Qhjl,2)
                  CALL close(Scr9,1)
                  CALL wrttrl(mcb)
!
!     END LOOP ON M-K PAIRS
!
                  IF ( iloop/=Imax ) THEN
!
!     CHECK TIME
!
                     CALL klock(itf)
                     CALL tmtogo(itmto)
                     itl = max0(itf-its,1,itl)
                     IF ( 1.1*itl>=itmto ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         RETURN
      CASE (2)
!
!     INSUFFICIENT TIME TO COMPLETE
!
         CALL mesage(45,Imax-iloop,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ampf
