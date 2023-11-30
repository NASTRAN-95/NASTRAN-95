
SUBROUTINE amp
   IMPLICIT NONE
   INTEGER Ajjcol , Ib , Ibbar , Idjh , Igust , Isk(32) , Iz(1) , Mcbqhh(7) , Mcbqjh(7) , Mcbrjh(7) , Ncol , Ngp , Ngpd(2,30) ,     &
         & Noh , Noue , Nout , Nsub , Qhhcol , Sysbuf , Xqhhl
   REAL Xk , Xm
   COMMON /ampcom/ Ncol , Nsub , Xm , Xk , Ajjcol , Qhhcol , Ngp , Ngpd , Mcbqhh , Mcbqjh , Noh , Idjh , Mcbrjh
   COMMON /blank / Noue , Xqhhl , Igust
   COMMON /cdcmpx/ Isk , Ib , Ibbar
   COMMON /system/ Sysbuf , Nout
   COMMON /zzzzzz/ Iz
   INTEGER aero , ajjl , d1je , d1jk , d2je , d2jk , gtka , i , iany , ibuf1 , imax , iop , itf , itl , itmto , its , mcb(7) ,      &
         & name(2) , phidh , qhhl , qhjl , qjhl , scr1 , scr10 , scr11 , scr12 , scr13 , scr14 , scr2 , scr3 , scr4 , scr5 , scr6 , &
         & scr7 , scr8 , scr9 , skj , useta
   INTEGER korsz
   REAL xko
!
!     THIS IS THE DMAP DRIVER FOR AMP
!
!     DMAP CALLING SEQUENCE
!
!     AMP  AJJL,SKJ,D1JK,D2JK,GTKA,PHIDH,D1JE,D2JE,USETA,AERO/
!          QHHL,QJHL/V,N,NOVE/V,N,XQHHL  $
!
!     D1JE AND D2JE MAY BE PURGED
!
!     QHHL AND QJHL ARE APPEND TYPE FILES
!
!     QJHL MAY BE PURGED
!
!     DATA BLOCK ASSIGNMENTS                           COMPUTED BY   USE
!
!     SCR1   --OLD QHHL                                AMPA        A,D
!     SCR2   --OLD QJHL                                AMPA        A,C
!     SCR3   --INDEX OF WORK TO BE DONE                AMPA        A,MOD
!     SCR4   --DJH1                                    AMPB        B,C
!     SCR5   --DJH2                                    AMPB        B,C
!     SCR6   --GKI                                     AMPB        B,D
!     SCR7   --DJH                                     AMPC        C,C
!     SCR8   --QJHUA                                   AMPC        C,D
!     SCR9   --SCRATCH FILE                                       B,C,D
!     SCR10  --SCRATCH FILE                                       B,C,D
!     SCR11  --SCRATCH FILE                                       B,C,D
!     SCR12  --SCRATCH FILE                                       C
!     SCR13  --SCRATCH FILE                                       C
!     SCR14  --SCRATCH FILE                                       C
!
!     VARIABLES
!     NAME          MEANING
!     -------      ---------------
!     NCOL          NUMBER OF COLUMNS IN SUBMATRIX OF AJJL
!     NSUB          ACTUAL NUMBER OF SUBMATRICES ON AJJL
!     XM            CURRENT M
!     XK            CURRENT K
!     AJJCOL        COLUMN NUMBER IN AJJL WHERE CURRENT SUBMATRIX STARTS
!     QHHCOL        COLUMN NUMBER IN QHH AND QJH WHERE SUBMATRIX STARTS
!                        0 MEANS RECOMPUTE
!     NGP           NUMBER OF GROUPS IN AJJL
!     NGPD          PAIRS FOR EACH GROUP - 1--THEORY -1 =D.L.
!                                          2--NUMBER OF COLUM
!                                          2--NUMBER OF COLS IN GROUP
!     NOH           NUMBER OF H D.O.F.
!     IDJH          FLAG TO RECOMPUTE DJH IF K CHANGES
!     IMAX          NUMBER OF M-K PAIRS
!     IANY          FLAG TO INDICATE SOME CALCULATION MUST BE PERFORMED
!     ITL           MAXIMUM TIME FOR ANY LOOP
!     XKO           OLD VALUE OF K
!
!
   DATA ajjl , skj , d1jk , d2jk , gtka , phidh , d1je , d2je , useta , aero/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109 ,  &
      & 110/
   DATA qhhl , qjhl , name/201 , 202 , 4HAMP  , 1H /
   DATA qhjl/203/
   DATA scr1 , scr2 , scr3 , scr4 , scr5 , scr6 , scr7 , scr8 , scr9 , scr10 , scr11 , scr12 , scr13 , scr14/301 , 302 , 303 , 304 ,&
      & 305 , 306 , 307 , 308 , 309 , 310 , 311 , 312 , 313 , 314/
!
!     INITIALIZE
!
   ibuf1 = korsz(Iz) - Sysbuf + 1
   mcb(1) = phidh
   CALL rdtrl(mcb(1))
   Noh = mcb(2)
   Mcbrjh(1) = qhjl
   Ib = 0
   Ibbar = 0
!
!     BUILD INDEXES
!
   CALL ampa(aero,qjhl,qhhl,ajjl,scr1,scr2,scr3,imax,iany)
!
!     COMPUTE DJH AND GKI
!
!
!     IF NO NEW VALUES ARE TO BE COMPUTED SKIP AMPB
!
   IF ( iany==0 ) CALL ampb(phidh,gtka,d1jk,d2jk,d1je,d2je,useta,scr4,scr5,scr6,scr9,scr10,scr11)
!
!     LOOP ON MK PAIRS
!
   xko = -1.0
   iop = 0
   itl = 0
   DO i = 1 , imax
      CALL klock(its)
      CALL gopen(scr3,Iz(ibuf1),iop)
      iop = 2
      CALL fread(scr3,Xm,4,1)
      CALL close(scr3,2)
!
!     COMPUTE QJH
!
      Idjh = 0
      IF ( Xk==xko ) Idjh = 1
      CALL ampc(scr4,scr5,scr7,ajjl,qjhl,scr2,scr8,scr9,scr10,scr11,scr12,scr13,scr14)
      IF ( Qhhcol==0 ) xko = Xk
!
!     COMPUTE QHH
!
      IF ( Mcbqhh(1)>0 ) CALL ampd(scr8,scr1,skj,scr6,qhhl,scr9,scr10,scr11,scr12)
      IF ( i/=imax ) THEN
!
!     CHECK TIME
!
         CALL klock(itf)
         CALL tmtogo(itmto)
         itl = max0(itf-its,1,itl)
         IF ( 1.1*itl>=itmto ) GOTO 200
      ENDIF
   ENDDO
!
!     FINISH UP
!
 100  IF ( Mcbqhh(1)>0 ) CALL wrttrl(Mcbqhh)
   IF ( Mcbqjh(1)>0 ) CALL wrttrl(Mcbqjh)
   Xqhhl = -1
   IF ( Igust<=0 ) RETURN
!
!     COMPUTE QHJL
!          NOTE  QHJL IS REALLY QJHL
!
!     FIRST COMPUTE GKH ONTO SCR4
!
!
   CALL ampe(phidh,gtka,scr4,scr5,scr6,useta)
!
!     LOOP ON GROUPS WITHIN MK PAIRS FOR QHJL
!
   CALL ampf(skj,scr4,ajjl,qhjl,scr3,imax,scr5,scr6,scr7,scr8,scr9,scr10,scr11,scr12,scr13,scr1)
   RETURN
!
!     INSUFFICIENT TIME TO COMPLETE
!
 200  CALL mesage(45,imax-i,name)
   GOTO 100
END SUBROUTINE amp