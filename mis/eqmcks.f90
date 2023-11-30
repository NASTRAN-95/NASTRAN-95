
SUBROUTINE eqmcks
   IMPLICIT NONE
   REAL Cor1(8,1) , Cor3(8,3) , Core(8,4) , Dum(52) , Freq , Skpb(15) , Zz(20000)
   INTEGER Igpt , Iopt , Iprec , Isbz , Iuninc , Iunpr , Iunrw , Kcstm , Keqin(8) , Klama , Kload , Kmpc , Knerw , Knrw , Koqm ,    &
         & Kpgg , Kqg , Krw , Kscc , Kscr(7) , Kspc , Ma(7) , Mb(7) , Mc(7) , Md(7) , Mpr , Msab , Msc , Mscr , Mt , Mz , Nout ,    &
         & Nskip , Nunrw , Parm(4) , Rdnrw , Rdrw , Wrtnrw , Wrtrw
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Iopt , Igpt , Nskip , Skpb , Core
   COMMON /eqmk1 / Kscc , Keqin , Kpgg , Kqg , Kcstm , Klama , Koqm , Kscr , Kmpc , Kload , Kspc , Parm
   COMMON /mpyadx/ Ma , Mb , Mc , Md , Mz , Mt , Msab , Msc , Mpr , Mscr
   COMMON /names / Rdnrw , Rdrw , Wrtnrw , Wrtrw , Krw , Knrw , Knerw
   COMMON /system/ Isbz , Nout , Dum , Iprec
   COMMON /unpakx/ Iunpr , Iunrw , Nunrw , Iuninc
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Zz
   INTEGER eject , korsz
   REAL head(2,4)
   INTEGER i , ibfl , ihdcnt , iret , ivec , j , k , l , maxvec , name(2) , nentry , nvec , nzz , nzz1 , nzz2 , nzz3
   LOGICAL lsteig
!
!     THIS SUBROUTINE CALCULATES AND OUTPUTS OVERALL EQUILIBRIUM FORCES
!
!     THE INPUT FILES ARE
!         KSCC - CASE CONTROL     - NOT PREPOSITIONED.
!         KPGG - LOAD VECTORS     - FILE 110 OR SCRATCH4
!         KQG  - SPC CONSTRAINTS  - FILE 111 OR SCRATCH5
!         QMG  - MPC CONSTRAINTS  - SCRATCH3
!         DT   - RIGID BODY TRANS - SCRATCH2
!
!WKBR 3/94 SPR93007      COMMON /SYSTEM/ ISBZ,NOUT
!ZZ   COMMON /ZZEQMS/ ZZ(1)
   EQUIVALENCE (Mb(6),Freq) , (Core(1,1),Cor1(1,1),Cor3(1,1))
   DATA name/4HEQMC , 4HKS  /
   DATA head/4HAPPL , 4HIED  , 4HSPCF , 4HORCE , 4HMPCF , 4HORCE , 4H---T , 4HOTAL/
!
   Parm(3) = name(1)
   Parm(4) = name(2)
   nzz = korsz(Zz)
   nzz3 = nzz - 3*Isbz + 1
   nzz2 = nzz3 + Isbz
   nzz1 = nzz2 + Isbz
!
   nvec = 0
   Ma(1) = Kscr(2)
   Mc(1) = 0
   CALL rdtrl(Ma)
   Mz = nzz
   Mt = 0
   Msab = 1
   Msc = 1
!WKBR 11/93 SPR93007      MPR   = 1
   Mpr = Iprec
   Mscr = Kscr(1)
!
!     CALCULATE  DT*PG  ON SCRATCH7
!
   IF ( Kload>0 ) THEN
      Mb(1) = Kpgg
      Md(1) = Kscr(7)
      CALL rdtrl(Mb)
      Md(3) = Ma(3)
      Md(4) = Mb(4)
!WKBR 11/93 SPR93007      MD(5) = 1
      Md(5) = Iprec
      CALL mpyad(Zz,Zz,Zz)
      IF ( Md(3)==Md(2) ) Md(4) = 1
      CALL wrttrl(Md)
      nvec = Md(2)
   ENDIF
!
!     CALCULATE DT*QG  ON SCRATCH6
!
   IF ( Kspc>0 ) THEN
      Mb(1) = Kqg
      Md(1) = Kscr(6)
      CALL rdtrl(Mb)
      Md(3) = Ma(3)
      Md(4) = Mb(4)
!WKBR 11/93 SPR93007      MD(5) = 1
      Md(5) = Iprec
      CALL mpyad(Zz,Zz,Zz)
      IF ( Md(3)==Md(2) ) Md(4) = 1
      CALL wrttrl(Md)
      nvec = max0(nvec,Md(2))
   ENDIF
!
!     CALCULATE  DT*MPC  ON SCRATCH5
!
   IF ( Kmpc>0 ) THEN
      Md(1) = Kscr(5)
      Mb(1) = Kscr(3)
      CALL rdtrl(Mb)
      Parm(2) = Mb(1)
      IF ( Mb(1)<=0 ) THEN
!
!     ILLEGAL INPUT
!
         Parm(1) = 1
!
         CALL mesage(Parm(1),Parm(2),Parm(3))
         GOTO 1000
      ELSE
         Md(3) = Ma(3)
         Md(4) = Mb(4)
!WKBR 11/93 SPR93007      MD(5) = 1
         Md(5) = Iprec
         CALL mpyad(Zz,Zz,Zz)
         IF ( Md(3)==Md(2) ) Md(4) = 1
         CALL wrttrl(Md)
         nvec = max0(Md(2),nvec)
      ENDIF
   ENDIF
   IF ( nvec<=0 ) GOTO 1000
!
!     POSITION CASE CONTROL
!
   CALL gopen(Kscc,Zz(nzz1),Rdrw)
   IF ( Nskip>0 ) THEN
      ibfl = nzz2
      IF ( Nskip>1 ) THEN
!
!     ASSUME USER MAY MALADJUST NSKIP
!
         j = Nskip - 1
         Parm(2) = Kscc
         DO i = 1 , j
            CALL fwdrec(*1200,Kscc)
         ENDDO
      ENDIF
   ELSE
!
!     RESERVE THIRD BUFFER FOR LAMA
!
      ibfl = nzz3
      Parm(2) = Klama
      CALL gopen(Klama,Zz(nzz3),Rdrw)
      CALL fwdrec(*1200,Klama)
   ENDIF
!
!     READ INTO CORE AS MANY (MAXVEC) VECTORS THAT FIT
!
   nentry = 0
   IF ( Kload>0 ) nentry = 6
   IF ( Kmpc>0 ) nentry = nentry + 6
   IF ( Kspc>0 ) nentry = nentry + 6
!
   maxvec = (ibfl-1)/nentry
   IF ( maxvec<nvec ) THEN
!
!     INSUFFICIENT CORE TO DO ALL VECTORS
!
      CALL page2(2)
      WRITE (Nout,99001) Uwm , maxvec , name
99001 FORMAT (A25,' 2374, INSUFFICIENT CORE TO PROCESS MORE THAN',I7,' VECTORS IN ',2A4)
!
      IF ( maxvec<=0 ) GOTO 1000
   ENDIF
!
   maxvec = min0(nvec,maxvec)
   l = 1
   Ma(1) = 0
   IF ( Kload<=0 ) GOTO 300
   Parm(2) = Kscr(7)
   Ma(1) = 1
   ASSIGN 300 TO iret
!
!     INTERNAL FUNCTION TO LOAD MAXVEC COLUMNS INTO CORE
!
 100  CALL gopen(Parm(2),Zz(nzz2),Rdrw)
   Iunpr = 1
   Iuninc = 1
   Iunrw = 1
   Nunrw = 6
!
   DO Mt = 1 , maxvec
      CALL unpack(*150,Parm(2),Zz(l))
      GOTO 200
 150  Mpr = l - 1
      DO i = 1 , 6
         Mpr = Mpr + 1
         Zz(Mpr) = 0.0
      ENDDO
 200  l = l + 6
   ENDDO
!
   CALL close(Parm(2),Krw)
   GOTO iret
!
 300  Ma(2) = 0
   IF ( Kspc>0 ) THEN
      Parm(2) = Kscr(6)
      Ma(2) = l
      ASSIGN 400 TO iret
      GOTO 100
   ENDIF
!
 400  Ma(3) = 0
   IF ( Kmpc>0 ) THEN
      Parm(2) = Kscr(5)
      Ma(3) = l
      ASSIGN 500 TO iret
      GOTO 100
   ENDIF
!
 500  ivec = 0
   lsteig = .FALSE.
   CALL page1
!
!     LOOP ON OUTPUT
!
 600  ivec = ivec + 1
   IF ( lsteig ) GOTO 800
   Parm(2) = Kscc
   CALL read(*700,*1100,Kscc,Mb(1),7,1,i)
   i = Mb(1)
   IF ( ivec==1 .OR. eject(11)/=0 ) WRITE (Nout,99002) Igpt
99002 FORMAT (1H0,20X,'E Q U I L I B R I U M   C H E C K   L O A D S',/,1H0,16X,'RESULTANT LOADS AT POINT',I7,                      &
             &' IN BASIC COORDINATE SYSTEM')
   IF ( Nskip<=0 ) GOTO 800
!
!     STATICS SUBCASES
!
   IF ( Mb(4)==0 ) Mb(4) = Mb(7)
   IF ( Mb(4)==0 ) Mb(4) = Mb(6)
   WRITE (Nout,99003) Mb(1) , Mb(4)
99003 FORMAT (1H0,24X,7HSUBCASE,I8,8H,   LOAD,I8)
   WRITE (Nout,99005)
!
   GOTO 900
!
!     EOF FOUND
!
 700  IF ( ivec>maxvec ) GOTO 1000
   IF ( Nskip>0 ) GOTO 1200
   lsteig = .TRUE.
!
!     EIGENVALUE PROBLEM
!
 800  Parm(2) = Klama
   CALL read(*1200,*1100,Klama,Mb(2),7,0,i)
   WRITE (Nout,99004) Mb(1) , Mb(2) , Freq
99004 FORMAT (1H0,24X,7HSUBCASE,I8,8H,   MODE,I5,13H,   FREQUENCY,1P,E15.6)
   WRITE (Nout,99005)
!
!     LOOP ON OUTPUT CATAGORY
!
 900  k = nentry/6 + 1
   ihdcnt = 1
   DO i = 3 , 8
      Core(i,k) = 0.0E0
   ENDDO
!
   DO i = 1 , 3
      IF ( Ma(i)/=0 ) THEN
         Core(1,ihdcnt) = head(1,i)
         Core(2,ihdcnt) = head(2,i)
         j = Ma(i) + ivec*6 - 6
!
         DO l = 3 , 8
            Core(l,ihdcnt) = Zz(j)
            Core(l,k) = Core(l,k) + Zz(j)
            j = j + 1
         ENDDO
         ihdcnt = ihdcnt + 1
      ENDIF
   ENDDO
!
   Core(1,k) = head(1,4)
   Core(2,k) = head(2,4)
   IF ( k==2 ) WRITE (Nout,99006) Cor1
   IF ( k==3 ) WRITE (Nout,99006) Cor3
   IF ( k==4 ) WRITE (Nout,99006) Core
   IF ( ivec<maxvec ) GOTO 600
 1000 CALL close(Kscc,Krw)
   IF ( Nskip<=0 ) CALL close(Klama,Krw)
   RETURN
!
!     ERROR MESSAGES
!
!     EOR
!
 1100 Parm(1) = 3
   CALL mesage(Parm(1),Parm(2),Parm(3))
   GOTO 1000
!
!     EOF
!
 1200 Parm(1) = 2
   CALL mesage(Parm(1),Parm(2),Parm(3))
   GOTO 1000
99005 FORMAT (1H0,5X,46H-TYPE-        T1             T2             T3,13X,32HR1             R2             R3)
!
99006 FORMAT (5X,2A4,1P,6E15.6)
END SUBROUTINE eqmcks
