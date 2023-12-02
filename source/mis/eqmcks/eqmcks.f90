!*==eqmcks.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE eqmcks
   IMPLICIT NONE
   USE c_blank
   USE c_eqmk1
   USE c_mpyadx
   USE c_names
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(8,1) :: cor1
   REAL , DIMENSION(8,3) :: cor3
   REAL :: freq
   REAL , DIMENSION(2,4) , SAVE :: head
   INTEGER :: i , ibfl , ihdcnt , iret , ivec , j , k , l , maxvec , nentry , nvec , nzz , nzz1 , nzz2 , nzz3
   LOGICAL :: lsteig
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
   !>>>>EQUIVALENCE (Mb(6),Freq) , (Core(1,1),Cor1(1,1),Cor3(1,1))
   DATA name/4HEQMC , 4HKS  /
   DATA head/4HAPPL , 4HIED  , 4HSPCF , 4HORCE , 4HMPCF , 4HORCE , 4H---T , 4HOTAL/
!
   parm(3) = name(1)
   parm(4) = name(2)
   nzz = korsz(zz)
   nzz3 = nzz - 3*isbz + 1
   nzz2 = nzz3 + isbz
   nzz1 = nzz2 + isbz
!
   nvec = 0
   ma(1) = kscr(2)
   mc(1) = 0
   CALL rdtrl(ma)
   mz = nzz
   mt = 0
   msab = 1
   msc = 1
!WKBR 11/93 SPR93007      MPR   = 1
   mpr = iprec
   mscr = kscr(1)
!
!     CALCULATE  DT*PG  ON SCRATCH7
!
   IF ( kload>0 ) THEN
      mb(1) = kpgg
      md(1) = kscr(7)
      CALL rdtrl(mb)
      md(3) = ma(3)
      md(4) = mb(4)
!WKBR 11/93 SPR93007      MD(5) = 1
      md(5) = iprec
      CALL mpyad(zz,zz,zz)
      IF ( md(3)==md(2) ) md(4) = 1
      CALL wrttrl(md)
      nvec = md(2)
   ENDIF
!
!     CALCULATE DT*QG  ON SCRATCH6
!
   IF ( kspc>0 ) THEN
      mb(1) = kqg
      md(1) = kscr(6)
      CALL rdtrl(mb)
      md(3) = ma(3)
      md(4) = mb(4)
!WKBR 11/93 SPR93007      MD(5) = 1
      md(5) = iprec
      CALL mpyad(zz,zz,zz)
      IF ( md(3)==md(2) ) md(4) = 1
      CALL wrttrl(md)
      nvec = max0(nvec,md(2))
   ENDIF
!
!     CALCULATE  DT*MPC  ON SCRATCH5
!
   IF ( kmpc>0 ) THEN
      md(1) = kscr(5)
      mb(1) = kscr(3)
      CALL rdtrl(mb)
      parm(2) = mb(1)
      IF ( mb(1)<=0 ) THEN
!
!     ILLEGAL INPUT
!
         parm(1) = 1
!
         CALL mesage(parm(1),parm(2),parm(3))
         GOTO 1000
      ELSE
         md(3) = ma(3)
         md(4) = mb(4)
!WKBR 11/93 SPR93007      MD(5) = 1
         md(5) = iprec
         CALL mpyad(zz,zz,zz)
         IF ( md(3)==md(2) ) md(4) = 1
         CALL wrttrl(md)
         nvec = max0(md(2),nvec)
      ENDIF
   ENDIF
   IF ( nvec<=0 ) GOTO 1000
!
!     POSITION CASE CONTROL
!
   CALL gopen(kscc,zz(nzz1),rdrw)
   IF ( nskip>0 ) THEN
      ibfl = nzz2
      IF ( nskip>1 ) THEN
!
!     ASSUME USER MAY MALADJUST NSKIP
!
         j = nskip - 1
         parm(2) = kscc
         DO i = 1 , j
            CALL fwdrec(*1200,kscc)
         ENDDO
      ENDIF
   ELSE
!
!     RESERVE THIRD BUFFER FOR LAMA
!
      ibfl = nzz3
      parm(2) = klama
      CALL gopen(klama,zz(nzz3),rdrw)
      CALL fwdrec(*1200,klama)
   ENDIF
!
!     READ INTO CORE AS MANY (MAXVEC) VECTORS THAT FIT
!
   nentry = 0
   IF ( kload>0 ) nentry = 6
   IF ( kmpc>0 ) nentry = nentry + 6
   IF ( kspc>0 ) nentry = nentry + 6
!
   maxvec = (ibfl-1)/nentry
   IF ( maxvec<nvec ) THEN
!
!     INSUFFICIENT CORE TO DO ALL VECTORS
!
      CALL page2(2)
      WRITE (nout,99001) uwm , maxvec , name
99001 FORMAT (A25,' 2374, INSUFFICIENT CORE TO PROCESS MORE THAN',I7,' VECTORS IN ',2A4)
!
      IF ( maxvec<=0 ) GOTO 1000
   ENDIF
!
   maxvec = min0(nvec,maxvec)
   l = 1
   ma(1) = 0
   IF ( kload<=0 ) GOTO 300
   parm(2) = kscr(7)
   ma(1) = 1
   ASSIGN 300 TO iret
!
!     INTERNAL FUNCTION TO LOAD MAXVEC COLUMNS INTO CORE
!
 100  CALL gopen(parm(2),zz(nzz2),rdrw)
   iunpr = 1
   iuninc = 1
   iunrw = 1
   nunrw = 6
!
   DO mt = 1 , maxvec
      CALL unpack(*150,parm(2),zz(l))
      GOTO 200
 150  mpr = l - 1
      DO i = 1 , 6
         mpr = mpr + 1
         zz(mpr) = 0.0
      ENDDO
 200  l = l + 6
   ENDDO
!
   CALL close(parm(2),krw)
   GOTO iret
!
 300  ma(2) = 0
   IF ( kspc>0 ) THEN
      parm(2) = kscr(6)
      ma(2) = l
      ASSIGN 400 TO iret
      GOTO 100
   ENDIF
!
 400  ma(3) = 0
   IF ( kmpc>0 ) THEN
      parm(2) = kscr(5)
      ma(3) = l
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
   parm(2) = kscc
   CALL read(*700,*1100,kscc,mb(1),7,1,i)
   i = mb(1)
   IF ( ivec==1 .OR. eject(11)/=0 ) WRITE (nout,99002) igpt
99002 FORMAT (1H0,20X,'E Q U I L I B R I U M   C H E C K   L O A D S',/,1H0,16X,'RESULTANT LOADS AT POINT',I7,                      &
             &' IN BASIC COORDINATE SYSTEM')
   IF ( nskip<=0 ) GOTO 800
!
!     STATICS SUBCASES
!
   IF ( mb(4)==0 ) mb(4) = mb(7)
   IF ( mb(4)==0 ) mb(4) = mb(6)
   WRITE (nout,99003) mb(1) , mb(4)
99003 FORMAT (1H0,24X,7HSUBCASE,I8,8H,   LOAD,I8)
   WRITE (nout,99005)
!
   GOTO 900
!
!     EOF FOUND
!
 700  IF ( ivec>maxvec ) GOTO 1000
   IF ( nskip>0 ) GOTO 1200
   lsteig = .TRUE.
!
!     EIGENVALUE PROBLEM
!
 800  parm(2) = klama
   CALL read(*1200,*1100,klama,mb(2),7,0,i)
   WRITE (nout,99004) mb(1) , mb(2) , freq
99004 FORMAT (1H0,24X,7HSUBCASE,I8,8H,   MODE,I5,13H,   FREQUENCY,1P,E15.6)
   WRITE (nout,99005)
!
!     LOOP ON OUTPUT CATAGORY
!
 900  k = nentry/6 + 1
   ihdcnt = 1
   DO i = 3 , 8
      core(i,k) = 0.0E0
   ENDDO
!
   DO i = 1 , 3
      IF ( ma(i)/=0 ) THEN
         core(1,ihdcnt) = head(1,i)
         core(2,ihdcnt) = head(2,i)
         j = ma(i) + ivec*6 - 6
!
         DO l = 3 , 8
            core(l,ihdcnt) = zz(j)
            core(l,k) = core(l,k) + zz(j)
            j = j + 1
         ENDDO
         ihdcnt = ihdcnt + 1
      ENDIF
   ENDDO
!
   core(1,k) = head(1,4)
   core(2,k) = head(2,4)
   IF ( k==2 ) WRITE (nout,99006) cor1
   IF ( k==3 ) WRITE (nout,99006) cor3
   IF ( k==4 ) WRITE (nout,99006) core
   IF ( ivec<maxvec ) GOTO 600
 1000 CALL close(kscc,krw)
   IF ( nskip<=0 ) CALL close(klama,krw)
   RETURN
!
!     ERROR MESSAGES
!
!     EOR
!
 1100 parm(1) = 3
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 1000
!
!     EOF
!
 1200 parm(1) = 2
   CALL mesage(parm(1),parm(2),parm(3))
   GOTO 1000
99005 FORMAT (1H0,5X,46H-TYPE-        T1             T2             T3,13X,32HR1             R2             R3)
!
99006 FORMAT (5X,2A4,1P,6E15.6)
END SUBROUTINE eqmcks
