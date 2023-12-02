!*==rcovuo.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE rcovuo(Pid,Uao,Lastss)
   IMPLICIT NONE
   USE c_blank
   USE c_fbsx
   USE c_names
   USE c_packx
   USE c_parmeg
   USE c_rcovcm
   USE c_rcovcr
   USE c_saddx
   USE c_sfact
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Pid
   INTEGER :: Uao
   INTEGER , DIMENSION(2) :: Lastss
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(1) :: dz
   INTEGER :: file , i , idpcor , item , lcorez , mattyp , n , pao , rc
   INTEGER , DIMENSION(1) :: iz
   INTEGER , SAVE :: kmtx , lmtx , pove , scr2 , scr3 , scr4 , scr6 , scr7 , scr8 , scr9 , uprt
   INTEGER , DIMENSION(7) :: mcbpao
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE CALCULATES THE FULL SIZE DISPLACEMENT VECTOR ON
!     ANY OMITTED POINTS.  THE OPTIONAL INERTIA AND DAMPING EFFECTS
!     WILL BE INCLUDED IF REQUESTED.
!
!     FILE USAGE IS AS FOLLOWS
!
!     SCR1 AND SCR5 ARE NOT USED
!     SCR4 CONTAINS PID ON INPUT AND IS DESTROYED
!     SCR7 CONTAINS UAO OUTPUT
!     ALL OTHER SCRATCH FILES ARE USED
!
!    4                 SOLN       ,SRD        ,SWRT       ,SCHK       ,
   !>>>>EQUIVALENCE (Z(1),Iz(1),Dz(1))
   DATA name/4HRCOV , 4HUO  /
   DATA pove , lmtx/4HPOVE , 4HLMTX/
   DATA uprt , kmtx/4HUPRT , 4HKMTX/
   DATA scr2 , scr3 , scr4 , scr6 , scr7 , scr8 , scr9/302 , 303 , 304 , 306 , 307 , 308 , 309/
!
!     SET UP COMMON BLOCKS
!
   lcorez = korsz(z) - lreq - icore - 1
   idpcor = icore/2 + 1
   rule = 0
   mcbk21(1) = 0
   mcbk12(1) = 0
   mcbk22(1) = 0
   sign = 1
!
!     CALCUATE THE LOADS ON THE OMMITED POINTS
!
   pao = 0
   IF ( rfno/=3 ) THEN
      pao = scr3
      CALL rcovsl(Lastss,pove,0,scr6,scr7,scr8,pao,z(icore),z(icore),sof3-icore-1,.FALSE.,rfno)
      mcbpao(1) = pao
      CALL rdtrl(mcbpao)
   ENDIF
!
!     ADD IN OPTIONAL INERTIA AND DAMPING FORCES TO THE LOADS ON THE
!     OMMITED POINTS
!
   IF ( Pid/=0 ) THEN
      IF ( pao==0 ) THEN
!
!     NO STATIC LOADS SO THE ADD IS UNECESSARY
!
         mcbpao(1) = Pid
         CALL rdtrl(mcbpao)
      ELSE
         nomat = 2
         typa = 1
         alpha = 1.0
         mcbaa(1) = Pid
         CALL rdtrl(mcbaa)
         typb = 1
         beta = 1.0
         mcbbb(1) = pao
         CALL rdtrl(mcbbb)
         CALL makmcb(mcbxx,scr6,mcbaa(3),rect,mcbaa(5))
         mcbxx(2) = mcbaa(2)
         lcor = lcorez
         CALL sofcls
         CALL sadd(dz(idpcor),dz(idpcor))
         CALL wrttrl(mcbxx)
         DO i = 1 , 7
            mcbpao(i) = mcbxx(i)
         ENDDO
         CALL sofopn(z(sof1),z(sof2),z(sof3))
      ENDIF
   ENDIF
!
   IF ( mcbpao(1)<=0 ) THEN
!
!     NO LOADS SO THE DISPLACEMENTS ARE ZERO
!
      Uao = 0
      CALL sofcls
      RETURN
   ELSE
!
!     CHECK FOR EXISTENCE OF LMTX ON THE SOF.  IF IT EXISTS
!     SKIP THE PARTN AND DECOMP
!
      CALL softrl(Lastss,lmtx,lmcb(1))
      IF ( lmcb(1)/=1 ) THEN
!
!     COMPUTE THE KOO PARTITION OF KMTX FOR LASTSS
!
!     COPY THE PARTITIONING VECTOR TO SCR2
!
         CALL mtrxi(scr2,Lastss,uprt,0,rc)
         item = uprt
         IF ( rc/=1 ) GOTO 200
!
!     COPY KMTX TO SCR5
!
         item = kmtx
         CALL mtrxi(scr8,Lastss,kmtx,0,rc)
         IF ( rc/=1 ) GOTO 200
         mcbk(1) = scr8
         CALL rdtrl(mcbk)
!
!     PARTITION KMTX INTO KOO.  STORE KOO ON SCR4.
!
         CALL sofcls
         iz(icore) = scr2
         CALL rdtrl(iz(icore))
         CALL makmcb(mcbk11,scr9,mcbpao(3),sym,mcbk(5))
         mcbk11(2) = mcbpao(3)
         mrgz = lcorez - 7
         i = (icore+7)/2 + 1
         CALL partn(z(icore),z(icore),dz(i))
         CALL wrttrl(mcbk11)
!
!     DECOMPOSE KOO
!
         DO i = 1 , 7
            mcba(i) = mcbk11(i)
         ENDDO
         CALL makmcb(mcbl,scr2,mcba(3),lower,mcba(5))
         mcblt(1) = scr8
         scra = scr3
         IF ( scra==mcbpao(1) ) scra = scr6
         scrb = scr4
         IF ( scrb==mcbpao(1) ) scrb = scr6
         scrc = scr7
         sdcmpz = mrgz
         power = 1
         chlsky = 0
         CALL sdcomp(*100,dz(idpcor),dz(idpcor),dz(idpcor))
         CALL wrttrl(mcbl)
!
!     FORWARD AND BACKWARD SUBSTITUTION TO SOLVE FOR UAO
!
         DO i = 1 , 7
            lmcb(i) = mcbl(i)
            bmcb(i) = mcbpao(i)
         ENDDO
      ELSE
!
!     BRING IN LMTX FROM SOF AND SET UP FOR FBS DIRECTLY
!
         CALL mtrxi(scr2,Lastss,lmtx,0,rc)
         DO i = 1 , 7
            bmcb(i) = mcbpao(i)
         ENDDO
         lmcb(1) = scr2
         CALL sofcls
      ENDIF
      fbsz = lcorez
      mattyp = bmcb(5)
      CALL makmcb(xmcb,scr8,bmcb(3),rect,mattyp)
      prec = 2 - (mattyp-2*(mattyp/2))
      CALL fbs(dz(idpcor),dz(idpcor))
      CALL wrttrl(xmcb)
!
!     MERGE UAO INTO THE UA SET
!
!     COPY UPRT BACK TO SCR2
!
      CALL sofopn(z(sof1),z(sof2),z(sof3))
      item = uprt
      CALL mtrxi(scr2,Lastss,uprt,0,rc)
      IF ( rc/=1 ) GOTO 200
      CALL sofcls
      iz(icore) = scr2
      CALL rdtrl(iz(icore))
!
!     SETUP MCB-S IN /PARMEG/
!
      DO i = 1 , 7
         mcbk11(i) = xmcb(i)
      ENDDO
      Uao = scr7
      CALL makmcb(mcbk,Uao,iz(icore+2),rect,mcbk11(5))
      mcbk(2) = xmcb(2)
      IF ( rfno==9 ) mcbk(2) = 3*xmcb(2)
!
!     SETUP A NULL ROW PARTITIONING VECTOR OR FOR RIGID FORMAT 9 A
!     VECTOR THAT WILL MERGE IN A NULL VELOCITY AND ACCELERATION
!     VECTOR FOR EACH DISPLACEMENT VECTOR
!
      nro = mcbk(2)
      CALL makmcb(z(icore+7),scr6,nro,rect,rsp)
      IF ( nro+15>lcorez ) THEN
!
         n = 8
         iopt = -1
         CALL sofcls
         CALL mesage(n,file,name)
         CALL close(pao,rew)
         CALL close(scr3,rew)
         GOTO 99999
      ELSE
         DO i = 1 , nro
            z(icore+14+i) = 0.0
         ENDDO
         IF ( rfno==9 ) THEN
            DO i = 1 , nro , 3
               z(icore+15+i) = 1.0
               z(icore+16+i) = 1.0
            ENDDO
         ENDIF
         CALL gopen(scr6,z(buf1),wrtrew)
         typin = 1
         typot = 1
         iro = 1
         incrp = 1
         CALL pack(z(icore+15),scr6,iz(icore+7))
         CALL close(scr6,rew)
         CALL wrttrl(iz(icore+7))
!
         mrgz = lcorez - 14
         i = (icore+14)/2 + 1
         CALL merge(z(icore+7),z(icore),dz(i))
         CALL wrttrl(mcbk)
!
!     NORMAL RETURN
!
         RETURN
      ENDIF
   ENDIF
!
!     ERROR PROCESSING
!
 100  WRITE (nout,99001) swm , Lastss
99001 FORMAT (A27,' 6311, SDCOMP DECOMPOSITION FAILED ON KOO MATRIX ','FOR SUBSTRUCTURE ',2A4)
   GOTO 300
 200  IF ( rc==2 ) rc = 3
   CALL smsg(rc-2,item,Lastss)
 300  iopt = -1
99999 END SUBROUTINE rcovuo
