!*==gfsmod.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gfsmod
USE C_BITPOS
USE C_BLANK
USE C_GFSMOX
USE C_PACKX
USE C_PATX
USE C_SYSTEM
USE C_TWO
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ac , ahj , ahy , ajh , ayh , azy , file , gjh , h , i , ibit , ident , ifr , inzm , izm , kc , kjj , kjjl , lvec , n ,&
            & nuy , phiar , phixr , pvec , scr10 , scr9 , usetd
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: namex
   EXTERNAL andf , calcv , close , factor , gfscom , gfsh , gfsmrg , gfsptn , gfsspc , gfstrn , gfswch , gopen , korsz , mesage ,   &
          & rdtrl , read , ssg2b , ssg3a , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE PERFORMS THE MODAL FORMULATION OF THE
!     FLUID / STRUCTURE MATRICES
!
   !>>>>EQUIVALENCE (Badd(2),Dbadd(1)) , (Rz(1),Z(1)) , (Scr1,Usetd) , (Scr2,Pvec,Ident,Kjjl) , (Scr3,Azy,Ahj,Kjj,Gjh) ,                 &
!>>>>    & (Scr4,Ajh,Khhbar,Gyh) , (Scr5,Ac,Ayh,Mzz,Kzzbar) , (Scr6,Kzz) , (Scr7,Kc,Ahy) , (Scr8,H) , (Scr9,Mmat) , (Scr10,Gia,Mhhbar)
!     DATA            AXY      ,AFRY     ,KYY      ,DKAA     ,DKFRFR   ,
!    1                USETF    ,PHIA     ,PHIX     ,LAMA     ,
!    2                KMAT     ,MMAT     ,GIA      ,POUT     ,
!    3                SCR1     ,SCR2     ,SCR3     ,SCR4     ,SCR5     ,
!    4                SCR6     ,SCR7     ,SCR8     /
!    5                101      ,102      ,103      ,104      ,105      ,
!    6                111      ,112      ,113      ,114      ,
!    7                201      ,202      ,203      ,204      ,
!    8                301      ,302      ,303      ,304      ,305      ,
!    9                306      ,307      ,308      /
!
!     DATA    BADD  / 11*0   /
   DATA namex/4HGFSM , 4HOD  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         Axy = 101
         Afry = 102
         Kyy = 103
         Dkaa = 104
         Dkfrfr = 105
         Usetf = 111
         Phia = 112
         Phix = 113
         Lama = 114
         Kmat = 201
         Mmat = 202
         Gia = 203
         Pout = 204
         Scr1 = 301
         Scr2 = 302
         Scr3 = 303
         Scr4 = 304
         Scr5 = 305
         Scr6 = 306
         Scr7 = 307
         Scr8 = 308
         Name(1) = namex(1)
         Name(2) = namex(2)
         DO i = 1 , 11
            Badd(i) = 0
         ENDDO
!
!
         phiar = Scr4
         phixr = Scr2
!
         Lcore = korsz(Z(1))
         Ibuf = Lcore - Sysbuf - 1
         IF ( Ibuf<0 ) THEN
            n = -8
!
            CALL mesage(n,file,Name)
            RETURN
         ELSE
!
!     CREATE A DUMMY USET VECTOR TOR USE WITH THE MODAL DISPLACEMENTS
!
!     BIT POSITIONS WILL BE
!
!     UM  - MODAL POINT  UZ + UNZ
!     UZ  - DESIRED MODAL POINT
!     UNZ - MODAL POINT TO BE SKIPPED
!     UFR - FREE SURFACE POINT
!     UH  - UFR + UZ
!
!     SET MODAL DISPLACEMENTS
!
            file = Phix
            mcb(1) = Phix
            CALL rdtrl(mcb)
            IF ( mcb(1)<0 ) THEN
!
!     ERROR EXITS
!
               n = -1
               CALL mesage(n,file,Name)
               RETURN
            ELSE
               Nmodes = mcb(2)
               IF ( Llmode>Nmodes .OR. Llmode==0 ) Llmode = -1
               Lmodes = Llmode
               IF ( Lmodes<=0 ) Lmodes = Nmodes
               IF ( Lmodes<=0 .OR. Lmodes>Nmodes ) Lmodes = Nmodes
               izm = Two(Uz) + Two(Um) + Two(Uh)
               inzm = Two(Unz) + Two(Um)
               IF ( Ibuf<=Nmodes ) THEN
                  n = -8
                  CALL mesage(n,file,Name)
                  RETURN
               ELSE
                  DO i = 1 , Nmodes
                     Z(i) = izm
                     IF ( i>Lmodes ) Z(i) = inzm
                  ENDDO
!
!     SET FREE SURFACE DISPLACEMENTS
!
                  ifr = Two(Ufr) + Two(Uh)
                  lvec = Nmodes
                  IF ( Nofree<0 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL gopen(Usetf,Z(Ibuf),0)
                  DO
                     CALL read(*20,*20,Usetf,ibit,1,0,n)
                     IF ( andf(ibit,Two(Ufr))/=0 ) THEN
                        lvec = lvec + 1
                        IF ( lvec>=Ibuf ) THEN
                           n = -8
                           CALL mesage(n,file,Name)
                           RETURN
                        ELSE
                           Z(lvec) = ifr
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
!
 20      CALL close(Usetf,1)
         spag_nextblock_1 = 2
      CASE (2)
!
!     WRITE DUMMY USETD FILE
!
         CALL gopen(usetd,Z(Ibuf),1)
         CALL write(usetd,Z(1),lvec,1)
         CALL close(usetd,1)
         mcb(1) = usetd
         mcb(2) = lvec
         DO i = 3 , 7
            mcb(i) = 0
         ENDDO
         CALL wrttrl(mcb)
!
!     EXTRACT THE DESIRED MODES FORM THE PHIX MATRIX
!
         Uset = usetd
         IF ( Lmodes>=Nmodes ) THEN
!
            phixr = Phix
         ELSE
            CALL calcv(pvec,Um,Uz,Unz,Z(1))
            CALL gfsptn(Phix,phixr,0,0,0,pvec,0)
         ENDIF
!
!     TRANSFORM THE FLUID STRUCTURE AREA MATRIX
!
         CALL ssg2b(phixr,Axy,0,azy,1,2,1,Scr5)
!
!     IF FREE SURFACE POINTS EXIST - MERGE THEM WITH THE TRANSFORMED
!     AREA MATRIX
!
         IF ( Nofree<0 ) THEN
!
            CALL gfswch(ahy,azy)
         ELSE
            CALL calcv(pvec,Uh,Uz,Ufr,Z(1))
            CALL gfsmrg(ahy,azy,Afry,0,0,0,pvec)
         ENDIF
!
!     DETERMINE IF ANY SINGLE POINT CONSTRAINTS EXIST ON THE FLUID
!
         Uset = Usetf
         CALL calcv(pvec,Uy,Uf,Us,Z(1))
         nuy = Nsub0 + Nsub1
         Sfbit = 1
         IF ( Nsub1==0 ) Sfbit = 0
!
!     IF SPC POINTS EXIST ON THE FLUID - PARTITION THEM OUT OF THE
!     FLUID AREA AND STIFFNESS MATRICES
!
         IF ( Sfbit/=0 ) THEN
            CALL gfsptn(ahy,ahj,0,0,0,pvec,0)
            CALL gfstrn(ahj,ajh,Scr5,Scr6)
            CALL gfsptn(Kyy,kjj,0,0,0,pvec,pvec)
         ELSE
!
!     IF NO SPC POINTS EXIST ON THE FLUID, CONSTRAIN THE FIRST FLUID
!     POINT TO REMOVE POTENTIAL SINGULARITIES
!
            IF ( Comptp>0 ) WRITE (Nout,99001) Uwm
99001       FORMAT (A25,' 8015. THE PURELY INCOMPRESSIBLE METHOD IS AVAIL','ABLE ONLY WITH THE DIRECT FORMULATION.')
            CALL gfsspc(nuy,pvec)
            Nsub0 = nuy - 1
            Nsub1 = 1
            CALL gfsptn(Kyy,kjj,0,0,0,pvec,pvec)
!
!     GENERATE THE H TRANSFORMATION MATRIX
!
            CALL gfsh(nuy,h)
            CALL gfstrn(ahy,ayh,Scr2,Scr6)
            CALL ssg2b(h,ayh,0,ajh,0,2,1,Scr6)
!
!     GENERATE THE COMPRESSIBLITY MATRIX
!
            CALL gfscom(ahy,nuy,kc,ident,ac,Scr6)
         ENDIF
!
!     SOLVE FOR THE INITIAL PRESSURE TRANSFORMATION MATRIX
!
         CALL factor(kjj,kjjl,Scr5,Scr6,scr9,scr10)
         CALL ssg3a(0,kjjl,ajh,gjh,Scr5,Scr6,-1,0)
!
!     FOR COMPUTER CORE CONSERVATION REASON, THE REST OF GFSMOD IS
!     MOVED TO GFSMO2, WHICH CAN BE SEGMENTED IN PARALLEL WITH GFSMOD.
!
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gfsmod
