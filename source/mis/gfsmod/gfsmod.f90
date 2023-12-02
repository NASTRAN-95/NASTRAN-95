!*==gfsmod.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gfsmod
   USE c_bitpos
   USE c_blank
   USE c_gfsmox
   USE c_packx
   USE c_patx
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
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
         axy = 101
         afry = 102
         kyy = 103
         dkaa = 104
         dkfrfr = 105
         usetf = 111
         phia = 112
         phix = 113
         lama = 114
         kmat = 201
         mmat = 202
         gia = 203
         pout = 204
         scr1 = 301
         scr2 = 302
         scr3 = 303
         scr4 = 304
         scr5 = 305
         scr6 = 306
         scr7 = 307
         scr8 = 308
         name(1) = namex(1)
         name(2) = namex(2)
         DO i = 1 , 11
            badd(i) = 0
         ENDDO
!
!
         phiar = scr4
         phixr = scr2
!
         lcore = korsz(z(1))
         ibuf = lcore - sysbuf - 1
         IF ( ibuf<0 ) THEN
            n = -8
!
            CALL mesage(n,file,name)
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
            file = phix
            mcb(1) = phix
            CALL rdtrl(mcb)
            IF ( mcb(1)<0 ) THEN
!
!     ERROR EXITS
!
               n = -1
               CALL mesage(n,file,name)
               RETURN
            ELSE
               nmodes = mcb(2)
               IF ( llmode>nmodes .OR. llmode==0 ) llmode = -1
               lmodes = llmode
               IF ( lmodes<=0 ) lmodes = nmodes
               IF ( lmodes<=0 .OR. lmodes>nmodes ) lmodes = nmodes
               izm = two(uz) + two(um) + two(uh)
               inzm = two(unz) + two(um)
               IF ( ibuf<=nmodes ) THEN
                  n = -8
                  CALL mesage(n,file,name)
                  RETURN
               ELSE
                  DO i = 1 , nmodes
                     z(i) = izm
                     IF ( i>lmodes ) z(i) = inzm
                  ENDDO
!
!     SET FREE SURFACE DISPLACEMENTS
!
                  ifr = two(ufr) + two(uh)
                  lvec = nmodes
                  IF ( nofree<0 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL gopen(usetf,z(ibuf),0)
                  DO
                     CALL read(*20,*20,usetf,ibit,1,0,n)
                     IF ( andf(ibit,two(ufr))/=0 ) THEN
                        lvec = lvec + 1
                        IF ( lvec>=ibuf ) THEN
                           n = -8
                           CALL mesage(n,file,name)
                           RETURN
                        ELSE
                           z(lvec) = ifr
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
!
 20      CALL close(usetf,1)
         spag_nextblock_1 = 2
      CASE (2)
!
!     WRITE DUMMY USETD FILE
!
         CALL gopen(usetd,z(ibuf),1)
         CALL write(usetd,z(1),lvec,1)
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
         uset = usetd
         IF ( lmodes>=nmodes ) THEN
!
            phixr = phix
         ELSE
            CALL calcv(pvec,um,uz,unz,z(1))
            CALL gfsptn(phix,phixr,0,0,0,pvec,0)
         ENDIF
!
!     TRANSFORM THE FLUID STRUCTURE AREA MATRIX
!
         CALL ssg2b(phixr,axy,0,azy,1,2,1,scr5)
!
!     IF FREE SURFACE POINTS EXIST - MERGE THEM WITH THE TRANSFORMED
!     AREA MATRIX
!
         IF ( nofree<0 ) THEN
!
            CALL gfswch(ahy,azy)
         ELSE
            CALL calcv(pvec,uh,uz,ufr,z(1))
            CALL gfsmrg(ahy,azy,afry,0,0,0,pvec)
         ENDIF
!
!     DETERMINE IF ANY SINGLE POINT CONSTRAINTS EXIST ON THE FLUID
!
         uset = usetf
         CALL calcv(pvec,uy,uf,us,z(1))
         nuy = nsub0 + nsub1
         sfbit = 1
         IF ( nsub1==0 ) sfbit = 0
!
!     IF SPC POINTS EXIST ON THE FLUID - PARTITION THEM OUT OF THE
!     FLUID AREA AND STIFFNESS MATRICES
!
         IF ( sfbit/=0 ) THEN
            CALL gfsptn(ahy,ahj,0,0,0,pvec,0)
            CALL gfstrn(ahj,ajh,scr5,scr6)
            CALL gfsptn(kyy,kjj,0,0,0,pvec,pvec)
         ELSE
!
!     IF NO SPC POINTS EXIST ON THE FLUID, CONSTRAIN THE FIRST FLUID
!     POINT TO REMOVE POTENTIAL SINGULARITIES
!
            IF ( comptp>0 ) WRITE (nout,99001) uwm
99001       FORMAT (A25,' 8015. THE PURELY INCOMPRESSIBLE METHOD IS AVAIL','ABLE ONLY WITH THE DIRECT FORMULATION.')
            CALL gfsspc(nuy,pvec)
            nsub0 = nuy - 1
            nsub1 = 1
            CALL gfsptn(kyy,kjj,0,0,0,pvec,pvec)
!
!     GENERATE THE H TRANSFORMATION MATRIX
!
            CALL gfsh(nuy,h)
            CALL gfstrn(ahy,ayh,scr2,scr6)
            CALL ssg2b(h,ayh,0,ajh,0,2,1,scr6)
!
!     GENERATE THE COMPRESSIBLITY MATRIX
!
            CALL gfscom(ahy,nuy,kc,ident,ac,scr6)
         ENDIF
!
!     SOLVE FOR THE INITIAL PRESSURE TRANSFORMATION MATRIX
!
         CALL factor(kjj,kjjl,scr5,scr6,scr9,scr10)
         CALL ssg3a(0,kjjl,ajh,gjh,scr5,scr6,-1,0)
!
!     FOR COMPUTER CORE CONSERVATION REASON, THE REST OF GFSMOD IS
!     MOVED TO GFSMO2, WHICH CAN BE SEGMENTED IN PARALLEL WITH GFSMOD.
!
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gfsmod
