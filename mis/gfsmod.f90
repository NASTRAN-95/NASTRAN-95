
SUBROUTINE gfsmod
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ac , Afry , Ahj , Ahy , Ajh , Axy , Ayh , Azy , Badd(11) , Comptp , Dkaa , Dkfrfr , Gia , Gjh , Gyh , H , Ibuf , Ident , &
         & Ii , Incr , Kc , Kcomp , Khhbar , Kjj , Kjjl , Kmat , Kyy , Kzz , Kzzbar , Lama , Lcore , Llmode , Lmodes , Mhhbar ,     &
         & Mmat , Mzz , Name(2) , Nmodes , Nn , Nofree , Nograv , Nout , Nsub0 , Nsub1 , Nsub2 , Phia , Phix , Pout , Pvec , Scr1 , &
         & Scr10 , Scr2 , Scr3 , Scr4 , Scr5 , Scr6 , Scr7 , Scr8 , Scr9 , Sfbit , Sysbuf , Two(32) , Typin , Typout , Uf , Ufr ,   &
         & Uh , Ui , Um , Unz , Us , Uset , Usetd , Usetf , Uy , Uz , Z(1)
   REAL Bit1(3) , Bit2(15) , Bit3(2) , Form , Rz(1)
   DOUBLE PRECISION Dbadd(5)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /bitpos/ Unz , Uz , Um , Uh , Bit1 , Uf , Us , Bit2 , Uy , Ufr , Bit3 , Ui
   COMMON /blank / Nograv , Nofree , Kcomp , Comptp , Form , Llmode
   COMMON /gfsmox/ Axy , Afry , Kyy , Dkaa , Dkfrfr , Usetf , Phia , Phix , Lama , Kmat , Mmat , Gia , Pout , Scr1 , Scr2 , Scr3 ,  &
                 & Scr4 , Scr5 , Scr6 , Scr7 , Scr8 , Lmodes , Nmodes , Ibuf , Sfbit , Badd , Name
   COMMON /packx / Typin , Typout , Ii , Nn , Incr
   COMMON /patx  / Lcore , Nsub0 , Nsub1 , Nsub2 , Uset
   COMMON /system/ Sysbuf , Nout
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER andf , korsz
   INTEGER file , i , ibit , ifr , inzm , izm , lvec , mcb(7) , n , namex(2) , nuy , phiar , phixr
   EXTERNAL andf
!
! End of declarations
!
!
!     THIS ROUTINE PERFORMS THE MODAL FORMULATION OF THE
!     FLUID / STRUCTURE MATRICES
!
   EQUIVALENCE (Badd(2),Dbadd(1)) , (Rz(1),Z(1)) , (Scr1,Usetd) , (Scr2,Pvec,Ident,Kjjl) , (Scr3,Azy,Ahj,Kjj,Gjh) ,                 &
    & (Scr4,Ajh,Khhbar,Gyh) , (Scr5,Ac,Ayh,Mzz,Kzzbar) , (Scr6,Kzz) , (Scr7,Kc,Ahy) , (Scr8,H) , (Scr9,Mmat) , (Scr10,Gia,Mhhbar)
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
      GOTO 99999
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
         GOTO 99999
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
            GOTO 99999
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
            IF ( Nofree<0 ) GOTO 200
            CALL gopen(Usetf,Z(Ibuf),0)
            DO
               CALL read(*100,*100,Usetf,ibit,1,0,n)
               IF ( andf(ibit,Two(Ufr))/=0 ) THEN
                  lvec = lvec + 1
                  IF ( lvec>=Ibuf ) THEN
                     n = -8
                     CALL mesage(n,file,Name)
                     GOTO 99999
                  ELSE
                     Z(lvec) = ifr
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
   ENDIF
!
 100  CALL close(Usetf,1)
!
!     WRITE DUMMY USETD FILE
!
 200  CALL gopen(Usetd,Z(Ibuf),1)
   CALL write(Usetd,Z(1),lvec,1)
   CALL close(Usetd,1)
   mcb(1) = Usetd
   mcb(2) = lvec
   DO i = 3 , 7
      mcb(i) = 0
   ENDDO
   CALL wrttrl(mcb)
!
!     EXTRACT THE DESIRED MODES FORM THE PHIX MATRIX
!
   Uset = Usetd
   IF ( Lmodes>=Nmodes ) THEN
!
      phixr = Phix
   ELSE
      CALL calcv(Pvec,Um,Uz,Unz,Z(1))
      CALL gfsptn(Phix,phixr,0,0,0,Pvec,0)
   ENDIF
!
!     TRANSFORM THE FLUID STRUCTURE AREA MATRIX
!
   CALL ssg2b(phixr,Axy,0,Azy,1,2,1,Scr5)
!
!     IF FREE SURFACE POINTS EXIST - MERGE THEM WITH THE TRANSFORMED
!     AREA MATRIX
!
   IF ( Nofree<0 ) THEN
!
      CALL gfswch(Ahy,Azy)
   ELSE
      CALL calcv(Pvec,Uh,Uz,Ufr,Z(1))
      CALL gfsmrg(Ahy,Azy,Afry,0,0,0,Pvec)
   ENDIF
!
!     DETERMINE IF ANY SINGLE POINT CONSTRAINTS EXIST ON THE FLUID
!
   Uset = Usetf
   CALL calcv(Pvec,Uy,Uf,Us,Z(1))
   nuy = Nsub0 + Nsub1
   Sfbit = 1
   IF ( Nsub1==0 ) Sfbit = 0
!
!     IF SPC POINTS EXIST ON THE FLUID - PARTITION THEM OUT OF THE
!     FLUID AREA AND STIFFNESS MATRICES
!
   IF ( Sfbit/=0 ) THEN
      CALL gfsptn(Ahy,Ahj,0,0,0,Pvec,0)
      CALL gfstrn(Ahj,Ajh,Scr5,Scr6)
      CALL gfsptn(Kyy,Kjj,0,0,0,Pvec,Pvec)
   ELSE
!
!     IF NO SPC POINTS EXIST ON THE FLUID, CONSTRAIN THE FIRST FLUID
!     POINT TO REMOVE POTENTIAL SINGULARITIES
!
      IF ( Comptp>0 ) WRITE (Nout,99001) Uwm
99001 FORMAT (A25,' 8015. THE PURELY INCOMPRESSIBLE METHOD IS AVAIL','ABLE ONLY WITH THE DIRECT FORMULATION.')
      CALL gfsspc(nuy,Pvec)
      Nsub0 = nuy - 1
      Nsub1 = 1
      CALL gfsptn(Kyy,Kjj,0,0,0,Pvec,Pvec)
!
!     GENERATE THE H TRANSFORMATION MATRIX
!
      CALL gfsh(nuy,H)
      CALL gfstrn(Ahy,Ayh,Scr2,Scr6)
      CALL ssg2b(H,Ayh,0,Ajh,0,2,1,Scr6)
!
!     GENERATE THE COMPRESSIBLITY MATRIX
!
      CALL gfscom(Ahy,nuy,Kc,Ident,Ac,Scr6)
   ENDIF
!
!     SOLVE FOR THE INITIAL PRESSURE TRANSFORMATION MATRIX
!
   CALL factor(Kjj,Kjjl,Scr5,Scr6,Scr9,Scr10)
   CALL ssg3a(0,Kjjl,Ajh,Gjh,Scr5,Scr6,-1,0)
!
!     FOR COMPUTER CORE CONSERVATION REASON, THE REST OF GFSMOD IS
!     MOVED TO GFSMO2, WHICH CAN BE SEGMENTED IN PARALLEL WITH GFSMOD.
!
   RETURN
99999 RETURN
END SUBROUTINE gfsmod
