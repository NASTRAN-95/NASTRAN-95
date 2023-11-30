
SUBROUTINE gfsdir
   IMPLICIT NONE
   REAL A(4) , Form , Kcomp , Rz(1)
   INTEGER Bit(12) , Comptp , Ii , Incr , Irow , Lcore , Lmodes , Nn , Nofree , Nograv , Nsub0 , Nsub1 , Nsub2 , Sysbuf , Two(32) , &
         & Typin , Typout , Ua , Uab , Uf , Ufr , Ug , Ui , Ul , Um , Un , Uo , Ur , Us , Usb , Uset , Usg , Ux , Uy , Uz , Z(1)
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug , Bit , Ux , Uy , Ufr , Uz , Uab , Ui
   COMMON /blank / Nograv , Nofree , Kcomp , Comptp , Form , Lmodes
   COMMON /packx / Typin , Typout , Ii , Nn , Incr
   COMMON /patx  / Lcore , Nsub0 , Nsub1 , Nsub2 , Uset
   COMMON /system/ Sysbuf
   COMMON /two   / Two
   COMMON /zblpkx/ A , Irow
   COMMON /zzzzzz/ Z
   INTEGER aay , aaybar , ac , afry , afy , ajw , amy , any , anybar , aoy , awj , awy , axy , ayw , badd(11) , dkaa , dkfrfr ,     &
         & file , gia , gjw , gm , go , gyw , h , hc , ibuf , ident , kaa , kaabar , kc , kjj , kjjl , kmat , kwwbar , kyy , maa ,  &
         & mbit , mcb(7) , mmat , mrow , mt , mwwbar , n , name(2) , nuy , obit , pvec , sbit , scr1 , scr2 , scr3 , scr4 , scr5 ,  &
         & scr6 , scr7 , scr8 , scr9 , sfbit , usetf , usets
   INTEGER andf , korsz
   DOUBLE PRECISION dbadd(5)
   REAL rbadd(12)
   EXTERNAL andf
!
!     THIS ROUTINE PERFORMS THE DIRECT FORMULATION OF THE
!     FLUID/STRUCTURE MATRICES
!
!
!     MODULE PARAMETERS
!
!
!     SYSTEM COMMON
!
!
!     CALCV COMMON BLOCK
!
!
!     OPEN CORE
!
!
!     PACK COMMON BLOCKS
!
!
!     POWERS OF TWO
!
!
!     USET BIT POSITIONS
!
!
!     SCRATCH FILE ASSIGNMENTS
!
   !>>>>EQUIVALENCE (badd(1),rbadd(2)) , (dbadd(1),rbadd(3)) , (Rz(1),Z(1)) , (scr1,pvec,ident,kjjl) , (scr2,anybar,afy,awy) ,           &
!>>>>    & (scr3,amy,aaybar,awj,gjw,kjj) , (scr4,aoy,ajw,gyw) , (scr5,aay,kwwbar,ayw) , (scr6,ac) , (scr7,kc,mt) , (scr8,h) , (scr9,gia)
!
!     GINO FILE ASSIGNMENTS
!
   DATA axy , afry , kyy , dkaa , dkfrfr , kaa , maa , gm , go , usets , usetf , kmat , mmat , hc , gia , scr1 , scr2 , scr3 ,      &
      & scr4 , scr5 , scr6 , scr7 , scr8/101 , 102 , 103 , 104 , 105 , 106 , 107 , 108 , 109 , 110 , 111 , 201 , 202 , 205 , 203 ,  &
      & 301 , 302 , 303 , 304 , 305 , 306 , 307 , 308/
!
   DATA name/4HGFSD , 4HIR  /
   DATA badd/11*0/
!
!
   any = scr4
   kaabar = scr2
   mwwbar = scr6
!
   Lcore = korsz(Z(1))
   ibuf = Lcore - Sysbuf - 1
   IF ( ibuf<0 ) THEN
!
!     ERROR CONDITIONS
!
      n = -8
      CALL mesage(n,file,name)
      GOTO 99999
   ELSE
!
!     REDUCE FLUID / STRUCTURE AREA MATRIX.  MATRIX IS TREATED AS
!     A LOAD VECTOR
!
      mcb(1) = usets
      CALL rdtrl(mcb)
      mbit = andf(mcb(5),Two(Um))
      sbit = andf(mcb(5),Two(Us))
      obit = andf(mcb(5),Two(Uo))
!
      Uset = usets
!
!     PARTITION OUT MULTIPOINT CONSTRAINTS
!
      IF ( mbit/=0 ) THEN
         CALL calcv(pvec,Ug,Un,Um,Z(1))
         CALL gfsptn(axy,anybar,amy,0,0,0,pvec)
         CALL ssg2b(gm,amy,anybar,any,1,2,1,scr1)
      ELSE
!
         any = axy
      ENDIF
!
!     PARTITION OUT SINGLE POINT CONSTRAINTS
!
      IF ( sbit/=0 ) THEN
         CALL calcv(pvec,Un,Uf,Us,Z(1))
         CALL gfsptn(any,afy,0,0,0,0,pvec)
      ELSE
!
         CALL gfswch(afy,any)
      ENDIF
!
!     PARTITION OUT OMITS
!
      IF ( obit/=0 ) THEN
         CALL calcv(pvec,Uf,Ua,Uo,Z(1))
         CALL gfsptn(afy,aaybar,aoy,0,0,0,pvec)
         CALL ssg2b(go,aoy,aaybar,aay,1,2,1,scr1)
      ELSE
!
         CALL gfswch(aay,afy)
      ENDIF
!
!     IF FREE SURFACE POINTS EXIST - MERGE THEM WITH THE REDUCED
!     AREA MATRIX
!
      Uset = usetf
      IF ( Nofree<0 ) THEN
!
         CALL gfswch(awy,aay)
      ELSE
         CALL calcv(pvec,Ua,Uab,Ufr,Z(1))
         CALL gfsmrg(awy,aay,afry,0,0,0,pvec)
      ENDIF
!
!     DETERMINE IF ANY SINGLE POINT CONSTRAINTS EXIST ON THE FLUID
!
      CALL calcv(pvec,Uy,Uf,Us,Z(1))
      nuy = Nsub0 + Nsub1
      sfbit = 1
      IF ( Nsub1==0 ) sfbit = 0
!
!     IF SPC POINTS EXIST ON THE FLUID - PARTITION THEM OUT OF
!     THE FLUID AREA AND STIFFNESS MATRIX
!
      IF ( sfbit/=0 ) THEN
         CALL gfsptn(awy,awj,0,0,0,pvec,0)
         CALL gfstrn(awj,ajw,scr2,scr5)
         CALL gfsptn(kyy,kjj,0,0,0,pvec,pvec)
      ELSE
!
!     NO SPC POINTS EXIST ON THE FLUID
!
!     CONSTRAIN THE FIRST FLUID POINT TO REMOVE ANY POTENTIAL
!     SINGULARITIES
!
         CALL gfsspc(nuy,pvec)
         Nsub0 = nuy - 1
         Nsub1 = 1
         CALL gfsptn(kyy,kjj,0,0,0,pvec,pvec)
!
!     GENERATE THE H TRANSFORMATION MATRIX
!
         CALL gfsh(nuy,h)
         CALL gfstrn(awy,ayw,scr1,scr6)
         CALL ssg2b(h,ayw,0,ajw,0,2,1,scr6)
!
!     CHECK COMPRESSIBLITY TYPE
!
         IF ( Comptp>0 ) THEN
!
!     PURELY INCOMPRESSIBLE APPROACH - A CONSTRAINT EQUATION IS
!     GENERATED TO RESTRICT VOLUME CHANGE
!
!     GENERATE HC MATRIX WHICH CONTAINS THE CONSTRAINT
!
            CALL gfshc(awy,nuy,hc,ident,ac,mrow)
         ELSE
!
!     A SPRING WILL BE GENERATED TO COUPLE THE STRUCTURE AND THE
!     FREE SURFACE TO RESTRICT VOLUME CHANGES
!
!     COMPUTE THE COMPRESSIBLITY MATRIX WHICH CONTAINS THIS SPRING
!
            CALL gfscom(awy,nuy,kc,ident,ac,scr5)
         ENDIF
      ENDIF
!
!     SOLVE FOR THE INITIAL PRESSURE TRANSFORMATION MATRIX
!
      CALL factor(kjj,kjjl,scr2,scr5,scr6,scr9)
      CALL ssg3a(0,kjjl,ajw,gjw,scr5,scr6,-1,0)
!
!     IF GRAVITY EXISTS - ADD THE ADDITIONAL STIFFNESS
!
      IF ( Nograv<0 ) THEN
!
         kaabar = kaa
      ELSE
         badd(1) = 2
         dbadd(1) = 1.0D0
         badd(7) = 2
         dbadd(4) = 1.0D0
         CALL ssg2c(kaa,dkaa,kaabar,0,badd)
      ENDIF
!
!     IF FREE SURFACE EXISTS - MERGE THE STIFFNESS TO SOLUTION SIZE
!     AND EXPAND THE MASS MATRIX
!
      IF ( Nofree<0 ) THEN
!
         CALL gfswch(kwwbar,kaabar)
         mwwbar = maa
      ELSE
         CALL calcv(pvec,Ua,Uab,Ufr,Z(1))
         CALL gfsmrg(kwwbar,kaabar,0,0,dkfrfr,pvec,pvec)
         CALL gfsmrg(mwwbar,maa,0,0,0,pvec,pvec)
      ENDIF
!
!     COMPUTE THE FINAL MASS MATRIX
!     FOR COMPTP = 1 THIS MATRIX IS NOT THE FINAL ONE
!
      CALL ssg2b(ajw,gjw,mwwbar,mmat,1,2,1,scr2)
!
!     COMPUTE THE FINAL STIFFNESS MATRIX
!
      IF ( sfbit/=0 ) THEN
!
         CALL gfswch(kmat,kwwbar)
      ELSEIF ( Comptp>0 ) THEN
!
!     APPLY THE CONSTRAINT EQUATION TO STIFFNESS AND MASS FOR
!     THE INCOMPRESSIBLE APPROACH
!
         CALL ssg2b(hc,kwwbar,0,scr2,1,2,1,scr1)
         CALL ssg2b(scr2,hc,0,kmat,0,2,1,scr1)
         CALL ssg2b(hc,mmat,0,scr2,1,2,1,scr1)
         CALL ssg2b(scr2,hc,0,mt,0,2,1,scr1)
!
!     ADD 1.0 TO THE NULL COLUMN IN THE MASS MATRIX TO PREVENT
!     SINGULATITIES
!
         CALL gfsmt(mt,mmat,mrow)
      ELSE
!
!     ADD IN THE SPRING FACTOR KC
!
         badd(1) = 2
         dbadd(1) = 1.0D0
         badd(7) = 2
         dbadd(4) = 1.0D0
         CALL ssg2c(kwwbar,kc,kmat,0,badd)
      ENDIF
!
!     TRANSFORM THE FINAL PRESSURE TRANSFORMATION MATRIX OR IF
!     SPC POINTS EXIST ON THE FLUID MERGE IN ZEROS
!
      IF ( sfbit/=0 ) THEN
!
         CALL calcv(pvec,Uy,Uf,Us,Z(1))
         CALL gfsmrg(gyw,gjw,0,0,0,0,pvec)
      ELSE
         CALL ssg2b(h,gjw,0,gyw,1,2,1,scr5)
      ENDIF
!
!     PARTITON OUT THE FREE SURFACE POINTS
!
      IF ( Nofree>=0 ) THEN
         CALL calcv(pvec,Uy,Ufr,Ui,Z(1))
         CALL gfsptn(gyw,0,gia,0,0,0,pvec)
         RETURN
      ENDIF
   ENDIF
!
   CALL gfswch(gia,gyw)
   RETURN
99999 RETURN
END SUBROUTINE gfsdir