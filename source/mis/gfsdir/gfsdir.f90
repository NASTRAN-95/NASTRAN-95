!*==gfsdir.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gfsdir
   IMPLICIT NONE
   USE c_bitpos
   USE c_blank
   USE c_packx
   USE c_patx
   USE c_system
   USE c_two
   USE c_zblpkx
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: aay , aaybar , ac , afy , ajw , amy , any , anybar , aoy , awj , awy , ayw , file , gjw , gyw , h , ibuf , ident ,    &
            & kaabar , kc , kjj , kjjl , kwwbar , mbit , mrow , mt , mwwbar , n , nuy , obit , pvec , sbit , scr9 , sfbit
   INTEGER , SAVE :: afry , axy , dkaa , dkfrfr , gia , gm , go , hc , kaa , kmat , kyy , maa , mmat , scr1 , scr2 , scr3 , scr4 ,  &
                   & scr5 , scr6 , scr7 , scr8 , usetf , usets
   INTEGER , DIMENSION(11) , SAVE :: badd
   REAL*8 , DIMENSION(5) :: dbadd
   INTEGER , DIMENSION(7) :: mcb
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
   lcore = korsz(z(1))
   ibuf = lcore - sysbuf - 1
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
      mbit = andf(mcb(5),two(um))
      sbit = andf(mcb(5),two(us))
      obit = andf(mcb(5),two(uo))
!
      uset = usets
!
!     PARTITION OUT MULTIPOINT CONSTRAINTS
!
      IF ( mbit/=0 ) THEN
         CALL calcv(pvec,ug,un,um,z(1))
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
         CALL calcv(pvec,un,uf,us,z(1))
         CALL gfsptn(any,afy,0,0,0,0,pvec)
      ELSE
!
         CALL gfswch(afy,any)
      ENDIF
!
!     PARTITION OUT OMITS
!
      IF ( obit/=0 ) THEN
         CALL calcv(pvec,uf,ua,uo,z(1))
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
      uset = usetf
      IF ( nofree<0 ) THEN
!
         CALL gfswch(awy,aay)
      ELSE
         CALL calcv(pvec,ua,uab,ufr,z(1))
         CALL gfsmrg(awy,aay,afry,0,0,0,pvec)
      ENDIF
!
!     DETERMINE IF ANY SINGLE POINT CONSTRAINTS EXIST ON THE FLUID
!
      CALL calcv(pvec,uy,uf,us,z(1))
      nuy = nsub0 + nsub1
      sfbit = 1
      IF ( nsub1==0 ) sfbit = 0
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
         nsub0 = nuy - 1
         nsub1 = 1
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
         IF ( comptp>0 ) THEN
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
      IF ( nograv<0 ) THEN
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
      IF ( nofree<0 ) THEN
!
         CALL gfswch(kwwbar,kaabar)
         mwwbar = maa
      ELSE
         CALL calcv(pvec,ua,uab,ufr,z(1))
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
      ELSEIF ( comptp>0 ) THEN
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
         CALL calcv(pvec,uy,uf,us,z(1))
         CALL gfsmrg(gyw,gjw,0,0,0,0,pvec)
      ELSE
         CALL ssg2b(h,gjw,0,gyw,1,2,1,scr5)
      ENDIF
!
!     PARTITON OUT THE FREE SURFACE POINTS
!
      IF ( nofree>=0 ) THEN
         CALL calcv(pvec,uy,ufr,ui,z(1))
         CALL gfsptn(gyw,0,gia,0,0,0,pvec)
         RETURN
      ENDIF
   ENDIF
!
   CALL gfswch(gia,gyw)
99999 END SUBROUTINE gfsdir
