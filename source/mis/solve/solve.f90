!*==solve.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE solve
   USE c_blank
   USE c_cdcmpx
   USE c_dcompx
   USE c_fbsx
   USE c_gfbsx
   USE c_names
   USE c_sfact
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: a , b , iscr1 , iscr2 , iscr3 , iscr4 , iscr5 , x
   INTEGER , DIMENSION(3) , SAVE :: dosi , refus
   INTEGER :: i , ia5 , ib5 , iform , index , jj , kprec , ltype , n , no , outpt
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(1) :: zz , zzz , zzzz , zzzzz
   EXTERNAL cdcomp , decomp , fbs , gfbs , korsz , makmcb , mesage , rdtrl , sdcomp , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     SOLVE IS A DMAP DRIVER TO SOLVE THE MATRIX EQUATION AX=B
!
!     SOLVE   A,B/X/SYM/SIGN/PREC/TYPE $
!
!     SYM     =  1 - USE SYMETRIC DECOMPOSITION
!                0 - CHOOSE WHICH DECOMPOSITION BASED ON INPUT MATRIX
!               -1 - USE UNSYMETRIC DECOMPOSITION
!     ISIGN   =  1   SOLVE AX = B
!               -1   SOLVE AX =-B
!     IPREC   =  PRECISION USED IN THE FBS PASS
!     ITYPE   =  DESIRED TYPE OF THE OUTPUT MATRIX X
!
!
   !>>>>EQUIVALENCE (Zz(1),Z(1))
   !>>>>EQUIVALENCE (Zzz(1),Z(1))
   !>>>>EQUIVALENCE (Zzzz(1),Z(1))
   !>>>>EQUIVALENCE (Zzzzz(1),Z(1))
   !>>>>EQUIVALENCE (Ksystm(55),Kprec) , (Ksystm(2),Outpt)
   DATA a , b , x/101 , 102 , 201/ , name/4HSOLV , 4HE   /
   DATA iscr1 , iscr2 , iscr3 , iscr4 , iscr5/301 , 302 , 303 , 304 , 305/
   DATA dosi/4HSING , 4HDOUB , 4HMLTP/ , refus/2*3H    , 3HREF/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         ja(1) = a
         CALL rdtrl(ja)
!
         iform = ja(4)
         IF ( isym<0 ) THEN
            IF ( iform==sym ) WRITE (outpt,99001) uwm , name
99001       FORMAT (A25,' 2340, MODULE ',2A4,' HAS BEEN REQUESTED TO DO ','UNSYMETRIC DECOMPOSITION OF A SYMETRIC MATRIX.')
            iform = rect
            IF ( ja(2)==ja(3) ) iform = sqr
         ELSEIF ( isym/=0 ) THEN
            IF ( ja(2)==ja(3) .AND. iform/=sym ) WRITE (outpt,99002) swm , name
99002       FORMAT (A27,' 2341, MODULE ',2A4,' HAS BEEN FURNISHED A SQUARE ','MATRIX MARKED UNSYMETRIC FOR SYMETRIC DECOMPOSITION.')
            iform = sym
         ENDIF
         isym = -1
         IF ( iform==sym ) isym = 1
         ja(4) = iform
         IF ( isym<0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SET UP CALL TO SDCOMP AND FBS
!
         index = 1
         ichol = 0
         DO i = 1 , 7
            ifila(i) = ja(i)
         ENDDO
         n = ifila(2)
         ifill1(1) = iscr1
         ifilc(1) = iscr2
         iscr11 = iscr3
         iscr22 = iscr4
         iscr33 = iscr5
         nz = korsz(z)
         ifill1(5) = ifila(5)
         CALL sdcomp(*20,z,z,z)
         ifill1(3) = ifill1(2)
         ifill1(4) = lower
         CALL wrttrl(ifill1)
         ifill(1) = iscr1
         CALL rdtrl(ifill)
         ifilb(1) = b
         CALL rdtrl(ifilb)
!
!     IF THE B MATRIX IS PURGED, ASSUME AN IDENTITY MATRIX IN ITS PLACE
!
         IF ( ifilb(1)<=0 ) CALL makmcb(ifilb,b,n,identy,ja(5))
         isign1 = ksign
         ia5 = ifila(5)
         ib5 = ifilb(5)
         spag_nextblock_1 = 2
      CASE (2)
!
!     DETERMINE THE PRECISION FOR THE CALCULATIONS
!     AND THE TYPE OF THE OUTPUT MATRIX
!
         iprec1 = kprec
         IF ( (ia5>0 .AND. ia5<=4) .OR. (ib5>0 .AND. ib5<=4) ) iprec1 = 1
         IF ( ia5==2 .OR. ia5==4 .OR. ib5==2 .OR. ib5==4 ) iprec1 = 2
         IF ( iprec/=iprec1 .AND. iprec/=0 ) THEN
            IF ( iprec<1 .OR. iprec>2 ) iprec = 3
            WRITE (outpt,99003) swm , dosi(iprec) , refus(iprec) , name , dosi(iprec1)
99003       FORMAT (A27,' 2163, REQUESTED ',A4,'LE PRECISION ',A3,'USED BY ',2A4,2H. ,A4,'LE PRECISION IS LOGICAL CHOICE')
            IF ( iprec/=3 ) iprec1 = iprec
         ENDIF
         iprec = iprec1
         ltype = iprec1
         IF ( ia5==3 .OR. ia5==4 .OR. ib5==3 .OR. ib5==4 ) ltype = iprec1 + 2
         IF ( itype/=0 .AND. itype/=ltype ) THEN
            jj = 1
            IF ( itype<1 .OR. itype>4 ) jj = 3
            WRITE (outpt,99004) sfm , itype , refus(jj) , name , ltype
99004       FORMAT (A27,' 2164, REQUESTED TYPE ',I4,2H, ,A3,'USED BY ',2A4,'. TYPE ',I4,' IS LOGICAL CHOICE.')
            IF ( jj/=3 ) ltype = itype
         ENDIF
         itype = ltype
         IF ( index==2 ) THEN
            ipr = iprec
!
!     DEFINE THE MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX
!
            CALL makmcb(jx,x,n,rect,itype)
            nzzz = korsz(zzzz)
            IF ( jb(4)==identy ) jb(5) = iprec
            CALL gfbs(zzzz,zzzz)
            IF ( jx(2)==n ) jx(4) = sqr
            CALL wrttrl(jx)
            RETURN
         ELSE
!
!     DEFINE THE MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX
!
            CALL makmcb(ifilx,x,n,rect,itype)
            nx = korsz(zz)
            IF ( ifilb(4)==identy ) ifilb(5) = iprec
            iscr = iscr1
            CALL fbs(zz,zz)
            IF ( ifilx(2)==n ) ifilx(4) = sqr
            CALL wrttrl(ifilx)
            RETURN
         ENDIF
!
 20      no = isign(5,isym)
         isym = -1
         CALL mesage(no,a,name)
         spag_nextblock_1 = 3
      CASE (3)
!
!     SET UP THE CALL TO DECOMP AND GFBS
!
         index = 2
         IF ( ja(5)>2 ) THEN
!
!     SET UP CALL TO CDCOMP AND GFBS
!
            kl(1) = iscr1
            ku(1) = iscr2
            jscr1 = iscr3
            jscr2 = iscr4
            jscr3 = iscr5
            nzzzz = korsz(zzzzz)
            ja(4) = sqr
            n = ja(2)
            kl(5) = ja(5)
            jbb = 0
            jbbar = 0
            CALL cdcomp(*20,zzzzz,zzzzz,zzzzz)
            DO i = 1 , 7
               jl(i) = kl(i)
               ju(i) = ku(i)
            ENDDO
         ELSE
            ia(1) = a
            il(1) = iscr1
            iu(1) = iscr2
            isr1 = iscr3
            isr3 = iscr5
            isr2 = iscr4
            nzz = korsz(zzz)
            CALL rdtrl(ia)
            ia(4) = sqr
            n = ia(2)
            il(5) = ja(5)
            ib = 0
            ibbar = 0
            CALL decomp(*20,zzz,zzz,zzz)
            DO i = 1 , 7
               jl(i) = il(i)
               ju(i) = iu(i)
            ENDDO
         ENDIF
         jb(1) = b
         CALL rdtrl(jb)
!
!     IF THE B MATRIX IS PURGED, ASSUME AN IDENTITY MATRIX IN ITS PLACE
!
         IF ( jb(1)<=0 ) CALL makmcb(jb,b,n,identy,ja(5))
         ia5 = ja(5)
         ib5 = jb(5)
!
!     DETERMINE THE PRECISION FOR THE CALCULATIONS
!     AND THE TYPE OF THE OUTPUT MATRIX
!
         isgn = ksign
         spag_nextblock_1 = 2
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE solve
