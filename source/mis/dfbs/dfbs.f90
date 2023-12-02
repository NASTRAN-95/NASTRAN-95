!*==dfbs.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dfbs
   IMPLICIT NONE
   USE c_blank
   USE c_fbsx
   USE c_gfbsx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: b , l , scr , u , x
   INTEGER , DIMENSION(3) , SAVE :: dosi , refus
   INTEGER :: iprec1 , jj , kprec , ltype , n , nogo , outpt
   INTEGER , DIMENSION(2) , SAVE :: sbnm
   REAL , DIMENSION(1) :: zz
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
!     FBS   L,U,B/X/V,Y,ISYM=0/V,Y,KSIGN=1/V,Y,IPREC=0/V,Y,ITYPE=0 $
!
!     ISYM  =  1  USE FBS
!           = -1  USE GFBS
!           =  0  CHOOSE WHICH BASED ON SUPPLIED INPUT
!     KSIGN =  1, SOLVE LUX= B
!             -1,       LUX=-B
!     IPREC = REQUESTED PRECISION - DEFAULT BASED ON INPUT OR SYSTEM(55)
!     ITYPE = REQUESTED TYPE OF X - DEFAULT IS LOGICAL CHOICE ON INPUT
!
!     REVISED  12/91 BY G.CHAN/UNISYS
!     FATAL ERROR IN FBS (NOT GFBS) IF INPUT MATRIX IS NOT A LOWER
!     TRIANGULAR FACTOR
!
!ZZ   COMMON /ZZDFB1/ Z(1)
!ZZ   COMMON /ZZDFB2/ ZZ(1)
   !>>>>EQUIVALENCE (Zz(1),Z(1))
   !>>>>EQUIVALENCE (Ksystm(55),Kprec) , (Ksystm(2),Outpt)
   DATA l , u , b , x , scr/101 , 102 , 103 , 201 , 301/
   DATA sbnm/4HDFBS , 1H /
   DATA dosi/4HSING , 4HDOUB , 4HMLTP/ , refus/2*3H    , 3HREF/
!
!
   ju(1) = u
   CALL rdtrl(ju)
   DO
      IF ( isym<0 ) THEN
!
!     SET UP CALL TO GFBS
!
         jl(1) = l
         CALL rdtrl(jl)
         n = jl(2)
         jb(1) = b
         CALL rdtrl(jb)
         jnx = korsz(zz)
         iprec1 = max0(jl(5),jb(5),ju(5))
         IF ( iprec1>2 ) iprec1 = iprec1 - 2
         IF ( iprec1<1 .OR. iprec1>2 ) iprec1 = kprec
         IF ( iprec/=iprec1 .AND. iprec/=0 ) THEN
            IF ( iprec<1 .OR. iprec>2 ) iprec = 3
            WRITE (outpt,99002) swm , dosi(iprec) , refus(iprec) , sbnm , dosi(iprec1)
            IF ( iprec/=3 ) iprec1 = iprec
         ENDIF
         iprec = iprec1
         jp1 = iprec1
         js1 = ksign
         jx(1) = x
         ltype = iprec1
         IF ( jl(5)==3 .OR. jl(5)==4 .OR. ju(5)==3 .OR. ju(5)==4 .OR. jl(5)==3 .OR. jl(5)==4 ) ltype = iprec1 + 2
         IF ( itype/=0 .AND. itype/=ltype ) THEN
            jj = 1
            IF ( itype<1 .OR. itype>4 ) jj = 3
            WRITE (outpt,99003) swm , itype , refus(jj) , sbnm , ltype
            IF ( jj/=3 ) ltype = itype
         ENDIF
         itype = ltype
         jx(5) = itype
         CALL gfbs(zz,zz)
         jx(3) = n
         jx(4) = 2
         IF ( jx(3)==jx(2) ) jx(4) = 1
         CALL wrttrl(jx)
      ELSEIF ( isym==0 ) THEN
         isym = -1
         IF ( ju(1)<0 ) isym = 1
         CYCLE
      ELSE
!
!     SET UP CALL TO FBS
!
         nogo = 0
         il(1) = l
         CALL rdtrl(il)
         IF ( il(1)<=0 ) THEN
            CALL mesage(30,198,l)
            nogo = 1
         ENDIF
         IF ( il(4)/=4 ) THEN
!
            CALL fname(il(1),il(2))
            WRITE (outpt,99001) il(2) , il(3) , il(4)
99001       FORMAT ('0*** INPUT MATRIX ',2A4,' TO FBS MODULE IS NOT A LOWER ','TRIANGULAR FACTOR.  FORM =',I4)
            CALL errtrc('DFBS    ',110)
         ELSE
            n = il(2)
            ib(1) = b
            CALL rdtrl(ib)
            IF ( nogo/=0 ) CALL mesage(-30,199,sbnm)
            inx = korsz(z)
            iprec1 = max0(il(5),ib(5),iu(5))
            IF ( iprec1>2 ) iprec1 = iprec1 - 2
            IF ( iprec1<1 .OR. iprec1>2 ) iprec1 = kprec
            IF ( iprec/=iprec1 .AND. iprec/=0 ) THEN
               IF ( iprec<1 .OR. iprec>2 ) iprec = 3
               WRITE (outpt,99002) swm , dosi(iprec) , refus(iprec) , sbnm , dosi(iprec1)
               IF ( iprec/=3 ) iprec1 = iprec
            ENDIF
            iprec = iprec1
            ip1 = iprec1
            is1 = ksign
            ltype = iprec1
!WKBR spr 93014  1   .IL(5).EQ.3 .OR. IL(5).EQ.4)  LTYPE = IPREC1 + 2
            IF ( il(5)==3 .OR. il(5)==4 .OR. iu(5)==3 .OR. iu(5)==4 .OR. ib(5)==3 .OR. ib(5)==4 ) ltype = iprec1 + 2
            IF ( itype/=0 .AND. itype/=ltype ) THEN
               jj = 1
               IF ( itype<1 .OR. itype>4 ) jj = 3
               WRITE (outpt,99003) swm , itype , refus(jj) , sbnm , ltype
               IF ( jj/=3 ) ltype = itype
            ENDIF
            itype = ltype
            ix(5) = itype
            ix(1) = x
            iscr = scr
            CALL fbs(z,z)
            ix(3) = n
            ix(4) = 2
            IF ( ix(3)==ix(2) ) ix(4) = 1
            CALL wrttrl(ix)
         ENDIF
      ENDIF
      EXIT
   ENDDO
99002 FORMAT (A27,' 2163, REQUESTED ',A4,'LE PRECISION ',A3,' USED BY ',2A4,2H. ,A4,'LE PRECISION IS LOGICAL CHOICE')
99003 FORMAT (A27,' 2164, REQUESTED TYPE ',I4,2H, ,A3,' USED BY ',2A4,'. TYPE ',I4,' IS LOGICAL CHOICE.')
!
END SUBROUTINE dfbs
