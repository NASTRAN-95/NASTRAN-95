
SUBROUTINE dfbs
   IMPLICIT NONE
   INTEGER Ib(7) , Il(7) , Inx , Ip1 , Iprec , Is1 , Iscr , Isym , Itype , Iu(7) , Ix(7) , Jb(7) , Jl(7) , Jnx , Jp1 , Js1 , Ju(7) ,&
         & Jx(7) , Kprec , Ksign , Ksystm(65) , Outpt
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL Z(20000) , Zz(1)
   COMMON /blank / Isym , Ksign , Iprec , Itype
   COMMON /fbsx  / Il , Iu , Ib , Ix , Inx , Ip1 , Is1 , Iscr
   COMMON /gfbsx / Jl , Ju , Jb , Jx , Jnx , Jp1 , Js1
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
   INTEGER b , dosi(3) , iprec1 , jj , l , ltype , n , nogo , refus(3) , sbnm(2) , scr , u , x
   INTEGER korsz
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
   Ju(1) = u
   CALL rdtrl(Ju)
   DO
      IF ( Isym<0 ) THEN
!
!     SET UP CALL TO GFBS
!
         Jl(1) = l
         CALL rdtrl(Jl)
         n = Jl(2)
         Jb(1) = b
         CALL rdtrl(Jb)
         Jnx = korsz(Zz)
         iprec1 = max0(Jl(5),Jb(5),Ju(5))
         IF ( iprec1>2 ) iprec1 = iprec1 - 2
         IF ( iprec1<1 .OR. iprec1>2 ) iprec1 = Kprec
         IF ( Iprec/=iprec1 .AND. Iprec/=0 ) THEN
            IF ( Iprec<1 .OR. Iprec>2 ) Iprec = 3
            WRITE (Outpt,99002) Swm , dosi(Iprec) , refus(Iprec) , sbnm , dosi(iprec1)
            IF ( Iprec/=3 ) iprec1 = Iprec
         ENDIF
         Iprec = iprec1
         Jp1 = iprec1
         Js1 = Ksign
         Jx(1) = x
         ltype = iprec1
         IF ( Jl(5)==3 .OR. Jl(5)==4 .OR. Ju(5)==3 .OR. Ju(5)==4 .OR. Jl(5)==3 .OR. Jl(5)==4 ) ltype = iprec1 + 2
         IF ( Itype/=0 .AND. Itype/=ltype ) THEN
            jj = 1
            IF ( Itype<1 .OR. Itype>4 ) jj = 3
            WRITE (Outpt,99003) Swm , Itype , refus(jj) , sbnm , ltype
            IF ( jj/=3 ) ltype = Itype
         ENDIF
         Itype = ltype
         Jx(5) = Itype
         CALL gfbs(Zz,Zz)
         Jx(3) = n
         Jx(4) = 2
         IF ( Jx(3)==Jx(2) ) Jx(4) = 1
         CALL wrttrl(Jx)
      ELSEIF ( Isym==0 ) THEN
         Isym = -1
         IF ( Ju(1)<0 ) Isym = 1
         CYCLE
      ELSE
!
!     SET UP CALL TO FBS
!
         nogo = 0
         Il(1) = l
         CALL rdtrl(Il)
         IF ( Il(1)<=0 ) THEN
            CALL mesage(30,198,l)
            nogo = 1
         ENDIF
         IF ( Il(4)/=4 ) THEN
!
            CALL fname(Il(1),Il(2))
            WRITE (Outpt,99001) Il(2) , Il(3) , Il(4)
99001       FORMAT ('0*** INPUT MATRIX ',2A4,' TO FBS MODULE IS NOT A LOWER ','TRIANGULAR FACTOR.  FORM =',I4)
            CALL errtrc('DFBS    ',110)
         ELSE
            n = Il(2)
            Ib(1) = b
            CALL rdtrl(Ib)
            IF ( nogo/=0 ) CALL mesage(-30,199,sbnm)
            Inx = korsz(Z)
            iprec1 = max0(Il(5),Ib(5),Iu(5))
            IF ( iprec1>2 ) iprec1 = iprec1 - 2
            IF ( iprec1<1 .OR. iprec1>2 ) iprec1 = Kprec
            IF ( Iprec/=iprec1 .AND. Iprec/=0 ) THEN
               IF ( Iprec<1 .OR. Iprec>2 ) Iprec = 3
               WRITE (Outpt,99002) Swm , dosi(Iprec) , refus(Iprec) , sbnm , dosi(iprec1)
               IF ( Iprec/=3 ) iprec1 = Iprec
            ENDIF
            Iprec = iprec1
            Ip1 = iprec1
            Is1 = Ksign
            ltype = iprec1
!WKBR spr 93014  1   .IL(5).EQ.3 .OR. IL(5).EQ.4)  LTYPE = IPREC1 + 2
            IF ( Il(5)==3 .OR. Il(5)==4 .OR. Iu(5)==3 .OR. Iu(5)==4 .OR. Ib(5)==3 .OR. Ib(5)==4 ) ltype = iprec1 + 2
            IF ( Itype/=0 .AND. Itype/=ltype ) THEN
               jj = 1
               IF ( Itype<1 .OR. Itype>4 ) jj = 3
               WRITE (Outpt,99003) Swm , Itype , refus(jj) , sbnm , ltype
               IF ( jj/=3 ) ltype = Itype
            ENDIF
            Itype = ltype
            Ix(5) = Itype
            Ix(1) = x
            Iscr = scr
            CALL fbs(Z,Z)
            Ix(3) = n
            Ix(4) = 2
            IF ( Ix(3)==Ix(2) ) Ix(4) = 1
            CALL wrttrl(Ix)
         ENDIF
      ENDIF
      EXIT
   ENDDO
99002 FORMAT (A27,' 2163, REQUESTED ',A4,'LE PRECISION ',A3,' USED BY ',2A4,2H. ,A4,'LE PRECISION IS LOGICAL CHOICE')
99003 FORMAT (A27,' 2164, REQUESTED TYPE ',I4,2H, ,A3,' USED BY ',2A4,'. TYPE ',I4,' IS LOGICAL CHOICE.')
!
END SUBROUTINE dfbs