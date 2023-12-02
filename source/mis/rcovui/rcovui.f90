!*==rcovui.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovui(Ub,Lastss,Modal)
USE C_BLANK
USE C_MPYADX
USE C_NAMES
USE C_PACKX
USE C_PARMEG
USE C_RCOVCM
USE C_RCOVCR
USE C_SADDX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ub
   INTEGER , DIMENSION(2) :: Lastss
   LOGICAL :: Modal
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bgg , bmtx , gims , horg , k4gg , k4mx , mgg , mmtx , nhpdat , scr2 , scr3 , scr4 , scr5 , scr6 , scr7 , scr8 ,&
                   & scr9 , uprt
   INTEGER :: dua , i , idp , idpcor , item , lcorez , nrowo , pid , rc , uad , uao , upart
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(2) , SAVE :: name
   LOGICAL :: reqf
   REAL , DIMENSION(1) :: rz
   EXTERNAL bug , close , gopen , korsz , makmcb , merge , mesage , mpyad , mtrxi , pack , partn , rcovuo , rcovva , rdtrl , sadd , &
          & smsg , sofcls , sofopn , softrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE CALCULATES THE IMPROVED LOWER LEVEL DISPLACEMENTS
!     ON A REDUCED SUBSTRUCTURE WHICH INCLUDE INERTIA AND DAMPING
!     EFFECTS
!
   !>>>>EQUIVALENCE (Dz(1),Rz(1),Z(1))
   DATA scr2 , scr3 , scr4 , scr5 , scr6 , scr7 , scr8 , scr9/302 , 303 , 304 , 305 , 306 , 307 , 308 , 309/
   DATA horg , mmtx , bmtx , uprt/4HHORG , 4HMMTX , 4HBMTX , 4HUPRT/
   DATA k4mx/4HK4MX/ , k4gg/110/
   DATA gims , nhpdat/4HGIMS , 4HPDAT/
   DATA mgg , bgg/104 , 109/
   DATA name/4HRCOV , 4HUI  /
!
!     INITILIZE
!
   lcorez = korsz(Z) - Lreq - Icore - 1
   idpcor = Icore/2 + 1
   Tflag = 0
   Signab = 1
   Signc = 1
   Mprec = 0
   Scrm = 309
   reqf = .FALSE.
   IF ( Lastss(1)==Fss(1) .AND. Lastss(2)==Fss(2) ) reqf = .TRUE.
!
!     GENERATE THE PARTIAL LOAD VECTOR USING THE NORMAL TRANSFORMATION
!
!     UPARTIAL = HORG*UB
!
   item = horg
   CALL mtrxi(scr2,Lastss,horg,0,rc)
   IF ( rc/=1 ) THEN
      CALL spag_block_7
      RETURN
   ENDIF
!
   Mcba(1) = scr2
   CALL rdtrl(Mcba)
   Mcbb(1) = Ub
   CALL rdtrl(Mcbb)
   Mcbc(1) = 0
   upart = scr5
   CALL makmcb(Mcbd,upart,Mcba(3),Rect,Mcbb(5))
   Mpyz = lcorez
   CALL sofcls
   CALL mpyad(dz(idpcor),dz(idpcor),dz(idpcor))
   CALL wrttrl(Mcbd)
   CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
!
!     DETERMINE THE NUMBER OF OMITTED POINTS
!
   nrowo = Mcba(3) - Mcba(2)
   CALL softrl(Lastss,gims,Mcba)
   IF ( Mcba(1)==1 ) nrowo = Mcba(3)
!
!     GENERATE THE VELOCITIES AND ACCELERATIONS
!
   Lcore = Buf4 - Icore - 1
   CALL rcovva(upart,0,0,0,scr7,scr8,Lastss,dz(idpcor),dz(idpcor),dz(idpcor))
   IF ( upart<=0 ) THEN
      CALL spag_block_8
      RETURN
   ENDIF
!
!     CALCULATE THE INERTIAL AND DAMPING LOADS
!
!     PID = -M*A - B*V
!
!     CALCULATE THE INERTAIL LOADS
!
   pid = 0
   IF ( reqf ) THEN
      Mcba(1) = mgg
      IF ( Mcba(1)>0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDIF
   CALL mtrxi(scr2,Lastss,mmtx,0,rc)
   IF ( rc/=1 ) THEN
      CALL spag_block_2
      RETURN
   ENDIF
   Mcba(1) = scr2
   CALL rdtrl(Mcba)
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      Mcbb(1) = scr8
      CALL rdtrl(Mcbb)
      Mcbc(1) = 0
      CALL makmcb(Mcbd,scr6,Mcbb(3),Rect,Mcbb(5))
      Signab = -1
      CALL sofcls
!
      CALL mpyad(dz(idpcor),dz(idpcor),dz(idpcor))
!
      DO i = 1 , 7
         Mcbc(i) = Mcbd(i)
      ENDDO
      pid = scr6
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     CALCULATE THE DAMPING LOADS
!
      IF ( Rfno==3 ) THEN
         CALL spag_block_6
         RETURN
      ENDIF
      IF ( reqf ) THEN
         Mcba(1) = k4gg
         CALL rdtrl(Mcba)
         IF ( Mcba(1)>0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
      ENDIF
      CALL mtrxi(scr2,Lastss,k4mx,0,rc)
      IF ( rc/=1 ) THEN
         CALL spag_block_4
         RETURN
      ENDIF
      Mcba(1) = scr2
      CALL rdtrl(Mcba)
      CALL spag_block_3
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
      Mcbb(1) = scr7
      CALL rdtrl(Mcbb)
      CALL makmcb(Mcbd,scr8,Mcbb(3),Rect,Mcbb(5))
      Signab = -1
      CALL sofcls
      CALL mpyad(dz(idpcor),dz(idpcor),dz(idpcor))
      pid = scr8
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      DO i = 1 , 7
         Mcbc(i) = Mcbd(i)
      ENDDO
      CALL spag_block_4
   END SUBROUTINE spag_block_3
   SUBROUTINE spag_block_4
!
      IF ( reqf ) THEN
         Mcba(1) = bgg
         CALL rdtrl(Mcba)
         IF ( Mcba(1)>0 ) THEN
            CALL spag_block_5
            RETURN
         ENDIF
      ENDIF
      CALL mtrxi(scr2,Lastss,bmtx,0,rc)
      IF ( rc/=1 ) THEN
         CALL spag_block_6
         RETURN
      ENDIF
      Mcba(1) = scr2
      CALL rdtrl(Mcba)
      CALL spag_block_5
   END SUBROUTINE spag_block_4
   SUBROUTINE spag_block_5
      Mcbb(1) = scr7
      CALL rdtrl(Mcbb)
      CALL makmcb(Mcbd,scr6,Mcbb(3),Rect,Mcbb(5))
      Signab = -1
      CALL sofcls
      CALL mpyad(dz(idpcor),dz(idpcor),dz(idpcor))
      pid = scr6
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      CALL spag_block_6
   END SUBROUTINE spag_block_5
   SUBROUTINE spag_block_6
!
!     PARTITION THE INERTIA AND DAMPING LOADS TO THE OMIT SET
!
!     GET THE PARTITIONING VECTOR FROM THE SOF
!
      IF ( pid/=0 ) THEN
         item = uprt
         CALL mtrxi(scr2,Lastss,uprt,0,rc)
         IF ( rc/=1 ) THEN
            CALL spag_block_7
            RETURN
         ENDIF
         Rule = 0
         Mrgz = lcorez - 14
         idp = (Icore+14)/2 + 1
         DO i = 1 , 7
            Mcb(i) = Mcbd(i)
         ENDDO
         pid = scr4
         CALL makmcb(Mcb11,pid,nrowo,Rect,Mcbd(5))
         Mcb11(2) = Mcbd(2)
         Mcb12(1) = 0
         Mcb21(1) = 0
         Mcb22(1) = 0
!
!     SET UP A NULL ROW PARTITION VECTOR
!
         Z(Icore) = scr2
         CALL rdtrl(Z(Icore))
         CALL makmcb(Z(Icore+7),0,Mcb(2),Rect,Rsp)
         Z(Icore+8) = 1
         CALL sofcls
         CALL partn(Z(Icore+7),Z(Icore),dz(idp))
         CALL wrttrl(Mcb11)
         CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      ENDIF
!
!     PERFORM THE FBS TO GET THE LOADS ON THE OMMITTED POINTS.  WE
!     WILL ALSO ADD IN THE EFFECTS OF THE DAMPING AND INERTIAL LOADS
!
      CALL rcovuo(pid,uao,Lastss)
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      IF ( Iopt<0 ) THEN
         CALL spag_block_8
         RETURN
      ENDIF
!
!     IF RECOVERING A MODAL REDUCED SUBSTRUCTURE, CALCULATE
!     THE MODAL CORRECTION TO THE U PARTIAL
!
      dua = 0
      IF ( Modal ) THEN
!
!     IF RF-9, SPLIT THE DISPLACEMENTS FROM THE TOTAL VECTOR
!
         uad = upart
         IF ( Rfno==9 ) THEN
            uad = scr9
            CALL rcovva(upart,1,0,uad,0,0,Lastss,dz(idpcor),dz(idpcor),dz(idpcor))
         ENDIF
!
!     PARTITION THE PARTIAL DISPLACEMENTS TO THE OMITTED AND
!     BOUNDARY SIZES
!
         item = uprt
         CALL mtrxi(scr2,Lastss,uprt,0,rc)
         IF ( rc/=1 ) THEN
            CALL spag_block_7
            RETURN
         ENDIF
         Rule = 0
         Mrgz = lcorez - 14
         idp = (Icore+14)/2 + 1
         Mcb(1) = uad
         CALL rdtrl(Mcb)
         CALL makmcb(Mcb11,scr3,nrowo,Rect,Mcb(5))
         CALL makmcb(Mcb21,scr4,Mcb(3)-nrowo,Rect,Mcb(5))
         Mcb11(2) = Mcb(2)
         Mcb21(2) = Mcb(2)
         Mcb12(1) = 0
         Mcb22(1) = 0
!
         Z(Icore) = scr2
         CALL rdtrl(Z(Icore))
         CALL makmcb(Z(Icore+7),0,Mcb(2),Rect,Rsp)
         Z(Icore+8) = 1
         CALL sofcls
!
         CALL bug(nhpdat,500,Mcb(1),37)
         CALL partn(Z(Icore+7),Z(Icore),dz(idp))
         CALL wrttrl(Mcb11)
         CALL wrttrl(Mcb21)
!
         CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
!
!     CALCULATE THE CORRECTION TERMS
!
!     DUO = GI*UB - UO
!
         item = gims
         CALL mtrxi(scr6,Lastss,gims,0,rc)
         IF ( rc/=1 ) THEN
            CALL spag_block_7
            RETURN
         ENDIF
         Mcba(1) = scr6
         CALL rdtrl(Mcba)
         DO i = 1 , 7
            Mcbb(i) = Mcb21(i)
            Mcbc(i) = Mcb11(i)
         ENDDO
         CALL makmcb(Mcbd,scr9,Mcba(3),Rect,Mcbb(5))
         Signab = 1
         Signc = -1
         Tflag = 0
         Scrm = 308
         Mprec = 0
         CALL sofcls
         Mpyz = Mrgz
         CALL mpyad(dz(idp),dz(idp),dz(idp))
         CALL wrttrl(Mcbd)
!
!     MERGE DUO TO -A- SIZE
!
         DO i = 1 , 7
            Mcb11(i) = Mcbd(i)
         ENDDO
         Mcb21(1) = 0
         dua = scr4
         CALL makmcb(Mcb,dua,Z(Icore+2),Rect,Mcb11(5))
         Mcb(2) = Mcbd(2)
         IF ( Rfno==9 ) Mcb(2) = 3*Mcbd(2)
!
!     SET UP A NULL ROW PARTITIONING VECTOR (OR FOR RF-9)
!     SET UP A VECTOR THAT WILL MERGE IN A NULL VELOCITY AND
!     ACCELERATION VECTOR FOR EACH DISPLACEMENT VECTOR
!
         Nro = Mcb(2)
         CALL makmcb(Z(Icore+7),scr3,Nro,Rect,Rsp)
         IF ( Nro+15>lcorez ) THEN
!
            CALL mesage(8,0,name)
            CALL spag_block_8
            RETURN
         ELSE
            DO i = 1 , Nro
               rz(Icore+14+i) = 0.0
            ENDDO
            IF ( Rfno==9 ) THEN
               DO i = 1 , Nro , 3
                  rz(Icore+15+i) = 1.0
                  rz(Icore+16+i) = 1.0
               ENDDO
            ENDIF
            CALL gopen(scr3,Z(Buf1),Wrtrew)
            Typin = 1
            Typot = 1
            Iro = 1
            Incrp = 1
            CALL pack(Z(Icore+15),scr3,Z(Icore+7))
            CALL close(scr3,Rew)
            CALL wrttrl(Z(Icore+7))
            CALL merge(Z(Icore+7),Z(Icore),dz(idp))
            CALL wrttrl(Mcb)
         ENDIF
      ENDIF
!
!     ADD THE PARTIAL DISPLACEMENT VECTOR TO THE DISPLACEMENTS FROM
!     THE OMITS, INERTIAL, DAMPING, AND MODAL CORRECTION EFFECTS
!     TO GET THE FINAL DISPLACEMENT VECTOR FOR THIS SUBSTRUCTURE
!
      Nomat = 2
      IF ( dua/=0 ) Nomat = 3
      Typa = 1
      Alpha = 1.0
      Mcbaa(1) = upart
      CALL rdtrl(Mcbaa)
      Typb = 1
      Beta = 1.0
      Mcbbb(1) = uao
      CALL rdtrl(Mcbbb)
      IF ( dua/=0 ) THEN
         Typc = 1
         Gama = 1.0
         Mcbcc(1) = dua
         CALL rdtrl(Mcbcc)
      ENDIF
      CALL makmcb(Mcbxx,Ua,Mcbaa(3),Rect,Mcbaa(5))
      Mcbxx(2) = Mcbaa(2)
      Lcor = lcorez
      CALL sofcls
      CALL sadd(dz(idpcor),dz(idpcor))
      CALL wrttrl(Mcbxx)
!
!     NORMAL RETURN
!
      Signab = 1
      RETURN
   END SUBROUTINE spag_block_6
   SUBROUTINE spag_block_7
!
!     ERROR MESSAGES
!
      IF ( rc==2 ) rc = 3
      CALL smsg(rc-2,item,Lastss)
      CALL spag_block_8
   END SUBROUTINE spag_block_7
   SUBROUTINE spag_block_8
      Iopt = -1
   END SUBROUTINE spag_block_8
END SUBROUTINE rcovui
