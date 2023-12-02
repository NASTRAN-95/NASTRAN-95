!*==rcovui.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rcovui(Ub,Lastss,Modal)
   USE c_blank
   USE c_mpyadx
   USE c_names
   USE c_packx
   USE c_parmeg
   USE c_rcovcm
   USE c_rcovcr
   USE c_saddx
   USE c_zzzzzz
   USE iso_fortran_env
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
   lcorez = korsz(z) - lreq - icore - 1
   idpcor = icore/2 + 1
   tflag = 0
   signab = 1
   signc = 1
   mprec = 0
   scrm = 309
   reqf = .FALSE.
   IF ( Lastss(1)==fss(1) .AND. Lastss(2)==fss(2) ) reqf = .TRUE.
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
   mcba(1) = scr2
   CALL rdtrl(mcba)
   mcbb(1) = Ub
   CALL rdtrl(mcbb)
   mcbc(1) = 0
   upart = scr5
   CALL makmcb(mcbd,upart,mcba(3),rect,mcbb(5))
   mpyz = lcorez
   CALL sofcls
   CALL mpyad(dz(idpcor),dz(idpcor),dz(idpcor))
   CALL wrttrl(mcbd)
   CALL sofopn(z(sof1),z(sof2),z(sof3))
!
!     DETERMINE THE NUMBER OF OMITTED POINTS
!
   nrowo = mcba(3) - mcba(2)
   CALL softrl(Lastss,gims,mcba)
   IF ( mcba(1)==1 ) nrowo = mcba(3)
!
!     GENERATE THE VELOCITIES AND ACCELERATIONS
!
   lcore = buf4 - icore - 1
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
      mcba(1) = mgg
      IF ( mcba(1)>0 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
   ENDIF
   CALL mtrxi(scr2,Lastss,mmtx,0,rc)
   IF ( rc/=1 ) THEN
      CALL spag_block_2
      RETURN
   ENDIF
   mcba(1) = scr2
   CALL rdtrl(mcba)
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      mcbb(1) = Scr8
      CALL rdtrl(mcbb)
      mcbc(1) = 0
      CALL makmcb(Mcbd,Scr6,mcbb(3),Rect,mcbb(5))
      Signab = -1
      CALL sofcls
!
      CALL mpyad(Dz(Idpcor),Dz(Idpcor),Dz(Idpcor))
!
      DO I = 1 , 7
         mcbc(I) = Mcbd(I)
      ENDDO
      Pid = Scr6
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     CALCULATE THE DAMPING LOADS
!
      IF ( rfno==3 ) THEN
         CALL spag_block_6
         RETURN
      ENDIF
      IF ( Reqf ) THEN
         mcba(1) = K4gg
         CALL rdtrl(mcba)
         IF ( mcba(1)>0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
      ENDIF
      CALL mtrxi(Scr2,Lastss,K4mx,0,Rc)
      IF ( Rc/=1 ) THEN
         CALL spag_block_4
         RETURN
      ENDIF
      mcba(1) = Scr2
      CALL rdtrl(mcba)
      CALL spag_block_3
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
      mcbb(1) = Scr7
      CALL rdtrl(mcbb)
      CALL makmcb(Mcbd,Scr8,mcbb(3),Rect,mcbb(5))
      Signab = -1
      CALL sofcls
      CALL mpyad(Dz(Idpcor),Dz(Idpcor),Dz(Idpcor))
      Pid = Scr8
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      DO I = 1 , 7
         mcbc(I) = Mcbd(I)
      ENDDO
      CALL spag_block_4
   END SUBROUTINE spag_block_3
   SUBROUTINE spag_block_4
!
      IF ( Reqf ) THEN
         mcba(1) = Bgg
         CALL rdtrl(mcba)
         IF ( mcba(1)>0 ) THEN
            CALL spag_block_5
            RETURN
         ENDIF
      ENDIF
      CALL mtrxi(Scr2,Lastss,Bmtx,0,Rc)
      IF ( Rc/=1 ) THEN
         CALL spag_block_6
         RETURN
      ENDIF
      mcba(1) = Scr2
      CALL rdtrl(mcba)
      CALL spag_block_5
   END SUBROUTINE spag_block_4
   SUBROUTINE spag_block_5
      mcbb(1) = Scr7
      CALL rdtrl(mcbb)
      CALL makmcb(Mcbd,Scr6,mcbb(3),Rect,mcbb(5))
      Signab = -1
      CALL sofcls
      CALL mpyad(Dz(Idpcor),Dz(Idpcor),Dz(Idpcor))
      Pid = Scr6
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      CALL spag_block_6
   END SUBROUTINE spag_block_5
   SUBROUTINE spag_block_6
!
!     PARTITION THE INERTIA AND DAMPING LOADS TO THE OMIT SET
!
!     GET THE PARTITIONING VECTOR FROM THE SOF
!
      IF ( Pid/=0 ) THEN
         Item = Uprt
         CALL mtrxi(Scr2,Lastss,Uprt,0,Rc)
         IF ( Rc/=1 ) THEN
            CALL spag_block_7
            RETURN
         ENDIF
         rule = 0
         mrgz = Lcorez - 14
         Idp = (Icore+14)/2 + 1
         DO I = 1 , 7
            mcb(I) = Mcbd(I)
         ENDDO
         Pid = Scr4
         CALL makmcb(mcb11,Pid,Nrowo,Rect,Mcbd(5))
         mcb11(2) = Mcbd(2)
         mcb12(1) = 0
         mcb21(1) = 0
         mcb22(1) = 0
!
!     SET UP A NULL ROW PARTITION VECTOR
!
         Z(Icore) = Scr2
         CALL rdtrl(Z(Icore))
         CALL makmcb(Z(Icore+7),0,mcb(2),Rect,rsp)
         Z(Icore+8) = 1
         CALL sofcls
         CALL partn(Z(Icore+7),Z(Icore),Dz(Idp))
         CALL wrttrl(mcb11)
         CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      ENDIF
!
!     PERFORM THE FBS TO GET THE LOADS ON THE OMMITTED POINTS.  WE
!     WILL ALSO ADD IN THE EFFECTS OF THE DAMPING AND INERTIAL LOADS
!
      CALL rcovuo(Pid,Uao,Lastss)
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      IF ( iopt<0 ) THEN
         CALL spag_block_8
         RETURN
      ENDIF
!
!     IF RECOVERING A MODAL REDUCED SUBSTRUCTURE, CALCULATE
!     THE MODAL CORRECTION TO THE U PARTIAL
!
      Dua = 0
      IF ( Modal ) THEN
!
!     IF RF-9, SPLIT THE DISPLACEMENTS FROM THE TOTAL VECTOR
!
         Uad = Upart
         IF ( rfno==9 ) THEN
            Uad = Scr9
            CALL rcovva(Upart,1,0,Uad,0,0,Lastss,Dz(Idpcor),Dz(Idpcor),Dz(Idpcor))
         ENDIF
!
!     PARTITION THE PARTIAL DISPLACEMENTS TO THE OMITTED AND
!     BOUNDARY SIZES
!
         Item = Uprt
         CALL mtrxi(Scr2,Lastss,Uprt,0,Rc)
         IF ( Rc/=1 ) THEN
            CALL spag_block_7
            RETURN
         ENDIF
         rule = 0
         mrgz = Lcorez - 14
         Idp = (Icore+14)/2 + 1
         mcb(1) = Uad
         CALL rdtrl(mcb)
         CALL makmcb(mcb11,Scr3,Nrowo,Rect,mcb(5))
         CALL makmcb(mcb21,Scr4,mcb(3)-Nrowo,Rect,mcb(5))
         mcb11(2) = mcb(2)
         mcb21(2) = mcb(2)
         mcb12(1) = 0
         mcb22(1) = 0
!
         Z(Icore) = Scr2
         CALL rdtrl(Z(Icore))
         CALL makmcb(Z(Icore+7),0,mcb(2),Rect,rsp)
         Z(Icore+8) = 1
         CALL sofcls
!
         CALL bug(Nhpdat,500,mcb(1),37)
         CALL partn(Z(Icore+7),Z(Icore),Dz(Idp))
         CALL wrttrl(mcb11)
         CALL wrttrl(mcb21)
!
         CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
!
!     CALCULATE THE CORRECTION TERMS
!
!     DUO = GI*UB - UO
!
         Item = Gims
         CALL mtrxi(Scr6,Lastss,Gims,0,Rc)
         IF ( Rc/=1 ) THEN
            CALL spag_block_7
            RETURN
         ENDIF
         mcba(1) = Scr6
         CALL rdtrl(mcba)
         DO I = 1 , 7
            mcbb(I) = mcb21(I)
            mcbc(I) = mcb11(I)
         ENDDO
         CALL makmcb(Mcbd,Scr9,mcba(3),Rect,mcbb(5))
         Signab = 1
         Signc = -1
         Tflag = 0
         Scrm = 308
         Mprec = 0
         CALL sofcls
         Mpyz = mrgz
         CALL mpyad(Dz(Idp),Dz(Idp),Dz(Idp))
         CALL wrttrl(Mcbd)
!
!     MERGE DUO TO -A- SIZE
!
         DO I = 1 , 7
            mcb11(I) = Mcbd(I)
         ENDDO
         mcb21(1) = 0
         Dua = Scr4
         CALL makmcb(mcb,Dua,Z(Icore+2),Rect,mcb11(5))
         mcb(2) = Mcbd(2)
         IF ( rfno==9 ) mcb(2) = 3*Mcbd(2)
!
!     SET UP A NULL ROW PARTITIONING VECTOR (OR FOR RF-9)
!     SET UP A VECTOR THAT WILL MERGE IN A NULL VELOCITY AND
!     ACCELERATION VECTOR FOR EACH DISPLACEMENT VECTOR
!
         nro = mcb(2)
         CALL makmcb(Z(Icore+7),Scr3,nro,Rect,rsp)
         IF ( nro+15>Lcorez ) THEN
!
            CALL mesage(8,0,Name)
            CALL spag_block_8
            RETURN
         ELSE
            DO I = 1 , nro
               Rz(Icore+14+I) = 0.0
            ENDDO
            IF ( rfno==9 ) THEN
               DO I = 1 , nro , 3
                  Rz(Icore+15+I) = 1.0
                  Rz(Icore+16+I) = 1.0
               ENDDO
            ENDIF
            CALL gopen(Scr3,Z(buf1),wrtrew)
            typin = 1
            typot = 1
            iro = 1
            incrp = 1
            CALL pack(Z(Icore+15),Scr3,Z(Icore+7))
            CALL close(Scr3,rew)
            CALL wrttrl(Z(Icore+7))
            CALL merge(Z(Icore+7),Z(Icore),Dz(Idp))
            CALL wrttrl(mcb)
         ENDIF
      ENDIF
!
!     ADD THE PARTIAL DISPLACEMENT VECTOR TO THE DISPLACEMENTS FROM
!     THE OMITS, INERTIAL, DAMPING, AND MODAL CORRECTION EFFECTS
!     TO GET THE FINAL DISPLACEMENT VECTOR FOR THIS SUBSTRUCTURE
!
      nomat = 2
      IF ( Dua/=0 ) nomat = 3
      typa = 1
      alpha = 1.0
      mcbaa(1) = Upart
      CALL rdtrl(mcbaa)
      typb = 1
      beta = 1.0
      mcbbb(1) = Uao
      CALL rdtrl(mcbbb)
      IF ( Dua/=0 ) THEN
         typc = 1
         gama = 1.0
         mcbcc(1) = Dua
         CALL rdtrl(mcbcc)
      ENDIF
      CALL makmcb(mcbxx,ua,mcbaa(3),Rect,mcbaa(5))
      mcbxx(2) = mcbaa(2)
      lcor = Lcorez
      CALL sofcls
      CALL sadd(Dz(Idpcor),Dz(Idpcor))
      CALL wrttrl(mcbxx)
!
!     NORMAL RETURN
!
      Signab = 1
   END SUBROUTINE spag_block_6
   SUBROUTINE spag_block_7
!
!     ERROR MESSAGES
!
      IF ( Rc==2 ) Rc = 3
      CALL smsg(Rc-2,Item,Lastss)
      CALL spag_block_8
   END SUBROUTINE spag_block_7
   SUBROUTINE spag_block_8
      iopt = -1
   END SUBROUTINE spag_block_8
END SUBROUTINE rcovui
