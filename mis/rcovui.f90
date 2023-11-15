
SUBROUTINE rcovui(Ub,Lastss,Modal)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alp(3) , Alpha , Bet(3) , Beta , Cdp , Csp , Diag , Dum(24) , Energy , Eofnrw , Gam(3) , Gama , Pa , Pthres , Qa , Qthres , &
      & Range(2) , Rd , Rdp , Rdrew , Rect , Rew , Rsp , Rz(1) , Square , Step , Sym , Uimpro , Uinms(2,5) , Upper , Uthres , Wrt , &
      & Wrtrew
   INTEGER Buf1 , Buf2 , Buf3 , Buf4 , Dry , Fss(2) , Icore , Incrp , Iopt , Ireq , Iro , Lbasic , Lcor , Lcore , Loop , Lower ,    &
         & Lreq , Lui , Mcb(7) , Mcb11(7) , Mcb12(7) , Mcb21(7) , Mcb22(7) , Mcba(7) , Mcbaa(7) , Mcbb(7) , Mcbbb(7) , Mcbc(7) ,    &
         & Mcbcc(7) , Mcbd(7) , Mcbxx(7) , Mprec , Mpyz , Mrecvr , Mrgz , Neigv , Nomat , Norew , Nosort , Nro , Rfno , Rss(2) ,    &
         & Rule , Scrm , Signab , Signc , Sof1 , Sof2 , Sof3 , Tflag , Typa , Typb , Typc , Typin , Typot , Ua , Z(1)
   DOUBLE PRECISION Dz(1)
   COMMON /blank / Dry , Loop , Step , Fss , Rfno , Neigv , Lui , Uinms , Nosort , Uthres , Pthres , Qthres
   COMMON /mpyadx/ Mcba , Mcbb , Mcbc , Mcbd , Mpyz , Tflag , Signab , Signc , Mprec , Scrm
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Square , Rect , Diag , Upper , Lower ,&
                 & Sym
   COMMON /packx / Typin , Typot , Iro , Nro , Incrp
   COMMON /parmeg/ Mcb , Mcb11 , Mcb21 , Mcb12 , Mcb22 , Mrgz , Rule
   COMMON /rcovcm/ Mrecvr , Ua , Pa , Qa , Iopt , Rss , Energy , Uimpro , Range , Ireq , Lreq , Lbasic
   COMMON /rcovcr/ Icore , Lcore , Buf1 , Buf2 , Buf3 , Buf4 , Sof1 , Sof2 , Sof3
   COMMON /saddx / Nomat , Lcor , Mcbaa , Typa , Alpha , Alp , Mcbbb , Typb , Beta , Bet , Mcbcc , Typc , Gama , Gam , Dum , Mcbxx
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   LOGICAL Modal
   INTEGER Ub
   INTEGER Lastss(2)
!
! Local variable declarations
!
   INTEGER bgg , bmtx , dua , gims , horg , i , idp , idpcor , item , k4gg , k4mx , lcorez , mgg , mmtx , name(2) , nhpdat , nrowo ,&
         & pid , rc , scr2 , scr3 , scr4 , scr5 , scr6 , scr7 , scr8 , scr9 , uad , uao , upart , uprt
   INTEGER korsz
   LOGICAL reqf
!
! End of declarations
!
!
!     THIS ROUTINE CALCULATES THE IMPROVED LOWER LEVEL DISPLACEMENTS
!     ON A REDUCED SUBSTRUCTURE WHICH INCLUDE INERTIA AND DAMPING
!     EFFECTS
!
   EQUIVALENCE (Dz(1),Rz(1),Z(1))
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
   IF ( rc/=1 ) GOTO 700
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
   CALL mpyad(Dz(idpcor),Dz(idpcor),Dz(idpcor))
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
   CALL rcovva(upart,0,0,0,scr7,scr8,Lastss,Dz(idpcor),Dz(idpcor),Dz(idpcor))
   IF ( upart<=0 ) GOTO 800
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
      IF ( Mcba(1)>0 ) GOTO 100
   ENDIF
   CALL mtrxi(scr2,Lastss,mmtx,0,rc)
   IF ( rc/=1 ) GOTO 200
   Mcba(1) = scr2
   CALL rdtrl(Mcba)
 100  Mcbb(1) = scr8
   CALL rdtrl(Mcbb)
   Mcbc(1) = 0
   CALL makmcb(Mcbd,scr6,Mcbb(3),Rect,Mcbb(5))
   Signab = -1
   CALL sofcls
!
   CALL mpyad(Dz(idpcor),Dz(idpcor),Dz(idpcor))
!
   DO i = 1 , 7
      Mcbc(i) = Mcbd(i)
   ENDDO
   pid = scr6
   CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
!
!     CALCULATE THE DAMPING LOADS
!
 200  IF ( Rfno==3 ) GOTO 600
   IF ( reqf ) THEN
      Mcba(1) = k4gg
      CALL rdtrl(Mcba)
      IF ( Mcba(1)>0 ) GOTO 300
   ENDIF
   CALL mtrxi(scr2,Lastss,k4mx,0,rc)
   IF ( rc/=1 ) GOTO 400
   Mcba(1) = scr2
   CALL rdtrl(Mcba)
 300  Mcbb(1) = scr7
   CALL rdtrl(Mcbb)
   CALL makmcb(Mcbd,scr8,Mcbb(3),Rect,Mcbb(5))
   Signab = -1
   CALL sofcls
   CALL mpyad(Dz(idpcor),Dz(idpcor),Dz(idpcor))
   pid = scr8
   CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
   DO i = 1 , 7
      Mcbc(i) = Mcbd(i)
   ENDDO
!
 400  IF ( reqf ) THEN
      Mcba(1) = bgg
      CALL rdtrl(Mcba)
      IF ( Mcba(1)>0 ) GOTO 500
   ENDIF
   CALL mtrxi(scr2,Lastss,bmtx,0,rc)
   IF ( rc/=1 ) GOTO 600
   Mcba(1) = scr2
   CALL rdtrl(Mcba)
 500  Mcbb(1) = scr7
   CALL rdtrl(Mcbb)
   CALL makmcb(Mcbd,scr6,Mcbb(3),Rect,Mcbb(5))
   Signab = -1
   CALL sofcls
   CALL mpyad(Dz(idpcor),Dz(idpcor),Dz(idpcor))
   pid = scr6
   CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
!
!     PARTITION THE INERTIA AND DAMPING LOADS TO THE OMIT SET
!
!     GET THE PARTITIONING VECTOR FROM THE SOF
!
 600  IF ( pid/=0 ) THEN
      item = uprt
      CALL mtrxi(scr2,Lastss,uprt,0,rc)
      IF ( rc/=1 ) GOTO 700
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
      CALL partn(Z(Icore+7),Z(Icore),Dz(idp))
      CALL wrttrl(Mcb11)
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
   ENDIF
!
!     PERFORM THE FBS TO GET THE LOADS ON THE OMMITTED POINTS.  WE
!     WILL ALSO ADD IN THE EFFECTS OF THE DAMPING AND INERTIAL LOADS
!
   CALL rcovuo(pid,uao,Lastss)
   CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
   IF ( Iopt<0 ) GOTO 800
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
         CALL rcovva(upart,1,0,uad,0,0,Lastss,Dz(idpcor),Dz(idpcor),Dz(idpcor))
      ENDIF
!
!     PARTITION THE PARTIAL DISPLACEMENTS TO THE OMITTED AND
!     BOUNDARY SIZES
!
      item = uprt
      CALL mtrxi(scr2,Lastss,uprt,0,rc)
      IF ( rc/=1 ) GOTO 700
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
      CALL partn(Z(Icore+7),Z(Icore),Dz(idp))
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
      IF ( rc/=1 ) GOTO 700
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
      CALL mpyad(Dz(idp),Dz(idp),Dz(idp))
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
         GOTO 800
      ELSE
         DO i = 1 , Nro
            Rz(Icore+14+i) = 0.0
         ENDDO
         IF ( Rfno==9 ) THEN
            DO i = 1 , Nro , 3
               Rz(Icore+15+i) = 1.0
               Rz(Icore+16+i) = 1.0
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
         CALL merge(Z(Icore+7),Z(Icore),Dz(idp))
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
   CALL sadd(Dz(idpcor),Dz(idpcor))
   CALL wrttrl(Mcbxx)
!
!     NORMAL RETURN
!
   Signab = 1
   RETURN
!
!     ERROR MESSAGES
!
 700  IF ( rc==2 ) rc = 3
   CALL smsg(rc-2,item,Lastss)
 800  Iopt = -1
   RETURN
END SUBROUTINE rcovui
