
SUBROUTINE rcovuo(Pid,Uao,Lastss)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alp(3) , Alpha , Bet(3) , Beta , Buf2 , Buf3 , Buf4 , Dum(36) , Energy , Pa , Pthres , Qa , Qthres , Range(2) , Rss(2) ,    &
      & Sysbuf , Uimpro , Uthres , Z(1)
   INTEGER Bmcb(7) , Buf1 , Cdp , Chlsky , Csp , Diag , Dry , Eofnrw , Fbsz , Fss(2) , Icore , Incrp , Iopt , Ireq , Iro , Iz(1) ,  &
         & Lbasic , Lcor , Lcore , Lmcb(7) , Loop , Lower , Lreq , Lui , Mcba(7) , Mcbaa(7) , Mcbbb(7) , Mcbk(7) , Mcbk11(7) ,      &
         & Mcbk12(7) , Mcbk21(7) , Mcbk22(7) , Mcbl(7) , Mcblt(7) , Mcbxx(7) , Mrecvr , Mrgz , Neigv , Nomat , Norew , Nosort ,     &
         & Nout , Nro , Power , Prec , Rd , Rdp , Rdrew , Rect , Rew , Rfno , Rsp , Rule , Scra , Scrb , Scrc , Sdcmpz , Sign ,     &
         & Sof1 , Sof2 , Sof3 , Square , Step , Sym , Typa , Typb , Typin , Typot , Ua , Uinms(2,5) , Umcb(7) , Upper , Wrt ,       &
         & Wrtrew , Xmcb(7)
   DOUBLE PRECISION Det , Deti , Dz(1) , Mindia
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Dry , Loop , Step , Fss , Rfno , Neigv , Lui , Uinms , Nosort , Uthres , Pthres , Qthres
   COMMON /fbsx  / Lmcb , Umcb , Bmcb , Xmcb , Fbsz , Prec , Sign
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Square , Rect , Diag , Upper , Lower ,&
                 & Sym
   COMMON /packx / Typin , Typot , Iro , Nro , Incrp
   COMMON /parmeg/ Mcbk , Mcbk11 , Mcbk21 , Mcbk12 , Mcbk22 , Mrgz , Rule
   COMMON /rcovcm/ Mrecvr , Ua , Pa , Qa , Iopt , Rss , Energy , Uimpro , Range , Ireq , Lreq , Lbasic
   COMMON /rcovcr/ Icore , Lcore , Buf1 , Buf2 , Buf3 , Buf4 , Sof1 , Sof2 , Sof3
   COMMON /saddx / Nomat , Lcor , Mcbaa , Typa , Alpha , Alp , Mcbbb , Typb , Beta , Bet , Dum , Mcbxx
   COMMON /sfact / Mcba , Mcbl , Mcblt , Scra , Scrb , Sdcmpz , Det , Deti , Power , Scrc , Mindia , Chlsky
   COMMON /system/ Sysbuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Pid , Uao
   INTEGER Lastss(2)
!
! Local variable declarations
!
   INTEGER file , i , idpcor , item , kmtx , lcorez , lmtx , mattyp , mcbpao(7) , n , name(2) , pao , pove , rc , scr2 , scr3 ,     &
         & scr4 , scr6 , scr7 , scr8 , scr9 , uprt
   INTEGER korsz
!
! End of declarations
!
!
!     THIS SUBROUTINE CALCULATES THE FULL SIZE DISPLACEMENT VECTOR ON
!     ANY OMITTED POINTS.  THE OPTIONAL INERTIA AND DAMPING EFFECTS
!     WILL BE INCLUDED IF REQUESTED.
!
!     FILE USAGE IS AS FOLLOWS
!
!     SCR1 AND SCR5 ARE NOT USED
!     SCR4 CONTAINS PID ON INPUT AND IS DESTROYED
!     SCR7 CONTAINS UAO OUTPUT
!     ALL OTHER SCRATCH FILES ARE USED
!
!    4                 SOLN       ,SRD        ,SWRT       ,SCHK       ,
   EQUIVALENCE (Z(1),Iz(1),Dz(1))
   DATA name/4HRCOV , 4HUO  /
   DATA pove , lmtx/4HPOVE , 4HLMTX/
   DATA uprt , kmtx/4HUPRT , 4HKMTX/
   DATA scr2 , scr3 , scr4 , scr6 , scr7 , scr8 , scr9/302 , 303 , 304 , 306 , 307 , 308 , 309/
!
!     SET UP COMMON BLOCKS
!
   lcorez = korsz(Z) - Lreq - Icore - 1
   idpcor = Icore/2 + 1
   Rule = 0
   Mcbk21(1) = 0
   Mcbk12(1) = 0
   Mcbk22(1) = 0
   Sign = 1
!
!     CALCUATE THE LOADS ON THE OMMITED POINTS
!
   pao = 0
   IF ( Rfno/=3 ) THEN
      pao = scr3
      CALL rcovsl(Lastss,pove,0,scr6,scr7,scr8,pao,Z(Icore),Z(Icore),Sof3-Icore-1,.FALSE.,Rfno)
      mcbpao(1) = pao
      CALL rdtrl(mcbpao)
   ENDIF
!
!     ADD IN OPTIONAL INERTIA AND DAMPING FORCES TO THE LOADS ON THE
!     OMMITED POINTS
!
   IF ( Pid/=0 ) THEN
      IF ( pao==0 ) THEN
!
!     NO STATIC LOADS SO THE ADD IS UNECESSARY
!
         mcbpao(1) = Pid
         CALL rdtrl(mcbpao)
      ELSE
         Nomat = 2
         Typa = 1
         Alpha = 1.0
         Mcbaa(1) = Pid
         CALL rdtrl(Mcbaa)
         Typb = 1
         Beta = 1.0
         Mcbbb(1) = pao
         CALL rdtrl(Mcbbb)
         CALL makmcb(Mcbxx,scr6,Mcbaa(3),Rect,Mcbaa(5))
         Mcbxx(2) = Mcbaa(2)
         Lcor = lcorez
         CALL sofcls
         CALL sadd(Dz(idpcor),Dz(idpcor))
         CALL wrttrl(Mcbxx)
         DO i = 1 , 7
            mcbpao(i) = Mcbxx(i)
         ENDDO
         CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      ENDIF
   ENDIF
!
   IF ( mcbpao(1)<=0 ) THEN
!
!     NO LOADS SO THE DISPLACEMENTS ARE ZERO
!
      Uao = 0
      CALL sofcls
      RETURN
   ELSE
!
!     CHECK FOR EXISTENCE OF LMTX ON THE SOF.  IF IT EXISTS
!     SKIP THE PARTN AND DECOMP
!
      CALL softrl(Lastss,lmtx,Lmcb(1))
      IF ( Lmcb(1)/=1 ) THEN
!
!     COMPUTE THE KOO PARTITION OF KMTX FOR LASTSS
!
!     COPY THE PARTITIONING VECTOR TO SCR2
!
         CALL mtrxi(scr2,Lastss,uprt,0,rc)
         item = uprt
         IF ( rc/=1 ) GOTO 200
!
!     COPY KMTX TO SCR5
!
         item = kmtx
         CALL mtrxi(scr8,Lastss,kmtx,0,rc)
         IF ( rc/=1 ) GOTO 200
         Mcbk(1) = scr8
         CALL rdtrl(Mcbk)
!
!     PARTITION KMTX INTO KOO.  STORE KOO ON SCR4.
!
         CALL sofcls
         Iz(Icore) = scr2
         CALL rdtrl(Iz(Icore))
         CALL makmcb(Mcbk11,scr9,mcbpao(3),Sym,Mcbk(5))
         Mcbk11(2) = mcbpao(3)
         Mrgz = lcorez - 7
         i = (Icore+7)/2 + 1
         CALL partn(Z(Icore),Z(Icore),Dz(i))
         CALL wrttrl(Mcbk11)
!
!     DECOMPOSE KOO
!
         DO i = 1 , 7
            Mcba(i) = Mcbk11(i)
         ENDDO
         CALL makmcb(Mcbl,scr2,Mcba(3),Lower,Mcba(5))
         Mcblt(1) = scr8
         Scra = scr3
         IF ( Scra==mcbpao(1) ) Scra = scr6
         Scrb = scr4
         IF ( Scrb==mcbpao(1) ) Scrb = scr6
         Scrc = scr7
         Sdcmpz = Mrgz
         Power = 1
         Chlsky = 0
         CALL sdcomp(*100,Dz(idpcor),Dz(idpcor),Dz(idpcor))
         CALL wrttrl(Mcbl)
!
!     FORWARD AND BACKWARD SUBSTITUTION TO SOLVE FOR UAO
!
         DO i = 1 , 7
            Lmcb(i) = Mcbl(i)
            Bmcb(i) = mcbpao(i)
         ENDDO
      ELSE
!
!     BRING IN LMTX FROM SOF AND SET UP FOR FBS DIRECTLY
!
         CALL mtrxi(scr2,Lastss,lmtx,0,rc)
         DO i = 1 , 7
            Bmcb(i) = mcbpao(i)
         ENDDO
         Lmcb(1) = scr2
         CALL sofcls
      ENDIF
      Fbsz = lcorez
      mattyp = Bmcb(5)
      CALL makmcb(Xmcb,scr8,Bmcb(3),Rect,mattyp)
      Prec = 2 - (mattyp-2*(mattyp/2))
      CALL fbs(Dz(idpcor),Dz(idpcor))
      CALL wrttrl(Xmcb)
!
!     MERGE UAO INTO THE UA SET
!
!     COPY UPRT BACK TO SCR2
!
      CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
      item = uprt
      CALL mtrxi(scr2,Lastss,uprt,0,rc)
      IF ( rc/=1 ) GOTO 200
      CALL sofcls
      Iz(Icore) = scr2
      CALL rdtrl(Iz(Icore))
!
!     SETUP MCB-S IN /PARMEG/
!
      DO i = 1 , 7
         Mcbk11(i) = Xmcb(i)
      ENDDO
      Uao = scr7
      CALL makmcb(Mcbk,Uao,Iz(Icore+2),Rect,Mcbk11(5))
      Mcbk(2) = Xmcb(2)
      IF ( Rfno==9 ) Mcbk(2) = 3*Xmcb(2)
!
!     SETUP A NULL ROW PARTITIONING VECTOR OR FOR RIGID FORMAT 9 A
!     VECTOR THAT WILL MERGE IN A NULL VELOCITY AND ACCELERATION
!     VECTOR FOR EACH DISPLACEMENT VECTOR
!
      Nro = Mcbk(2)
      CALL makmcb(Z(Icore+7),scr6,Nro,Rect,Rsp)
      IF ( Nro+15>lcorez ) THEN
!
         n = 8
         Iopt = -1
         CALL sofcls
         CALL mesage(n,file,name)
         CALL close(pao,Rew)
         CALL close(scr3,Rew)
         GOTO 99999
      ELSE
         DO i = 1 , Nro
            Z(Icore+14+i) = 0.0
         ENDDO
         IF ( Rfno==9 ) THEN
            DO i = 1 , Nro , 3
               Z(Icore+15+i) = 1.0
               Z(Icore+16+i) = 1.0
            ENDDO
         ENDIF
         CALL gopen(scr6,Z(Buf1),Wrtrew)
         Typin = 1
         Typot = 1
         Iro = 1
         Incrp = 1
         CALL pack(Z(Icore+15),scr6,Iz(Icore+7))
         CALL close(scr6,Rew)
         CALL wrttrl(Iz(Icore+7))
!
         Mrgz = lcorez - 14
         i = (Icore+14)/2 + 1
         CALL merge(Z(Icore+7),Z(Icore),Dz(i))
         CALL wrttrl(Mcbk)
!
!     NORMAL RETURN
!
         RETURN
      ENDIF
   ENDIF
!
!     ERROR PROCESSING
!
 100  WRITE (Nout,99001) Swm , Lastss
99001 FORMAT (A27,' 6311, SDCOMP DECOMPOSITION FAILED ON KOO MATRIX ','FOR SUBSTRUCTURE ',2A4)
   GOTO 300
 200  IF ( rc==2 ) rc = 3
   CALL smsg(rc-2,item,Lastss)
 300  Iopt = -1
   RETURN
99999 RETURN
END SUBROUTINE rcovuo
