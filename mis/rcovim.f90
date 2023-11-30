
SUBROUTINE rcovim(Higher)
   IMPLICIT NONE
   INTEGER Buf1 , Buf2 , Buf3 , Buf4 , Fss(2) , Icore , Incrp , Incru , Iopt , Ireq , Irp , Iru , Itinp , Itinu , Itoutp , Lbasic , &
         & Lcore , Loop , Lower , Lreq , Lui , Mcba(7) , Mcbb(7) , Mcbc(7) , Mcbd(7) , Mprec , Mpyz , Mrecvr , Neigv , Norew ,      &
         & Nosort , Nrp , Nru , Rfno , Rsp , Rss(2) , Scrm , Signab , Signc , Sof1 , Sof2 , Sof3 , Tflag , Ua , Z(1)
   REAL Cdp , Csp , Diag , Dry , Energy , Eofnrw , Pa , Pthres , Qa , Qthres , Range(2) , Rd , Rdp , Rdrew , Rect , Rew , Rz(1) ,   &
      & Square , Step , Sym , Uimpro , Uinms(2,5) , Upper , Uthres , Wrt , Wrtrew
   COMMON /blank / Dry , Loop , Step , Fss , Rfno , Neigv , Lui , Uinms , Nosort , Uthres , Pthres , Qthres
   COMMON /mpyadx/ Mcba , Mcbb , Mcbc , Mcbd , Mpyz , Tflag , Signab , Signc , Mprec , Scrm
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Square , Rect , Diag , Upper , Lower ,&
                 & Sym
   COMMON /packx / Itinp , Itoutp , Irp , Nrp , Incrp
   COMMON /rcovcm/ Mrecvr , Ua , Pa , Qa , Iopt , Rss , Energy , Uimpro , Range , Ireq , Lreq , Lbasic
   COMMON /rcovcr/ Icore , Lcore , Buf1 , Buf2 , Buf3 , Buf4 , Sof1 , Sof2 , Sof3
   COMMON /unpakx/ Itinu , Iru , Nru , Incru
   COMMON /zzzzzz/ Z
   INTEGER Higher(2)
   INTEGER file , i , intyp , isk , item , ivec1 , ivec2 , j , k , kmtx , lcorez , mmtx , n , name(2) , ncol , rc , scr5 , scr6 ,   &
         & scr7 , scr8 , scr9 , uvec
   INTEGER korsz
   REAL total
!
!     THIS SUBROUTINE CALCULATES THE ENERGIES ON THE MODAL COORDINATES
!     IN A SUBSTRUCTURE THAT WAS MODAL REDUCED.  IT WILL ALSO
!     CALCULATE THE TOTAL ENERGY FOR EACH COLUMN.
!
   !>>>>EQUIVALENCE (Z(1),Rz(1))
   DATA uvec , kmtx , mmtx/4HUVEC , 4HKMTX , 4HMMTX/
   DATA scr5 , scr6 , scr7 , scr8 , scr9/305 , 306 , 307 , 308 , 309/
   DATA name/4HRCOV , 4HIM  /
!
!     INITIALIZE
!
   lcorez = korsz(Z)
   Mpyz = lcorez
   Tflag = 0
   Signab = 1
   Signc = 1
   Mprec = 0
!
!     GET THE DISPLACEMENT VECTOR FOR THE HIGHER LEVEL REDUCED
!     SUBSTRUCTURE.
!
   item = uvec
   CALL mtrxi(scr5,Higher,uvec,0,rc)
   IF ( rc/=1 ) THEN
!
!     ERRORS
!
      CALL smsg(rc-2,item,Higher)
      Iopt = -1
      GOTO 99999
   ELSE
!
!     CALCULATE VELOCITIES IF NOT ALREADY DONE FOR THE OUTPUT PHASE.
!
      intyp = 1
      IF ( Rfno==3 .OR. Rfno==8 ) intyp = 0
      CALL rcovva(scr5,intyp,0,scr8,scr9,0,Higher,Z(1),Z(1),Z(1))
      IF ( Ua<=0 ) THEN
         Iopt = -1
         GOTO 99999
      ELSE
!
!     CALCULATE THE KENETIC ENERTY MULTIPLIER - M * V
!
         item = mmtx
         CALL mtrxi(scr5,Higher,mmtx,0,rc)
         IF ( rc/=1 ) THEN
            CALL smsg(rc-2,item,Higher)
            Iopt = -1
            GOTO 99999
         ELSE
            Mcba(1) = scr5
            CALL rdtrl(Mcba)
            Mcbb(1) = scr9
            CALL rdtrl(Mcbb)
            ncol = Mcbb(2)
            Mcbc(1) = 0
            CALL makmcb(Mcbd,scr7,Mcbb(3),Rect,Mcbb(5))
            Scrm = scr6
            CALL sofcls
            CALL mpyad(Z(1),Z(1),Z(1))
            CALL wrttrl(Mcbd)
!
!     CALCULATE THE KENETIC ENERGIES BY PERFORMING THE SCALAR
!     MULTIPLY IN SINGLE PERCISION.  USE ONLY THE REAL PART IF COMPLEX
!     VECTORS.  APPEND THE TOTAL KINETIC ENERGY TO THE END OF EACH
!     COLUMN.
!
            Itinu = Rsp
            Iru = 1
            Nru = Mcbd(3)
            Incru = 1
            Itinp = Rsp
            Itoutp = Rsp
            Irp = 1
            Nrp = Nru + 1
            Incrp = 1
            ivec1 = 1
            ivec2 = ivec1 + Nru + 1
            IF ( ivec2+Nru+1>Sof3 ) THEN
               n = 8
               CALL mesage(n,file,name)
               Iopt = -1
               GOTO 99999
            ELSE
!
               file = scr9
               CALL gopen(scr7,Z(Sof1),Rdrew)
               CALL gopen(scr9,Z(Sof2),Rdrew)
               CALL gopen(scr6,Z(Sof3),Wrtrew)
               CALL makmcb(Mcba,scr6,Nrp,Rect,Rsp)
!
               DO i = 1 , ncol
                  isk = 1
                  CALL unpack(*2,scr7,Rz(ivec1))
                  isk = 0
                  CALL unpack(*2,scr9,Rz(ivec2))
!
                  total = 0.0
                  DO j = 1 , Nru
                     k = j - 1
                     Rz(ivec1+k) = Rz(ivec1+k)*Rz(ivec2+k)
                     total = total + Rz(ivec1+k)
                  ENDDO
                  Rz(ivec1+Nru) = total
                  GOTO 4
!
 2                DO j = 1 , Nrp
                     Rz(ivec1+j-1) = 0.0
                  ENDDO
                  IF ( isk/=0 ) CALL fwdrec(*100,scr9)
!
 4                CALL pack(Rz(ivec1),scr6,Mcba)
!
               ENDDO
!
               CALL close(scr7,Rew)
               CALL close(scr9,Rew)
               CALL close(scr6,Rew)
               CALL wrttrl(Mcba)
               CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
!
!     CALCULATE THE POTENTIAL ENERTY MULTPLYIER - K*U
!
               item = kmtx
               CALL mtrxi(scr5,Higher,kmtx,0,rc)
               IF ( rc/=1 ) THEN
                  CALL smsg(rc-2,item,Higher)
                  Iopt = -1
                  GOTO 99999
               ELSE
                  Mcba(1) = scr5
                  CALL rdtrl(Mcba)
                  Mcbb(1) = scr8
                  CALL rdtrl(Mcbb)
                  CALL makmcb(Mcbd,scr9,Mcbb(3),Rect,Mcbb(5))
                  Scrm = scr7
                  CALL sofcls
                  CALL mpyad(Z(1),Z(1),Z(1))
                  CALL wrttrl(Mcbd)
!
!     CALCULATE THE POTENTIAL ENERGIES BY PERFORMING THE SCALAR
!     MULTIPLY IN SINGLE PERCISION.  USE ONLY THE REAL PART IF COMPLEX
!     VECTORS.  APPEND THE TOTAL POTENTIAL ENERGY TO THE END OF EACH
!     COLUMN.
!
                  Itinu = Rsp
                  Iru = 1
                  Nru = Mcbd(3)
                  Incru = 1
                  Itinp = Rsp
                  Itoutp = Rsp
                  Irp = 1
                  Nrp = Nru + 1
                  Incrp = 1
!
                  file = scr8
                  CALL gopen(scr9,Z(Sof1),Rdrew)
                  CALL gopen(scr8,Z(Sof2),Rdrew)
                  CALL gopen(scr7,Z(Sof3),Wrtrew)
                  CALL makmcb(Mcba,scr7,Nrp,Rect,Rsp)
!
                  DO i = 1 , ncol
                     isk = 1
                     CALL unpack(*6,scr9,Rz(ivec1))
                     isk = 0
                     CALL unpack(*6,scr8,Rz(ivec2))
                     total = 0.0
                     DO j = 1 , Nru
                        k = j - 1
                        Rz(ivec1+k) = Rz(ivec1+k)*Rz(ivec2+k)
                        total = total + Rz(ivec1+k)
                     ENDDO
                     Rz(ivec1+Nru) = total
                     GOTO 8
!
 6                   DO j = 1 , Nrp
                        Rz(ivec1+j-1) = 0.0
                     ENDDO
                     IF ( isk/=0 ) CALL fwdrec(*100,scr8)
!
 8                   CALL pack(Rz(ivec1),scr7,Mcba)
!
                  ENDDO
!
                  CALL close(scr9,Rew)
                  CALL close(scr8,Rew)
                  CALL close(scr7,Rew)
                  CALL wrttrl(Mcba)
!
!     NORMAL RETURN
!
                  CALL sofopn(Z(Sof1),Z(Sof2),Z(Sof3))
                  RETURN
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
 100  n = 2
   CALL mesage(n,file,name)
   Iopt = -1
99999 RETURN
END SUBROUTINE rcovim