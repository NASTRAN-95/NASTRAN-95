!*==frd2i.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE frd2i(Fl,Nfreq,Ncore,Qhhl,Scr2,Scr1,Scr3,Scr4,Nrow)
   USE c_blank
   USE c_condas
   USE c_packx
   USE c_system
   USE c_type
   USE c_unpakx
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Fl
   INTEGER :: Nfreq
   INTEGER :: Ncore
   INTEGER :: Qhhl
   INTEGER :: Scr2
   INTEGER :: Scr1
   INTEGER :: Scr3
   INTEGER :: Scr4
   INTEGER :: Nrow
!
! Local variable declarations rewritten by SPAG
!
   REAL :: flag , rmi , rms , rmx , xm
   INTEGER :: i , ibuf1 , ibuf2 , icore , icp , ik , im , indx , ipd , ipi , iscr , j , ji , jj , k , kk , kkk , n , nc , ni , nl , &
            & nloop , nogo , nwc
   INTEGER , DIMENSION(7) :: mcb , trl
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , SAVE :: nhfrdi
   EXTERNAL bug , close , dmpfil , gopen , makmcb , mesage , mintrp , open , pack , rdtrl , read , skprec , unpack , wrttrl , zeroc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
   DATA name/4HFRD2 , 4HI   /
   DATA nhfrdi/4HFRDI/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ibuf1 = Ncore - isys
         ibuf2 = ibuf1 - isys
         Nrow = 0
         incr = 1
         incr1 = 1
         ii = 1
         inn = 1
         mcb(1) = Qhhl
         CALL rdtrl(mcb)
         IF ( mcb(1)<0 ) GOTO 40
         Nrow = mcb(3)
         ni = (mcb(2)/mcb(3))*2
         nnn = Nrow
         nn = Nrow*Nrow
         iti = 3
         ito = iti
         iout = iti
         nwc = iwc(iti)
         iscr = Scr1
         nloop = 1
         indx = 0
         xm = rm
         IF ( rm<0.0 ) THEN
            iscr = Scr2
            nloop = Nfreq
            indx = 1
         ENDIF
         CALL makmcb(trl,iscr,nn,mcb(4),ito)
!
!     MAKE INDEPENDENT FREQ LIST
!
         ipd = 1
         nl = 2*Nfreq
         n = Nfreq + 1
         icore = ibuf1
         ipi = ipd + nl
         DO i = 1 , Nfreq
            Fl(nl) = Fl(n-i)*twopi*bov
            Fl(nl-1) = 0.0
            nl = nl - 2
         ENDDO
!
!     MAKE INDEPENDENT FREQ LIST
!
         CALL open(*40,Qhhl,Fl(ibuf2),0)
         CALL gopen(iscr,Fl(ibuf1),1)
         CALL read(*20,*20,Qhhl,Fl(ipi),-3,0,flag)
         CALL read(*20,*20,Qhhl,n,1,0,flag)
         n = n + n
         IF ( rm<0.0 .AND. n/=ni ) THEN
            WRITE (out,99001) ufm , n , ni
99001       FORMAT (A23,', THE NUMBER OF (M,K) PAIRS SPECIFIED ON MKAEROX ','CARDS (',I5,') IS NOT EQUAL ',/5X,                     &
                   &'TO THE NUMBER OF FREQUENCIES SPECIFIED (',I5,'),')
            CALL mesage(-37,0,name)
         ENDIF
         ni = min0(ni,n)
         CALL read(*20,*20,Qhhl,Fl(ipi),ni,1,flag)
         IF ( rm<0.0 ) CALL close(Qhhl,1)
!
         DO kkk = 1 , nloop
            IF ( rm<0.0 ) THEN
               xm = Fl(2*kkk)
               CALL gopen(Qhhl,Fl(ibuf2),0)
            ENDIF
!
!     FOR RM.GE.0.0, FIND M CLOSEST TO XM
!     FOR RM.LT.0.0, FIND K CLOSEST TO XM
!
            icp = ipi + ni
            rmi = 1.E20
            rms = 0.0
            DO i = 1 , ni , 2
               rmx = abs(Fl(ipi+i+indx-1)-xm)
               rmi = amin1(rmi,rmx)
               IF ( rmx<=rmi ) rms = Fl(ipi+i+indx-1)
            ENDDO
            rmi = rms
!
!     FOR RM.GE.0.0, SELECT ALL K'S ASSOCIATED WITH RMI
!     FOR RM.LT.0.0, SELECT THE K EQUAL TO RMI
!
            k = 0
            SPAG_Loop_2_1: DO i = 1 , ni , 2
               IF ( Fl(ipi+i+indx-1)==rmi ) THEN
!
!     MAKE MATRIX INTO COLUMN
!
                  Fl(ipi+k+1) = Fl(ipi+i)
                  k = k + 2
                  ji = icp
                  n = Nrow*nwc
                  DO j = 1 , Nrow
                     spag_nextblock_2 = 1
                     SPAG_DispatchLoop_2: DO
                        SELECT CASE (spag_nextblock_2)
                        CASE (1)
                           CALL unpack(*2,Qhhl,Fl(ji))
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
 2                         CALL zeroc(Fl(ji),n)
                           spag_nextblock_2 = 2
                        CASE (2)
                           ji = ji + n
                           EXIT SPAG_DispatchLoop_2
                        END SELECT
                     ENDDO SPAG_DispatchLoop_2
                  ENDDO
!
!     DIVIDE IMAG PART OF QHHL BY FREQUENCY
!
                  jj = icp + 1
                  kk = ji - 1
                  DO j = jj , kk , 2
                     Fl(j) = Fl(j)/Fl(ipi+i)
                  ENDDO
                  IF ( rm<0.0 ) Fl(ipi+i) = -10000.0
                  CALL pack(Fl(icp),iscr,trl)
                  IF ( rm<0.0 ) EXIT SPAG_Loop_2_1
               ELSE
!
!     SKIP MATRIX
!
                  CALL skprec(Qhhl,Nrow)
               ENDIF
            ENDDO SPAG_Loop_2_1
            CALL close(Qhhl,1)
            CALL close(iscr,1)
         ENDDO
!
         CALL wrttrl(trl)
         CALL bug(nhfrdi,200,k,1)
         CALL bug(nhfrdi,200,Nfreq,1)
         CALL bug(nhfrdi,200,Fl(1),icp)
         IF ( rm<0.0 ) RETURN
!
!     SETUP TO CALL MINTRP
!
         ni = k/2
         nogo = 0
         nc = Ncore - icp
         CALL dmpfil(-Scr1,Fl(icp),nc)
         im = 0
         ik = 1
         CALL mintrp(ni,Fl(ipi),Nfreq,Fl(ipd),-1,im,ik,0.0,Scr1,Scr2,Scr3,Scr4,Fl(icp),nc,nogo,iprec)
         IF ( nogo==1 ) THEN
!
            WRITE (out,99002) ufm
99002       FORMAT (A23,' 2271, INTERPOLATION MATRIX IS SINGULAR')
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL dmpfil(-Scr2,Fl(icp),nc)
            RETURN
         ENDIF
 20      CALL mesage(-3,Qhhl,name)
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(-61,0,name)
 40      CALL close(Qhhl,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE frd2i
