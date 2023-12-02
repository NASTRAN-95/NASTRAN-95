!*==adri.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE adri(Fl,Nfreq,Ncore,Qhhl,Scr2,Scr1,Scr3,Scr4,Nrow,Ncol,Nogo)
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
   INTEGER :: Ncol
   INTEGER :: Nogo
!
! Local variable declarations rewritten by SPAG
!
   REAL :: flag , rmi , rms , rmx
   INTEGER :: i , ibuf1 , ibuf2 , icp , ik , im , ipd , ipi , j , ji , jj , k , kk , n , nc , ni , nl , nwc
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
   DATA nhfrdi , name/4HFRDI , 4HADRI , 4H    /
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
         CALL open(*40,Qhhl,Fl(ibuf2),0)
         CALL gopen(Scr1,Fl(ibuf1),1)
         CALL read(*20,*20,Qhhl,Fl(1),-2,0,flag)
         CALL read(*20,*20,Qhhl,Ncol,1,0,flag)
         CALL read(*20,*20,Qhhl,n,1,0,flag)
         n = n + n
         ni = (mcb(2)/Ncol)*2
         ni = min0(ni,n)
         nnn = Nrow
         nn = Ncol*Nrow
         iti = 3
         ito = iti
         iout = iti
         nwc = iwc(iti)
         CALL makmcb(trl,Scr1,nn,mcb(4),ito)
!
!     MAKE   DEPENDENT FREQ LIST
!
         ipd = 1
         nl = 2*Nfreq
         n = Nfreq + 1
         ipi = ipd + nl
         DO i = 1 , Nfreq
            Fl(nl) = Fl(n-i)*twopi*bov
            Fl(nl-1) = 0.0
            nl = nl - 2
         ENDDO
!
!     MAKE INDEPENDENT FREQ LIST
!
         CALL read(*20,*20,Qhhl,Fl(ipi),ni,1,flag)
!
!     FIND M"S CLOSEST TO RM
!
         icp = ipi + ni
         rmi = 1.E20
         rms = 0.0
         DO i = 1 , ni , 2
            rmx = abs(Fl(ipi+i-1)-rm)
            rmi = amin1(rmi,rmx)
            IF ( rmx<=rmi ) rms = Fl(ipi+i-1)
         ENDDO
         rmi = rms
!
!     DO ALL K"S ASSOCIATED WITH RMI
!
         k = 0
         DO i = 1 , ni , 2
            IF ( Fl(ipi+i-1)==rmi ) THEN
!
!     MAKE MATRIX INTO COLUMN
!
               Fl(ipi+k+1) = Fl(ipi+i)
               k = k + 2
               ji = icp
               n = Nrow*nwc
               DO j = 1 , Ncol
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        CALL unpack(*2,Qhhl,Fl(ji))
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
 2                      CALL zeroc(Fl(ji),n)
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
               CALL pack(Fl(icp),Scr1,trl)
            ELSE
!
!     SKIP MATRIX
!
               CALL skprec(Qhhl,Ncol)
            ENDIF
         ENDDO
         CALL close(Qhhl,1)
         CALL close(Scr1,1)
         CALL wrttrl(trl)
         CALL bug(nhfrdi,150,k,1)
         CALL bug(nhfrdi,150,Nfreq,1)
         CALL bug(nhfrdi,150,Fl(1),icp)
!
!     SETUP TO CALL MINTRP
!
         ni = k/2
         Nogo = 0
         nc = Ncore - icp
         CALL dmpfil(-Scr1,Fl(icp),nc)
         im = 0
         ik = 1
         CALL mintrp(ni,Fl(ipi),Nfreq,Fl(ipd),-1,im,ik,0.0,Scr1,Scr2,Scr3,Scr4,Fl(icp),nc,Nogo,iprec)
         IF ( Nogo==1 ) THEN
!
            WRITE (out,99001) ufm
!IBMR 6/93  GO TO 240                                                 !*
99001       FORMAT (A23,' 2271, INTERPOLATION MATRIX IS SINGULAR')
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            CALL dmpfil(-Scr2,Fl(icp),nc)
            RETURN
         ENDIF
 20      CALL mesage(3,Qhhl,name)
         spag_nextblock_1 = 2
      CASE (2)
         Nogo = 1
 40      CALL close(Qhhl,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE adri
