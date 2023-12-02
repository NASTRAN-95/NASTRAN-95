!*==genpar.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE genpar
   USE c_blank
   USE c_packx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , i , l1 , lcore , ndir
   INTEGER , SAVE :: cplamb , cpmp , pf , rplamb , rppf
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL close , gopen , korsz , mesage , pack , rdtrl , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     GENERATES PARTITIONING VECTORS FOR DDAM SO THAT ONLY THE FIRST
!     LMODES MODES WILL BE USED, NOT NECESSARILY ALL THE ONES FOUND ON
!     THE PREVIOUS EIGENVALUE RUN. LMODES MUST BE GREATER THAN ZERO.
!     IF LMODES IS GREATER THAN THE NUMBER FOUND(OBTAINED FROM PF), IT
!     IS REDUCED TO THE NUMBER PREVIOUSLY FOUND
!
!     GENPART  PF/RPLAMB,CPLAMB,RPPF,CPMP/C,Y,LMODES/V,N,NMODES $
!     SAVE NMODES $
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA pf , rplamb , cplamb , rppf , cpmp/101 , 201 , 202 , 203 , 204/
   DATA nam/4HGENP , 4HART /
!
   lcore = korsz(z)
   buf1 = lcore - sysbuf + 1
   lcore = buf1 - 1
   IF ( lcore<5 ) THEN
!
      CALL mesage(-8,0,nam)
   ELSE
!
      in = 1
      iout = 1
      ii = 1
      incr = 1
!
      IF ( lmodes<=0 ) THEN
         WRITE (otpe,99001) ufm
99001    FORMAT (A23,', LMODES PARAMETER MUST POSITIVE')
         CALL mesage(-61,0,0)
         CALL mesage(-8,0,nam)
      ELSE
         mcb(1) = pf
         CALL rdtrl(mcb)
         nmodes = mcb(3)
         ndir = mcb(2)
         IF ( lmodes>nmodes ) lmodes = nmodes
!
!     GENERATE ROW PARTITIONING VECTOR FOR LAMB MATRIX TO PICK OFF THE
!     2ND COLUMN, WHICH IS THE COLUMN OF RADIAN FREQUENCIES. THEN
!     TRUNCATE THE COLUMN TO LMODES SIZE
!
         IF ( lcore<nmodes ) THEN
            CALL mesage(-8,0,nam)
         ELSE
            CALL gopen(cplamb,z(buf1),1)
            nn = 0
            z(1) = 0.
            z(nn+2) = 1.
            z(nn+3) = 0.
            z(nn+4) = 0.
            z(nn+5) = 0.
            nn = 5
            mcb(1) = cplamb
            mcb(2) = 0
            mcb(3) = 5
            mcb(4) = 2
            mcb(5) = 1
            mcb(6) = 0
            mcb(7) = 0
            CALL pack(z,cplamb,mcb)
            CALL close(cplamb,1)
            CALL wrttrl(mcb)
!
            CALL gopen(rplamb,z(buf1),1)
            DO i = 1 , lmodes
               z(i) = 1.
            ENDDO
            IF ( lmodes/=nmodes ) THEN
               l1 = lmodes + 1
               DO i = l1 , nmodes
                  z(i) = 0.
               ENDDO
            ENDIF
            nn = nmodes
            mcb(1) = rplamb
            mcb(2) = 0
            mcb(3) = nmodes
            mcb(4) = 2
            mcb(5) = 1
            mcb(6) = 0
            mcb(7) = 0
            CALL pack(z,rplamb,mcb)
            CALL close(rplamb,1)
            CALL wrttrl(mcb)
!
!     ROW PARTITION FOR PF
!
            CALL gopen(rppf,z(buf1),1)
            mcb(1) = rppf
            mcb(2) = 0
            mcb(6) = 0
            mcb(7) = 0
            CALL pack(z,rppf,mcb)
            CALL close(rppf,1)
            CALL wrttrl(mcb)
!
!     COLUMN PARTITION FOR MP-SAME AS ROW PARTITION FOR PREVIOUS FILES
!
            CALL gopen(cpmp,z(buf1),1)
            mcb(1) = cpmp
            mcb(2) = 0
            mcb(6) = 0
            mcb(7) = 0
            CALL pack(z,cpmp,mcb)
            CALL close(cpmp,1)
            CALL wrttrl(mcb)
!
            RETURN
         ENDIF
      ENDIF
   ENDIF
END SUBROUTINE genpar
