
SUBROUTINE genpar
   IMPLICIT NONE
   INTEGER Ii , In , Incr , Iout , Iz(1) , Lmodes , Nmodes , Nn , Otpe , Sysbuf
   CHARACTER*23 Ufm
   REAL Z(1)
   COMMON /blank / Lmodes , Nmodes
   COMMON /packx / In , Iout , Ii , Nn , Incr
   COMMON /system/ Sysbuf , Otpe
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   INTEGER buf1 , cplamb , cpmp , i , l1 , lcore , mcb(7) , nam(2) , ndir , pf , rplamb , rppf
   INTEGER korsz
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
   EQUIVALENCE (Z(1),Iz(1))
   DATA pf , rplamb , cplamb , rppf , cpmp/101 , 201 , 202 , 203 , 204/
   DATA nam/4HGENP , 4HART /
!
   lcore = korsz(Z)
   buf1 = lcore - Sysbuf + 1
   lcore = buf1 - 1
   IF ( lcore<5 ) THEN
!
      CALL mesage(-8,0,nam)
   ELSE
!
      In = 1
      Iout = 1
      Ii = 1
      Incr = 1
!
      IF ( Lmodes<=0 ) THEN
         WRITE (Otpe,99001) Ufm
99001    FORMAT (A23,', LMODES PARAMETER MUST POSITIVE')
         CALL mesage(-61,0,0)
         CALL mesage(-8,0,nam)
      ELSE
         mcb(1) = pf
         CALL rdtrl(mcb)
         Nmodes = mcb(3)
         ndir = mcb(2)
         IF ( Lmodes>Nmodes ) Lmodes = Nmodes
!
!     GENERATE ROW PARTITIONING VECTOR FOR LAMB MATRIX TO PICK OFF THE
!     2ND COLUMN, WHICH IS THE COLUMN OF RADIAN FREQUENCIES. THEN
!     TRUNCATE THE COLUMN TO LMODES SIZE
!
         IF ( lcore<Nmodes ) THEN
            CALL mesage(-8,0,nam)
         ELSE
            CALL gopen(cplamb,Z(buf1),1)
            Nn = 0
            Z(1) = 0.
            Z(Nn+2) = 1.
            Z(Nn+3) = 0.
            Z(Nn+4) = 0.
            Z(Nn+5) = 0.
            Nn = 5
            mcb(1) = cplamb
            mcb(2) = 0
            mcb(3) = 5
            mcb(4) = 2
            mcb(5) = 1
            mcb(6) = 0
            mcb(7) = 0
            CALL pack(Z,cplamb,mcb)
            CALL close(cplamb,1)
            CALL wrttrl(mcb)
!
            CALL gopen(rplamb,Z(buf1),1)
            DO i = 1 , Lmodes
               Z(i) = 1.
            ENDDO
            IF ( Lmodes/=Nmodes ) THEN
               l1 = Lmodes + 1
               DO i = l1 , Nmodes
                  Z(i) = 0.
               ENDDO
            ENDIF
            Nn = Nmodes
            mcb(1) = rplamb
            mcb(2) = 0
            mcb(3) = Nmodes
            mcb(4) = 2
            mcb(5) = 1
            mcb(6) = 0
            mcb(7) = 0
            CALL pack(Z,rplamb,mcb)
            CALL close(rplamb,1)
            CALL wrttrl(mcb)
!
!     ROW PARTITION FOR PF
!
            CALL gopen(rppf,Z(buf1),1)
            mcb(1) = rppf
            mcb(2) = 0
            mcb(6) = 0
            mcb(7) = 0
            CALL pack(Z,rppf,mcb)
            CALL close(rppf,1)
            CALL wrttrl(mcb)
!
!     COLUMN PARTITION FOR MP-SAME AS ROW PARTITION FOR PREVIOUS FILES
!
            CALL gopen(cpmp,Z(buf1),1)
            mcb(1) = cpmp
            mcb(2) = 0
            mcb(6) = 0
            mcb(7) = 0
            CALL pack(Z,cpmp,mcb)
            CALL close(cpmp,1)
            CALL wrttrl(mcb)
!
            RETURN
         ENDIF
      ENDIF
   ENDIF
END SUBROUTINE genpar
