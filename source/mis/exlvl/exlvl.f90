!*==exlvl.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE exlvl(Nos,Md,Name,Z,Nwds)
   USE c_sof
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nos
   INTEGER , DIMENSION(4,1) :: Md
   INTEGER , DIMENSION(2) :: Name
   INTEGER , DIMENSION(2) :: Z
   INTEGER :: Nwds
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: cs , hl , i , iold , iss , j , ll , m , nl , nss , ps , tp
   INTEGER , DIMENSION(2) , SAVE :: subr
   EXTERNAL andf , crsub , fdit , fdsub , fmdi , lshift , mesage , orf , page , rshift , smsg
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     EXLVL ADDS A SUBSTRUCTURE TO THE RESIDENT SOF FOR THE SOFIN
!     OPERATION.  IT USES THE DIT AND MDI DATA WRITTEN ON THE EXTERNAL
!     FILE BY SOFOUT TO RESTORE THE HL, CS, AND LL POINTERS IN THE MDI.
!
   DATA subr/4HEXLV , 4HL   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     ADD THE NEW SUBSTRUCTURE TO THE RESIDENT DIT.
!
         CALL fdsub(Name,i)
         IF ( i/=-1 ) THEN
!
!     SUBSTRUCTURE NAME WAS DUPLICATED
!
            CALL smsg(4,0,Name)
            RETURN
         ELSE
            CALL crsub(Name,i)
            IF ( Nos<=0 ) THEN
!
!     SUBSTRUCTURE ADDED TO SOF SUCCESSFULLY
!
               WRITE (nout,99005) Name
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSE
               Z(1) = Name(1)
               Z(2) = Name(2)
               nss = 1
               iss = 1
            ENDIF
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     DECODE THE OLD MDI ENTRY
!
         SPAG_Loop_1_1: DO i = 1 , Nos
            IF ( Md(1,i)==Z(2*iss-1) .AND. Md(2,i)==Z(2*iss) ) THEN
               ps = andf(Md(3,i),1023)
               tp = andf(rshift(Md(3,i),20),1023)
               ll = rshift(Md(4,i),20)
               cs = andf(rshift(Md(4,i),10),1023)
               hl = andf(Md(4,i),1023)
               iold = i
               EXIT SPAG_Loop_1_1
            ENDIF
         ENDDO SPAG_Loop_1_1
!
!     SET NEW MDI POINTERS FOR HL, CS, AND LL IF THE SUBSTRUCTURES OF
!     THE ORIGINATING SOF WHICH ARE INDICATED THEREBY EXIST.
!
!
!     HIGHER LEVEL (HL)
!
         m = 0
         IF ( hl/=0 ) THEN
            CALL fdsub(Md(1,hl),i)
            IF ( i<=0 ) THEN
               CALL crsub(Md(1,hl),i)
               nss = nss + 1
               IF ( 2*nss>Nwds ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Z(2*nss-1) = Md(1,hl)
               Z(2*nss) = Md(2,hl)
            ENDIF
            m = i
            hl = i
         ENDIF
!
!     COMBINED SUBSTRUCTURE (CS)
!
         IF ( cs/=0 ) THEN
            CALL fdsub(Md(1,cs),j)
            IF ( j<=0 ) THEN
               CALL crsub(Md(1,cs),j)
               nss = nss + 1
               IF ( 2*nss>Nwds ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Z(2*nss-1) = Md(1,cs)
               Z(2*nss) = Md(2,cs)
            ENDIF
            m = orf(m,lshift(j,10))
            cs = j
         ENDIF
!
!     LOWER LEVEL (LL)
!
         IF ( ll/=0 ) THEN
            CALL fdsub(Md(1,ll),j)
            IF ( j<=0 ) THEN
               CALL crsub(Md(1,ll),j)
               nss = nss + 1
               IF ( 2*nss>Nwds ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Z(2*nss-1) = Md(1,ll)
               Z(2*nss) = Md(2,ll)
            ENDIF
            m = orf(m,lshift(j,20))
            ll = j
         ENDIF
!
!     UPDATE THE MDI
!
         CALL fdsub(Z(2*iss-1),j)
         CALL fmdi(j,i)
         buf(i+1) = lshift(tp,20)
         buf(i+2) = m
         mdiup = .TRUE.
!
!     WRITE USER MESSAGES
!
         nl = 2
         IF ( ll/=0 ) nl = nl + 1
         IF ( cs/=0 ) nl = nl + 1
         IF ( hl/=0 ) nl = nl + 1
         IF ( ps/=0 ) nl = nl + 3
         IF ( line+nl>nlpp ) CALL page
         line = line + nl
         WRITE (nout,99005) Z(2*iss-1) , Z(2*iss)
         IF ( hl/=0 ) THEN
            CALL fdit(hl,i)
            WRITE (nout,99001) buf(i) , buf(i+1)
99001       FORMAT (5X,25HHIGHER LEVEL SUBSTRUCTURE,2X,2A4)
         ENDIF
         IF ( cs/=0 ) THEN
            CALL fdit(cs,i)
            WRITE (nout,99002) buf(i) , buf(i+1)
99002       FORMAT (5X,25HCOMBINED SUBSTRUCTURE    ,6(2X,2A4))
         ENDIF
         IF ( ll/=0 ) THEN
            CALL fdit(ll,i)
            WRITE (nout,99003) buf(i) , buf(i+1)
99003       FORMAT (5X,25HLOWER LEVEL SUBSTRUCTURE ,7(2X,2A4))
         ENDIF
         IF ( ps/=0 ) THEN
            WRITE (nout,99004) Z(2*iss-1) , Z(2*iss)
99004       FORMAT (49H0*** USER INFORMATION MESSAGE 6359, SUBSTRUCTURE ,2A4,41H WAS ORIGINALLY A SECONDARY SUBSTRUCTURE./36X,      &
                   &42HON THIS SOF, IT IS A PRIMARY SUBSTRUCTURE.)
         ENDIF
         iss = iss + 1
         IF ( iss<=nss ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         RETURN
      CASE (4)
!
!     INSUFFICIENT CORE
!
         CALL mesage(-8,0,subr)
         RETURN
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
!     MESSAGE TEXT
!
99005 FORMAT (49H0*** USER INFORMATION MESSAGE 6347, SUBSTRUCTURE ,2A4,18H ADDED TO THE SOF.)
END SUBROUTINE exlvl
