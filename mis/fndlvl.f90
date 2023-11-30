
SUBROUTINE fndlvl(Name,Newnm)
   IMPLICIT NONE
   INTEGER Buf(1)
   COMMON /zzzzzz/ Buf
   INTEGER Name(2) , Newnm(2)
   INTEGER andf , rshift
   INTEGER iempty , ill , imdi , jdit , k , ll , nmsbr(2)
   EXTERNAL andf , rshift
!
!     THIS SUBROUTINE LOOKS FOR A LOWER LEVEL SUBSTRUCTUE TO THE
!     SUBSTRUCTURE NAME.  IF NAME DOES HAVE A LOWER LEVEL SUBSTRUCTURE,
!     THE NAME OF ONE OF THESE LOWER LEVEL SUBSTRUCTURES WILL BE
!     RETURNED IN NEWNM.  IF NAME DOES NOT HAVE A LOWER LEVEL
!     SUBSTRUCTURE, NAME WILL BE RETURNED IN NEWNM.  IF NAME IS NOT
!     KNOWN TO THE SYSTEM, BLANKS WILL BE RETURNED IN NEWNM.
!
   DATA ll/2/
   DATA iempty/4H    / , nmsbr/4HFNDL , 4HVL  /
!
!     CHECK IF NAME EXISTS
!
   CALL chkopn(nmsbr(1))
   CALL fdsub(Name(1),k)
   IF ( k/=-1 ) THEN
!
!     FIND THE LOWER LEVEL SUBSTRUCTURE
!
      CALL fmdi(k,imdi)
      ill = andf(rshift(Buf(imdi+ll),20),1023)
      IF ( ill==0 ) THEN
!
!     NAME DOES NOT HAVE A LOWER LEVEL SUBSTRUCTURE
!
         Newnm(1) = Name(1)
         Newnm(2) = Name(2)
         GOTO 99999
      ENDIF
   ELSE
      Newnm(1) = iempty
      Newnm(2) = iempty
      RETURN
   ENDIF
!
!     NAME DOES HAVE A LOWER LEVEL SUBSTRUCTURE
!
   CALL fdit(ill,jdit)
   Newnm(1) = Buf(jdit)
   Newnm(2) = Buf(jdit+1)
   RETURN
99999 RETURN
END SUBROUTINE fndlvl