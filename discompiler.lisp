(in-package :discompiler)

(defun run ()
  (format t "running skeleton program")
  ;; (test-arithmetic)
  ;; (test-mod-vals)
  ;; (run-tests :all)
  (defparameter *problem-files* '())
  (defparameter *reference-files* (cl-fad:list-directory "my-reference"))
  ;; (car (nth 469 *instructions*))
  ;; ("XTEST — Test If In Transactional Execution" "Opcode/Instruction Op/En"
  ;; "64/32bit" "Mode" "Support" "CPUID" "Feature" "Flag" "Description")
  ;; wow!!!
  (defparameter *instructions* nil)
  (dolist (file  *reference-files*)
    (setf *instructions* (nconc *instructions* (process-file file)))))

(defun instruction-title (instruction)
  (car instruction))

(defun instruction-columns (instruction)
  (cadr instruction))

(defun column-keywords (instruction)
  (let ((column-data (instruction-columns instruction)))      
    (sort (map 'list 
               #'(lambda (x) (string-trim "/" x)) 
               (remove-if 'emptystrp 
                          (cl-utilities:split-sequence #\space 
                                                       (list-to-string 
                                                        column-data)))) 
          #'string-lessp)))

(defun mnemonic-columns (mnemonics columns)

(("64/32bit" "CPUID" "Description" "En" "Feature" "Flag" "Instruction"
  "Mode" "Op" "Opcode" "Support")

 ("XTEST" "XEND" "XBEGIN" "XABORT"))

(("64/32-" "bit" "CPUID" "Description" "Feature" "Flag" "Instruction"
  "Mode" "Opcode" "Support")
 ("XACQUIRE/XRELEASE"))

(("64/32" "bit" "CPUID" "Description" "En" "Feature" "Flag" "Instruction"
  "Mode" "Op" "Opcode" "Support")
 ("XSAVEOPT" "XORPS" "XORPD" "VZEROUPPER" "VZEROALL" "VTESTPD/VTESTPS"
  "VPERM2F128" "VPERMILPS" "VPERMILPD" "UNPCKLPS" "UNPCKLPD" "UNPCKHPS"
  "UNPCKHPD" "UCOMISS" "UCOMISD" "SUBSS" "SUBSD" "SUBPS" "SUBPD" "STMXCSR"
  "SQRTSS" "SQRTSD" "SQRTPS" "SQRTPD" "SHUFPS" "SHUFPD" "RSQRTSS"
  "RSQRTPS" "ROUNDSS" "ROUNDSD" "ROUNDPS" "ROUNDPD" "RDRAND" "RCPSS"
  "RCPPS" "PXOR" "PUNPCKLBW/PUNPCKLWD/PUNPCKLDQ/PUNPCKLQDQ"
  "PUNPCKHBW/PUNPCKHWD/PUNPCKHDQ/PUNPCKHQDQ" "PTEST" "PSUBUSB/PSUBUSW"
  "PSUBSB/PSUBSW" "PSUBQ" "PSUBB/PSUBW/PSUBD" "PSRLW/PSRLD/PSRLQ" "PSRLDQ"
  "PSRAW/PSRAD" "PSLLW/PSLLD/PSLLQ" "PSLLDQ" "PSIGNB/PSIGNW/PSIGND"
  "PSHUFLW" "PSHUFHW" "PSHUFD" "PSHUFB" "PSADBW" "POR" "PMULUDQ" "PMULLW"
  "PMULLD" "PMULHW" "PMULHUW" "PMULHRSW" "PMULDQ" "PMOVZX" "PMOVSX"
  "PMOVMSKB" "PMINUW" "PMINUD" "PMINUB" "PMINSW" "PMINSD" "PMINSB"
  "PMAXUW" "PMAXUD" "PMAXUB" "PMAXSW" "PMAXSD" "PMAXSB" "PMADDWD"
  "PMADDUBSW" "PINSRW" "PINSRB/PINSRD/PINSRQ" "PHSUBSW" "PHSUBW/PHSUBD"
  "PHMINPOSUW" "PHADDSW" "PHADDW/PHADDD" "PEXTRW" "PEXTRB/PEXTRD/PEXTRQ"
  "PCMPISTRM" "PCMPISTRI" "PCMPGTQ" "PCMPGTB/PCMPGTW/PCMPGTD" "PCMPESTRM"
  "PCMPESTRI" "PCMPEQQ" "PCMPEQB/PCMPEQW/PCMPEQD" "PCLMULQDQ" "PBLENDW"
  "PBLENDVB" "PAVGB/PAVGW" "PANDN" "PAND" "PALIGNR" "PADDUSB/PADDUSW"
  "PADDSB/PADDSW" "PADDQ" "PADDB/PADDW/PADDD" "PACKUSWB" "PACKUSDW"
  "PACKSSWB/PACKSSDW" "PABSB/PABSW/PABSD" "ORPS" "ORPD"))

(("64-Bit" "Compat" "Description" "Instruction" "Leg" "Mode" "Mode"
  "Opcode")
 ("FYL2XP1" "FYL2X" "FXTRACT" "FXCH" "FXAM" "FUCOM/FUCOMP/FUCOMPP" "FTST"
  "FSUBR/FSUBRP/FISUBR" "FSUB/FSUBP/FISUB" "FSTSW/FNSTSW" "FSTENV/FNSTENV"
  "FSTCW/FNSTCW" "FST/FSTP" "FSQRT" "FSINCOS" "FSIN" "FSCALE"
  "FSAVE/FNSAVE" "FRSTOR" "FRNDINT" "FPTAN" "FPREM1" "FPREM" "FPATAN"
  "FNOP" "FMUL/FMULP/FIMUL" "FLDENV" "FLDCW"
  "FLD1/FLDL2T/FLDL2E/FLDPI/FLDLG2/FLDLN2/FLDZ" "FLD" "FIST/FISTP"
  "FINIT/FNINIT" "FINCSTP" "FILD" "FICOM/FICOMP" "FFREE"
  "FDIVR/FDIVRP/FIDIVR" "FDIV/FDIVP/FIDIV" "FDECSTP" "FCOS"
  "FCOMI/FCOMIP/ FUCOMI/FUCOMIP" "FCOM/FCOMP/FCOMPP" "FCMOVcc"
  "FCLEX/FNCLEX" "FCHS" "FBSTP" "FBLD" "FADD/FADDP/FIADD" "FABS" "F2XM1"))

(("64/32-" "bit" "CPUID" "Description" "En" "Feature" "Flag" "Instruction"
  "Mode" "Op" "Opcode")
 ("VFNMSUB132SD/VFNMSUB213SD/VFNMSUB231SD"
  "VFMADD132SD/VFMADD213SD/VFMADD231SD"
  "VFMADD132PD/VFMADD213PD/VFMADD231PD" "VCVTPS2PH" "VCVTPH2PS" "INVPCID"
  "CMPPS" "CMPPD"))

(("-bit" "64/32" "CPUID" "Description" "En" "Feature" "Flag" "Instruction"
  "Mode" "Op" "Opcode")
 ("VPSRLVD/VPSRLVQ" "VPSRAVD" "VPSLLVD/VPSLLVQ" "VPMASKMOV" "VPERM2I128"
  "VPERMQ" "VPERMPS" "VPERMPD" "VPERMD" "VPBROADCAST" "VPBLENDD"
  "VINSERTI128" "VPGATHERDQ/VPGATHERQQ" "VPGATHERDD/VPGATHERQD"
  "VGATHERDPS/VGATHERQPS" "VFNMSUB132SS/VFNMSUB213SS/VFNMSUB231SS"
  "VFNMSUB132PS/VFNMSUB213PS/VFNMSUB231PS"
  "VFNMSUB132PD/VFNMSUB213PD/VFNMSUB231PD"
  "VFNMADD132SS/VFNMADD213SS/VFNMADD231SS"
  "VFNMADD132SD/VFNMADD213SD/VFNMADD231SD"
  "VFNMADD132PS/VFNMADD213PS/VFNMADD231PS"
  "VFNMADD132PD/VFNMADD213PD/VFNMADD231PD"
  "VFMSUB132SS/VFMSUB213SS/VFMSUB231SS"
  "VFMSUB132SD/VFMSUB213SD/VFMSUB231SD"
  "VFMSUB132PS/VFMSUB213PS/VFMSUB231PS"
  "VFMSUB132PD/VFMSUB213PD/VFMSUB231PD"
  "VFMSUBADD132PS/VFMSUBADD213PS/VFMSUBADD231PS"
  "VFMSUBADD132PD/VFMSUBADD213PD/VFMSUBADD231PD"
  "VFMADDSUB132PS/VFMADDSUB213PS/VFMADDSUB231PS"
  "VFMADDSUB132PD/VFMADDSUB213PD/VFMADDSUB231PD"
  "VFMADD132SS/VFMADD213SS/VFMADD231SS"
  "VFMADD132PS/VFMADD213PS/VFMADD231PS" "TZCNT" "SARX/SHLX/SHRX" "RORX"
  "PEXT" "PDEP" "MULX" "LZCNT" "BZHI" "BLSR" "BLSMSK" "BLSI" "BEXTR"
  "ANDN"))

(("64/32-bit" "CPUID" "Description" "En" "Feature" "Flag" "Instruction"
  "Mode" "Op" "Opcode")
 ("WRFSBASE/WRGSBASE" "VMASKMOV" "VINSERTF128" "VGATHERDPD/VGATHERQPD"
  "VEXTRACTI128" "VEXTRACTF128" "VBROADCAST" "RDFSBASE/RDGSBASE" "MULSS"
  "MULSD" "MULPS" "MULPD" "MPSADBW" "MOVUPS" "MOVUPD" "MOVSS" "MOVSLDUP"
  "MOVSHDUP" "MOVSD" "MOVQ" "MOVNTPS" "MOVNTPD" "MOVNTDQ" "MOVNTDQA"
  "MOVMSKPS" "MOVMSKPD" "MOVLPS" "MOVLPD" "MOVLHPS" "MOVHPS" "MOVHPD"
  "MOVHLPS" "MOVDQU" "MOVDQA" "MOVDDUP" "MOVD/MOVQ" "MOVAPS" "MOVAPD"
  "MINSS" "MINSD" "MINPS" "MINPD" "MAXSS" "MAXSD" "MAXPS" "MAXPD"
  "MASKMOVDQU" "LDMXCSR" "LDDQU" "INSERTPS" "HSUBPS" "HSUBPD" "HADDPS"
  "HADDPD" "EXTRACTPS" "DPPS" "DPPD" "DIVSS" "DIVSD" "DIVPS" "DIVPD"
  "CVTTSS2SI" "CVTTSD2SI" "CVTTPS2DQ" "CVTTPD2DQ" "CVTSS2SI" "CVTSS2SD"
  "CVTSI2SS" "CVTSI2SD" "CVTSD2SS" "CVTSD2SI" "CVTPS2PD" "CVTPS2DQ"
  "CVTPD2PS" "CVTPD2DQ" "CVTDQ2PS" "CVTDQ2PD" "COMISS" "COMISD" "CMPSS"
  "CMPSD" "BLENDVPS" "BLENDVPD" "BLENDPS" "BLENDPD" "AESIMC" "AESENCLAST"
  "AESENC" "AESDECLAST" "AESDEC" "ANDNPS" "ANDNPD" "ANDPS" "ANDPD"
  "ADDSUBPS" "ADDSUBPD" "ADDSS" "ADDSD" "ADDPS" "ADDPD"))

(("64-bit" "Compat" "Description" "En" "Instruction" "Leg" "Mode" "Mode"
  "Op" "Opcode")
 ("XSETBV" "XSAVE" "XRSTOR" "XOR" "XLAT/XLATB" "XGETBV" "XCHG" "XADD"
  "WRMSR" "WBINVD" "WAIT/FWAIT" "VERR/VERW" "UD2" "TEST" "SYSRET"
  "SYSEXIT" "SYSENTER" "SYSCALL" "SWAPGS" "SUB" "STR"
  "STOS/STOSB/STOSW/STOSD/STOSQ" "STI" "STD" "STC" "SMSW" "SLDT" "SIDT"
  "SHRD" "SHLD" "SGDT" "SFENCE" "SETcc" "SCAS/SCASB/SCASW/SCASD" "SBB"
  "SAL/SAR/SHL/SHR" "SAHF" "RSM" "RET" "REP/REPE/REPZ/REPNE/REPNZ"
  "RDTSCP" "RDTSC" "RDPMC" "RDMSR" "RCL/RCR/ROL/ROR-" "PUSHF/PUSHFD"
  "PUSHA/PUSHAD" "PUSH" "PSHUFW" "PREFETCHh" "POPF/POPFD/POPFQ" "POPCNT"
  "POPA/POPAD" "POP" "PAUSE" "OUTS/OUTSB/OUTSW/OUTSD" "OUT" "OR" "NOT"
  "NOP" "NEG" "MWAIT" "MUL" "MOVZX" "MOVSX/MOVSXD"
  "MOVS/MOVSB/MOVSW/MOVSD/MOVSQ" "MOVQ2DQ" "MOVNTQ" "MOVNTI" "MOVDQ2Q"
  "MOVBE" "MOV" "MOV" "MOV" "MONITOR" "MFENCE" "MASKMOVQ" "LTR" "LSL"
  "LOOP/LOOPcc" "LODS/LODSB/LODSW/LODSD/LODSQ" "LOCK" "LMSW" "LLDT"
  "LGDT/LIDT" "LFENCE" "LEAVE" "LEA" "LDS/LES/LFS/LGS/LSS" "LAR" "LAHF"
  "JMP" "Jcc" "IRET/IRETD" "INVLPG" "INVD" "INT n/INTO/INT 3"
  "INS/INSB/INSW/INSD" "INC" "IN" "IMUL" "IDIV" "HLT" "FXSAVE" "FXRSTOR"
  "ENTER" "EMMS" "DIV" "DEC" "DAS" "DAA" "CWD/CDQ/CQO" "CVTTPS2PI"
  "CVTTPD2PI" "CVTPS2PI" "CVTPI2PS" "CVTPI2PD" "CVTPD2PI" "CRC32" "CPUID"
  "CMPXCHG8B/CMPXCHG16B" "CMPXCHG" "CMPS/CMPSB/CMPSW/CMPSD/CMPSQ" "CMP"
  "CMOVcc" "CMC" "CLTS" "CLI" "CLFLUSH" "CLD" "CLC" "CBW/CWDE/CDQE" "CALL"
  "BTS" "BTR" "BTC" "BT" "BSWAP" "BSR" "BSF" "BOUND" "ARPL" "AND" "ADD"
  "ADC" "AAS" "AAM" "AAD" "AAA"))




)

(defun emptystrp (string)
  (if (equal string "") 
      T 
      nil))

(defun list-to-string (my-list &optional (separator " ")) 
  (let ((result))
    (dolist (item my-list)
      (setf result (concatenate 'string 
                                result 
                                (format nil "~A~a" item separator))))
    result))

(defun show-suspected () 
  (let ((ci))
    (dolist (inst *instructions*)
      (setf ci (cadr inst)) 
      (if (> (list-length ci) 1) 
          (format t "~&~a ~%~%~%~s ~%" (length  ci)  ci )))))

(defun instruction-memonics (instruction)
  (let ((separator "—") 
        (title (caar instruction)))
    (string-trim " " (subseq title 0 (search  separator title)))))

(defun uniques (instructions)
  (let ((columns) (mnemonics) (found) (column-mnemonics))
    (dolist (i instructions)
      (setf columns (column-keywords i))
      (setf mnemonics (instruction-memonics i))
      (setf found nil)
      ;;(format t "~s ~s ~S~%" found columns mnemonics)
      (loop for cm in column-mnemonics
         when (equalp columns (car cm))
         do
           (progn
             (push mnemonics (cadr cm))
             ;;(format t ">>>>> ~S ~S~%" cm (cadr cm))
             (setf found T)
             (loop-finish)))      
      (unless found
        (progn
          ;;(format t "not found ~%")
          (push `(,columns (,mnemonics)) column-mnemonics))))
    column-mnemonics))

(defun process-file (file)
  (declare (optimize (speed 0) (space 1) (compilation-speed 0) (debug 3)))
  ;; use -> (step (process-file (car *reference-files*)))
  ;; to step through the function
  (let ((lines (file-to-lines file)) (section) (sections) (instructions))
    (dolist (line lines)
      (cond ((blankp line)
             (unless (eq section nil)
                 (setf sections (nconc sections (list section))))
             (setf section nil))
            ((separatorp line)
             (setf sections (nconc sections (list section))
                   instructions (nconc instructions (list sections))
                   sections nil
                   section nil))
            (t
             (setf section (nconc section (list line))))))
    instructions))

(defun file-to-lines (file)
  (let ((lines))
    (with-open-file (stream file)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (setq lines (nconc lines (list line)))))
    lines))

(defun get-file-instruction (file-no instruction-no)
  (nth instruction-no (process-file (nth file-no *reference-files*))))

(defun process (line)
  (cond ((blankp line)
         (print line))
        ((separatorp line)
         (format t "~&~s ~D~%" line (length line)))
        (t (format t "."))))

(defun blankp (line)
  (cl-ppcre:scan-to-strings "\\A\\z" line))

(defun separatorp (line)
  (cl-ppcre:scan-to-strings "-{10,}" line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun int-to-bin (int)
  (format nil "~8,'0B" int))

(defun int-to-hex (int)
  (format nil "~x" int))

(defun hex-to-int (hex)
  (parse-integer hex :radix 16))

(defun hex-to-bin (hex)
  (int-to-bin (hex-to-int hex)))

(defun bin-to-int (bin)
  (parse-integer bin :radix 2))

(defun byte-part (bin start end)
  (bin-to-int (subseq bin start end)))

(defun byte-parts (hex)
  (let ((bin (hex-to-bin hex))a)
    (list (byte-part bin 0 2)
          (byte-part bin 2 5)
          (byte-part bin 5 8))))

(defun modrmreg-vals (hex)
  (let ((parts (byte-parts hex)))
    (list (first parts) (third parts) (second parts))))

(defun mod-part (hex)
  (byte-part (hex-to-bin hex) 0 2))
(defun rm-part (hex)
  (byte-part (hex-to-bin hex) 5 8))
(defun reg-part (hex)
  (byte-part (hex-to-bin hex) 2 5))

(defun ss-part (hex)
  (byte-part (hex-to-bin hex) 0 2))
(defun index-part (hex)
  (byte-part  (hex-to-bin hex) 2 5))
(defun base-part (hex)
  (byte-part  (hex-to-bin hex) 5 8))

(defun get-reg (reg)
  "general-purpose registers, ia-32 vol1 p60"
  (let ((registers '(('eax ('ax ('ah 'al)))
                     ('ebx ('bx ('bh 'bl)))
                     ('ecx ('cx ('ch 'cl)))
                     ('edx ('dx ('dh 'dl)))
                     ('ebp ('bp))
                     ('esi ('si))
                     ('edi ('di))
                     ('esp ('sp)))))
    nil
    ))

(defun get-segment (reg)
  "segment registers ia32 vol1 p61"
  (let ((segments '('cs 'ds 'ss 'es 'fs 'gs)))
    nil
    ))

(defun intel-bit-position (index &optional (width 8))
  (if (typep index 'integer)
      (cond ((>= index width) (error "index ~a should not be >= ~a" index width))
            ((< index 0) (error "index should not be less than 0"))
            (t (- (1- width) index)))
      (error "expected integer but ~a given" index)))
