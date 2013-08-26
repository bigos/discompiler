(defun mnemonic-columns (mnemonics)
  (let ((my-data))
    (setf my-data '((("Opcode/Instruction" "Op/En" "64/32bit Mode Support" 
                      "CPUID Feature Flag" "Description")
                     ("XTEST" "XEND" "XBEGIN" "XABORT"))

                    (("Opcode/Instruction" "64/32bit Mode Support"
                      "CPUID Feature Flag" "Description")
                     ("XACQUIRE/XRELEASE"))

                    (("Opcode/Instruction" "Op/En" "64/32bit Mode Support" 
                      "CPUID Feature Flag" "Description")
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


                    (("Opcode" "Instruction" "64-Bit Mode" "Compat/ Leg Mode" "Description")
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

                    (("Opcode/Instruction" "Op/En" "64/32bit Mode" 
                      "CPUID Feature Flag" "Description")
                     ("VFNMSUB132SD/VFNMSUB213SD/VFNMSUB231SD"
                      "VFMADD132SD/VFMADD213SD/VFMADD231SD"
                      "VFMADD132PD/VFMADD213PD/VFMADD231PD" "VCVTPS2PH" "VCVTPH2PS" "INVPCID"
                      "CMPPS" "CMPPD"))

                    (("Opcode/Instruction" "Op/En" "64/32bit Mode" 
                      "CPUID Feature Flag" "Description")
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

                    (("Opcode/Instruction" "Op/En" "64/32bit Mode" 
                      "CPUID Feature Flag" "Description")
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

                    (("Opcode" "Instruction" "Op/En" "64-bit Mode" 
                      "Compat Leg Mode" "Description")
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
                      "ADC" "AAS" "AAM" "AAD" "AAA"))))
    (dolist (d my-data)
      (if (search `(,mnemonics) (nth 2 d) :test #'equalp)
          (format t "~&~S~%~%" (nth 1 d))))))
