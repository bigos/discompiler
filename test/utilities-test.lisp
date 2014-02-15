(in-package :discompiler)

(in-suite :utilities)

(test test-addition
  "test simple addition"
  (is (equal 3 (+ 1 2)))
  (is (equal 1 (+ 0 1)))
  (is (equal 17 (1- (* 3 6))))
  (is (equal 6 (/ 6 1)))
  (is (equal 6 (+ 1 2 3))))

(test test-hex-bin
  "converting hex to binary"
  (is (equal "00000000" (int-to-bin 0)))
  (is (equal "11111111" (int-to-bin 255)))
  (is (equal "10101010" (int-to-bin 170))))

(test mod-rm
    "testing correct values of modrm and sib byte fiels"
    (is (equal 1 (mod-part "6b")))
    (is (equal 3 (rm-part "6b")))
    (is (equal 5 (reg-part "6b")))
    (is (equal 1 (ss-part "5d")))
    (is (equal 3 (index-part "5d")))
    (is (equal 5 (base-part "5d")))
    (is (equal 2 (ss-part "a6")))
    (is (equal 4 (index-part "a6")))
    (is (equal 6 (base-part "a6"))))

(test intel-bit-position
  "in Intel documentation bit 0 is the last one and bit 7 is the first one"
  (is (equal 0 (intel-bit-position 7)))
  (is (equal 1 (intel-bit-position 6)))
  (is (equal 2 (intel-bit-position 5)))
  (is (equal 3 (intel-bit-position 4)))
  (is (equal 4 (intel-bit-position 3)))
  (is (equal 5 (intel-bit-position 2)))
  (is (equal 6 (intel-bit-position 1)))
  (is (equal 7 (intel-bit-position 0))))
