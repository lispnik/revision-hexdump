;;;; templates.lisp --- SAMPLE structural templates for the revision-hexdump hex editor.
;;;;
;;;; Copy this file to  ~/.config/revision-hexdump/templates.lisp  (it is loaded
;;;; automatically at startup) and edit to taste.
;;;;
;;;; Each top-level form is a (NAME . FIELD-SPECS) entry added to *TEMPLATES*.  Loaded at
;;;; startup (LOAD-TEMPLATES): each top-level form is a (NAME . FIELD-SPECS) entry
;;;; and is added to REVISION-HEXDUMP:*TEMPLATES*.  A field spec is
;;;; (NAME TYPE . OPTIONS); a leading (:endian ...) sets byte order and (:magic ...)
;;;; makes the template auto-detect on open.  Types: scalar keywords (:u8 .. :i64,
;;;; :f32/:f64), (:string N), (:bytes N), (:array ELEM N) and (:struct ...); a length
;;;; N may name a prior field; a scalar may carry :enum / :flags.  Apply with Ctrl-D.

;;; PNG — big-endian, 8-byte magic, then the IHDR chunk.
("PNG image"
 (:endian :big) (:magic (#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A))
 (signature   (:bytes 8))
 (ihdr-length :u32)
 (ihdr-type   (:string 4))          ; "IHDR"
 (width       :u32)
 (height      :u32)
 (bit-depth   :u8)
 (color-type  :u8 :enum ((0 . "grayscale") (2 . "RGB") (3 . "palette")
                         (4 . "grayscale+alpha") (6 . "RGBA")))
 (compression :u8 :enum ((0 . "deflate")))
 (filter      :u8 :enum ((0 . "adaptive")))
 (interlace   :u8 :enum ((0 . "none") (1 . "Adam7"))))

;;; ELF (64-bit, little-endian) — magic includes class=2 and data=1 so it only
;;; auto-detects 64-bit LE objects (the common Linux x86-64 / AArch64 case).
("ELF (64-bit LE)"
 (:endian :little) (:magic (#x7F #x45 #x4C #x46 #x02 #x01))
 (magic       (:bytes 4))           ; 7F 45 4C 46  (\x7FELF)
 (class       :u8  :enum ((1 . "ELF32") (2 . "ELF64")))
 (data        :u8  :enum ((1 . "little-endian") (2 . "big-endian")))
 (ei-version  :u8)
 (os-abi      :u8  :enum ((0 . "System V") (3 . "Linux") (9 . "FreeBSD")))
 (abi-version :u8)
 (pad         (:bytes 7))
 (type        :u16 :enum ((1 . "REL") (2 . "EXEC") (3 . "DYN") (4 . "CORE")))
 (machine     :u16 :enum ((#x03 . "x86") (#x28 . "ARM") (#x3E . "x86-64")
                          (#xB7 . "AArch64") (#xF3 . "RISC-V")))
 (version     :u32)
 (entry       :u64)
 (phoff       :u64)
 (shoff       :u64)
 (flags       :u32)
 (ehsize      :u16)
 (phentsize   :u16)
 (phnum       :u16)
 (shentsize   :u16)
 (shnum       :u16)
 (shstrndx    :u16))

;;; ZIP local file header — dynamic-length filename / extra fields, driven by the
;;; length fields that precede them.
("ZIP local file header"
 (:endian :little) (:magic (#x50 #x4B #x03 #x04))   ; PK\x03\x04
 (signature         (:bytes 4))
 (version-needed    :u16)
 (flags             :u16)
 (compression       :u16 :enum ((0 . "stored") (8 . "deflate") (12 . "bzip2") (14 . "LZMA")))
 (mod-time          :u16)
 (mod-date          :u16)
 (crc32             :u32)
 (compressed-size   :u32)
 (uncompressed-size :u32)
 (filename-length   :u16)
 (extra-length      :u16)
 (filename          (:string filename-length))     ; length from FILENAME-LENGTH
 (extra             (:bytes  extra-length)))        ; length from EXTRA-LENGTH

;;; JPEG / JFIF — big-endian, marker-based.  Magic FF D8 FF E0 is SOI + APP0, i.e.
;;; a JFIF-format JPEG (the common case); this template maps the JFIF APP0 header.
("JPEG / JFIF header"
 (:endian :big) (:magic (#xFF #xD8 #xFF #xE0))
 (soi           :u16)               ; FFD8  Start Of Image
 (app0-marker   :u16)               ; FFE0  APP0
 (app0-length   :u16)               ; length of the APP0 segment
 (identifier    (:string 5))        ; "JFIF\0"
 (version-major :u8)
 (version-minor :u8)
 (density-units :u8 :enum ((0 . "aspect ratio") (1 . "pixels/inch") (2 . "pixels/cm")))
 (x-density     :u16)
 (y-density     :u16)
 (thumb-width   :u8)
 (thumb-height  :u8))

;;; PDF — a text header "%PDF-x.y" (the body is an object graph, not fixed binary,
;;; so the template just identifies the file and its version).
("PDF document"
 (:magic "%PDF-")
 (signature (:string 5))            ; "%PDF-"
 (version   (:string 3)))           ; e.g. "1.7", "2.0"

;;; Mach-O 64-bit (little-endian) — a native single-arch macOS/iOS binary.
;;; Magic CF FA ED FE = 0xFEEDFACF stored little-endian (MH_MAGIC_64).
("Mach-O (64-bit LE)"
 (:endian :little) (:magic (#xCF #xFA #xED #xFE))
 (magic      :u32)
 (cputype    :i32 :enum ((#x01000007 . "x86-64") (#x0100000C . "ARM64")
                         (#x00000007 . "x86") (#x0000000C . "ARM")))
 (cpusubtype :i32)
 (filetype   :u32 :enum ((1 . "object") (2 . "executable") (4 . "core") (6 . "dylib")
                         (7 . "dylinker") (8 . "bundle") (10 . "dSYM") (11 . "kext")))
 (ncmds      :u32)
 (sizeofcmds :u32)
 (flags      :u32 :flags ((#x1 . "noundefs") (#x4 . "dyldlink") (#x80 . "twolevel")
                          (#x100000 . "weak-defines") (#x200000 . "pie")))
 (reserved   :u32))

;;; Mach-O universal / "fat" binary — big-endian header wrapping N architecture
;;; slices (the common form for macOS system binaries).  Magic CA FE BA BE is also
;;; Java .class; here it maps the fat header + its dynamic array of arch records.
("Mach-O universal (fat)"
 (:endian :big) (:magic (#xCA #xFE #xBA #xBE))
 (magic     :u32)                   ; CAFEBABE
 (nfat-arch :u32)                   ; number of architecture slices
 (archs (:array (:struct
                  (cputype    :i32 :enum ((#x01000007 . "x86-64") (#x0100000C . "ARM64")
                                          (#x00000007 . "x86") (#x0000000C . "ARM")))
                  (cpusubtype :i32)
                  (offset     :u32)   ; file offset of this slice
                  (size       :u32)
                  (align      :u32))  ; power of 2
                nfat-arch)))

;;; tar (POSIX ustar) header — the 512-byte record before each archived file.
;;; No leading magic (the "ustar" tag is at byte 257, not 0), so this one doesn't
;;; auto-detect: put the cursor on a header and apply it with Ctrl-D.  Numeric
;;; fields (mode/size/mtime/…) are octal ASCII text, shown as strings.
("tar (ustar) header"
 (name     (:string 100))
 (mode     (:string 8))
 (uid      (:string 8))
 (gid      (:string 8))
 (size     (:string 12))            ; octal ASCII byte count
 (mtime    (:string 12))            ; octal ASCII unix time
 (chksum   (:string 8))             ; octal ASCII header checksum
 (typeflag :u8 :enum ((48 . "regular") (49 . "hard link") (50 . "symlink")
                      (51 . "char device") (52 . "block device") (53 . "directory")
                      (54 . "fifo") (55 . "contiguous") (76 . "GNU long name")
                      (120 . "pax header") (103 . "pax global")))
 (linkname (:string 100))
 (magic    (:string 6))             ; "ustar" (+ NUL, or space for GNU)
 (version  (:string 2))
 (uname    (:string 32))            ; owner user name
 (gname    (:string 32))            ; owner group name
 (devmajor (:string 8))
 (devminor (:string 8))
 (prefix   (:string 155))           ; path prefix (for long names)
 (pad      (:bytes 12)))
