(defpackage :nut.elf64
  (:use :cl))
(in-package :nut.elf64)

(defun read-little-endian (stream num)
  (let ((seq (make-array num)))
    (read-sequence seq stream)
    (loop for byte across seq
          for power from 0
          sum (ash byte (* 8 power)))))

(defun read-int8 (stream)
  (read-byte stream))

(defun read-int16 (stream &key little-endian-p)
  (if little-endian-p
      (read-little-endian stream 2)
      (assert nil)))

(defun read-int32 (stream &key little-endian-p)
  (if little-endian-p
      (read-little-endian stream 4)
      (assert nil)))

(defun read-int64 (stream &key little-endian-p)
  (if little-endian-p
      (read-little-endian stream 8)
      (assert nil)))

(defstruct ident
  magic
  class
  endian
  version
  osabi
  abiversion)

(defun ident-little-endian-p (ident)
  (= (ident-endian ident) 1))

(defun read-magic (stream)
  (let ((seq (make-array 4)))
    (read-sequence seq stream)
    (assert (= (aref seq 0) #x7f))
    (assert (= (aref seq 1) (char-code #\E)))
    (assert (= (aref seq 2) (char-code #\L)))
    (assert (= (aref seq 3) (char-code #\F)))
    seq))

(defun read-class (stream)
  (let ((int8 (read-int8 stream)))
    (assert (or (= int8 1) (= int8 2)))
    int8))

(defun read-ident (stream)
  (let ((i (make-ident)))
    (setf (ident-magic i) (read-magic stream))
    (setf (ident-class i) (read-class stream))
    (cond ((= (ident-class i) 2)
           (setf (ident-endian i)  (read-int8 stream))
           (setf (ident-version i) (read-int8 stream))
           (setf (ident-osabi i) (read-int8 stream))
           (setf (ident-abiversion i) (read-int8 stream))
           (read-sequence (make-array 7) stream))
          (t
           (assert nil)))
    i))

(defstruct file-header
  ident
  type
  machine
  version
  entry
  phoff
  shoff
  flags
  ehsize
  phentsize
  phnum
  shentsize
  shnum
  shstrndx)

(defun read-file-header (stream)
  (let* ((ident (read-ident stream))
         (little-endian-p (ident-little-endian-p ident)))
    (let ((h (make-file-header)))
      (setf (file-header-ident h) ident)
      (setf (file-header-type h)
            (read-int16 stream :little-endian-p little-endian-p))
      (setf (file-header-machine h)
            (read-int16 stream :little-endian-p little-endian-p))
      (setf (file-header-version h)
            (read-int32 stream :little-endian-p little-endian-p))
      (setf (file-header-entry h)
            (read-int64 stream :little-endian-p little-endian-p))
      (setf (file-header-phoff h)
            (read-int64 stream :little-endian-p little-endian-p))
      (setf (file-header-shoff h)
            (read-int64 stream :little-endian-p little-endian-p))
      (setf (file-header-flags h)
            (read-int32 stream :little-endian-p little-endian-p))
      (setf (file-header-ehsize h)
            (read-int16 stream :little-endian-p little-endian-p))
      (setf (file-header-phentsize h)
            (read-int16 stream :little-endian-p little-endian-p))
      (setf (file-header-phnum h)
            (read-int16 stream :little-endian-p little-endian-p))
      (setf (file-header-shentsize h)
            (read-int16 stream :little-endian-p little-endian-p))
      (setf (file-header-shnum h)
            (read-int16 stream :little-endian-p little-endian-p))
      (setf (file-header-shstrndx h)
            (read-int16 stream :little-endian-p little-endian-p))
      h)))

(defstruct section-header
  name
  type
  flags
  addr
  offset
  size
  link
  info
  addralign
  entsize)

(defun read-section-header (stream ident)
  (let ((little-endian-p (ident-little-endian-p ident)))
    (let ((h (make-section-header)))
      (setf (section-header-name h)
            (read-int32 stream :little-endian-p little-endian-p))
      (setf (section-header-type h)
            (read-int32 stream :little-endian-p little-endian-p))
      (setf (section-header-flags h)
            (read-int64 stream :little-endian-p little-endian-p))
      (setf (section-header-addr h)
            (read-int64 stream :little-endian-p little-endian-p))
      (setf (section-header-offset h)
            (read-int64 stream :little-endian-p little-endian-p))
      (setf (section-header-size h)
            (read-int64 stream :little-endian-p little-endian-p))
      (setf (section-header-link h)
            (read-int32 stream :little-endian-p little-endian-p))
      (setf (section-header-info h)
            (read-int32 stream :little-endian-p little-endian-p))
      (setf (section-header-addralign h)
            (read-int64 stream :little-endian-p little-endian-p))
      (setf (section-header-entsize h)
            (read-int64 stream :little-endian-p little-endian-p))
      h)))

(defun read-section-header-list (stream file-header)
  (let ((ident (file-header-ident file-header))
        (shnum (file-header-shnum file-header)))
    (loop repeat shnum
          collect (read-section-header stream ident))))

(defstruct string-table
  seq)

(defun read-string-table (stream file-header section-header-list)
  (let ((shstr (nth (file-header-shstrndx file-header)
                    section-header-list)))
    (file-position stream (section-header-offset shstr))
    (let ((seq (make-array (section-header-size shstr)
                           :element-type '(unsigned-byte 8))))
      (read-sequence seq stream)
      (make-string-table :seq seq))))

(defun string-table-string-list (string-table)
  (let ((seq (string-table-seq string-table))
        (str-list nil)
        (prev-null-index 0))
    (loop for index from 1 to (1- (length seq)) do
      (when (= (aref seq index) 0)
        (let ((str (babel:octets-to-string
                    seq
                    :start (1+ prev-null-index)
                    :end index)))
          (push str str-list)
          (setf prev-null-index index))))
    (nreverse str-list)))

(defun string-table-string-from (string-table start)
  (let ((seq (string-table-seq string-table)))
    (loop for end from (1+ start) to (1- (length seq))
          when (= (aref seq end) 0)
            return (babel:octets-to-string
                    seq :start start :end end))))

(defun section-header-string-name (sh string-table)
  (string-table-string-from string-table (section-header-name sh)))

(defstruct elf
  file-header
  section-header-list
  string-table)

(defun read-file (path)
  (with-open-file (in-stream path
                   :element-type '(unsigned-byte 8))
    (let ((file-header (read-file-header in-stream)))
      (let ((section-header-list (read-section-header-list
                                  in-stream
                                  file-header)))
        (let ((string-table (read-string-table
                             in-stream
                             file-header
                             section-header-list)))
          (make-elf :file-header file-header
                    :section-header-list section-header-list
                    :string-table string-table))))))

(defun read-text (path elf)
  (let ((sh-text (find-if (lambda (sh)
                            (string= (section-header-string-name
                                      sh (elf-string-table elf))
                                     ".text"))
                          (elf-section-header-list elf))))
    (when sh-text
      (with-open-file (in-stream path
                       :element-type '(unsigned-byte 8))
        (file-position in-stream (section-header-offset sh-text))
        (let ((seq (make-array (section-header-size sh-text))))
          (read-sequence seq in-stream)
          seq)))))
