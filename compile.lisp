(defpackage :nut.compiler
  (:use :cl)
  (:export :main))
(in-package :nut.compiler)

(defun function-definition-p (expr)
  (and (consp expr)
       (eq (nth 0 expr) 'defun)))

(defun function-definition-name (expr)
  (nth 1 expr))

(defun function-definition-args (expr)
  (nth 2 expr))

(defun function-definition-body (expr)
  ;; todo
  (nth 3 expr))

(defun make-function-definition (&key name args body)
  `(defun ,name ,args ,body))

(defun if-p (expr)
  (and (consp expr)
       (eq (nth 0 expr) 'if)))

(defun if-cond (expr)
  (nth 1 expr))

(defun if-then (expr)
  (nth 2 expr))

(defun if-else (expr)
  (nth 3 expr))

(defun call-p (expr)
  (consp expr))

(defun call-op (expr)
  (nth 0 expr))

(defun call-args (expr)
  (nthcdr 1 expr))

(defun lambda-p (expr)
  (and (consp expr)
       (eq (nth 0 expr) 'lambda)))

(defun lambda-args (expr)
  (nth 1 expr))

(defun lambda-body (expr)
  ;; todo
  (nth 2 expr))

(defun let-p (expr)
  (and (consp expr)
       (eq (nth 0 expr) 'let)))

(defun let-vars (expr)
  (mapcar #'first (nth 1 expr)))

(defun let-vals (expr)
  (mapcar #'second (nth 1 expr)))

(defun let-body (expr)
  ;; todo
  (nth 2 expr))

;;; compiler

(defun make-label (sym)
  (gensym sym))

(defstruct output
  strings
  statements)

(defun put-instructions (output insts)
  (setf (output-statements output)
        (append (output-statements output) insts)))

(defun put-label (output label)
  (setf (output-statements output)
        (append (output-statements output) (list label))))

(defun add-string (output label string)
  (push (cons label string)
        (output-strings output)))

(defun make-nasm-source (output stream global)
  (format stream "  bits 64~%")
  (format stream "  global ~A~%" (string-downcase (string global)))
  (format stream "
  extern is_cons
  extern car
  extern cdr
  extern number_int
  extern make_number
  extern is_symbol
  extern symbol_string
  extern strcmp
")
  (format stream "  section .data~%")
  (loop for (label . string) in (output-strings output) do
    (format stream "    ~A db \"~A\", 0~%" label string))
  (format stream "  section .text~%")
  (format stream "
int_eq:
  cmp rdi, rsi
  jne .not_eq
  mov rax, 1
  ret
.not_eq:
  mov rax, 0
  ret

apply_plus:
  push rbp
  mov rbp, rsp

  push rdi                ; args
  push 0                  ; retval

.loop:
  mov rdi, [rbp-8]
  call is_cons

  cmp rax, 1
  jne .loop_over

  mov rdi, [rbp-8]
  call car

  mov rdi, rax
  call number_int

  add [rbp-16], rax

  mov rdi, [rbp-8]
  call cdr

  mov [rbp-8], rax

  jmp .loop

.loop_over:
  pop rdi

  call make_number

  mov rsp, rbp
  pop rbp
  ret
")
  (dolist (statement (output-statements output))
    (cond ((symbolp statement)
           (format stream "~A:~%" (string-downcase (string statement))))
          ((consp statement)
           (destructuring-bind (operator &rest operands) statement
             (format stream "  ~A " operator)
             (loop for (type content) in operands
                   for first-p = t then nil do
               (progn
                 (when (not first-p)
                   (format stream ", "))
                 (ecase type
                   ((:reg :imm)
                    (format stream "~A" content))
                   ((:label)
                    (format stream "~A" (string-downcase
                                         (string content))))
                   ((:mem)
                    (format stream "[~A]" content))))))
           (format stream "~%")))))

(defstruct binding
  var reg)

(defstruct frame
  bindings)

(defun compile-expression (output expr frame)
  (assert expr)
  (cond ((numberp expr)
         (put-instructions
          output
          `((:mov (:reg :rax) (:imm ,expr)))))
        ((stringp expr)
         (let ((label (make-label "string_")))
           (add-string
            output label expr)
           (put-instructions
            output
            `((:mov (:reg :rax) (:label ,label))))))
        ((symbolp expr)
         (put-instructions
          output
          `((:mov (:reg :rax)
                  ,(binding-reg
                    (find expr
                          (frame-bindings frame)
                          :key #'binding-var
                          :test #'eq))))))
        ((function-definition-p expr)
         (compile-function-definition output expr))
        ((let-p expr)
         (compile-expression
          output `((lambda ,(let-vars expr)
                     ,(let-body expr))
                   ,@(let-vals expr))
          frame))
        ((if-p expr)
         (compile-if output expr frame))
        ((call-p expr)
         (compile-call output expr frame))))

(defun compile-function-definition (output expr)
  (let ((name (function-definition-name expr))
        (args (function-definition-args expr))
        (body (function-definition-body expr)))
    (assert (<= (length args) 2))
    (put-label
     output name)

    (put-instructions
     output `((:push (:reg :rbp))
              (:mov (:reg :rbp) (:reg :rsp))))

    (case (length args)
      (1
       (put-instructions
        output `((:push (:reg :rdi))))
       (compile-expression
        output body
        (make-frame
         :bindings (list (make-binding
                          :var (car args)
                          :reg '(:mem :rbp-8))))))
      (2
       (put-instructions
        output `((:push (:reg :rdi))
                 (:push (:reg :rsi))))
       (compile-expression
        output body
        (make-frame
         :bindings (list (make-binding
                          :var (car args)
                          :reg '(:mem :rbp-8))
                         (make-binding
                          :var (cadr args)
                          :reg '(:mem :rbp-16)))))))

    (put-instructions
     output `((:mov (:reg :rsp) (:reg :rbp))
              (:pop (:reg :rbp))
              (:ret)))))

(defun compile-call (output expr frame)
  (let ((op (call-op expr))
        (args (call-args expr))
        (label nil))
    (assert (<= (length args) 2))

    (cond ((symbolp op)
           (setq label op))
          ((lambda-p op)
           (let ((label-after-def (make-label "after_def_")))
             (setq label (gensym "anonymous_fn_"))
             (put-instructions
              output `((:jmp (:label ,label-after-def))))
             (compile-function-definition
              output (make-function-definition
                      :name label
                      :args (lambda-args op)
                      :body (lambda-body op)))
             (put-label
              output label-after-def))))

    (dolist (arg args)
      (compile-expression
       output arg frame)
      (put-instructions
       output `((:push (:reg :rax)))))
    (case (length args)
      (1 (put-instructions
          output `((:mov (:reg :rdi) (:mem :rsp)))))
      (2 (put-instructions
          output `((:mov (:reg :rdi) (:mem :rsp+8))
                   (:mov (:reg :rsi) (:mem :rsp))))))
    (put-instructions
     output `((:add (:reg :rsp) (:imm ,(* 8 (length args))))))
    (put-instructions
     output `((:call (:label ,label))))))

(defun compile-if (output if-expr frame)
  (let ((label-else (make-label "else_"))
        (label-after-if (make-label "after_if_")))
    (compile-expression
     output (if-cond if-expr) frame)
    (put-instructions
     output `((:cmp (:reg :rax) (:imm 0))
              (:je (:label ,label-else))))
    (compile-expression
     output (if-then if-expr) frame)
    (put-instructions
     output `((:jmp (:label ,label-after-if))))
    (put-label
     output label-else)
    (compile-expression
     output (if-else if-expr) frame)
    (put-label
     output label-after-if)))

(defun main ()
  (let ((o (make-output)))
    (compile-expression
     o '(defun nut_eval_arg_list (args) args) nil)
    (compile-expression
     o '(defun nut_eval (expr)
         (if (is_cons expr)
             (let ((op (car expr))
                   (args (nut_eval_arg_list (cdr expr))))
               (if (is_symbol op)
                   (if (int_eq (strcmp (symbol_string op) "+") 0)
                       (apply_plus args)
                       -1)
                   -2))
             -3)) nil)
    (with-open-file (out-stream "nut.s"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (make-nasm-source o out-stream 'nut_eval)))
  (values))
