#lang racket

(define-syntax (define-provide stx)
  (syntax-case stx ()
    [(_ id defn)
     #`(begin (define id defn) (provide id))]))

(struct RGB (r g b) #:transparent)
(struct RGB-linear (r g b) #:transparent)
(struct XYZ (x y z) #:transparent)
(struct LAB (l a b) #:transparent)

(provide RGB RGB-linear XYZ LAB)

(define-provide ->string (lambda (c)
  (format "rgb(~a,~a,~a)" (RGB-r c) (RGB-g c) (RGB-b c))))

(define-provide add-LAB (lambda (c1 c2)
  (LAB (+ (LAB-l c1) (LAB-l c2))
        (+ (LAB-a c1) (LAB-a c2))
        (+ (LAB-b c1) (LAB-b c2)))))

(define-provide D65-XYZ (XYZ 95.047 100.0 108.883))

(define-provide LAB->XYZ (lambda (in)
  (local
    [(define whitepoint D65-XYZ)
      (define (f t)
        (if (> (* t t t) 0.008856) (* t t t) (/ (- t 16/116) 7.787)))
      (define Y (/ (+ (LAB-l in) 16) 116))
      (define X (+ (/ (LAB-a in) 500) Y))
      (define Z (- Y (/ (LAB-b in) 200)))]
    (XYZ (* (XYZ-x whitepoint) (f X))
          (* (XYZ-y whitepoint) (f Y))
          (* (XYZ-z whitepoint) (f Z))))))

(define-provide XYZ->RGB-linear (lambda (in)
  (local
    [(define X (/ (XYZ-x in) 100))
      (define Y (/ (XYZ-y in) 100))
      (define Z (/ (XYZ-z in) 100))]
      (RGB-linear (+ (* 3.2406 X) (* -1.5372 Y) (* -0.4986 Z))
                  (+ (* -0.9689 X) (* 1.8758 Y) (* 0.0415 Z))
                  (+ (* 0.0557 X) (* -0.2040 Y) (* 1.0570 Z))))))

(define-provide delinearize-RGB (lambda (in)
  (local
    [(define (f x)
      (* 255 (if (> x 0.0031308) (- (* 1.055 (expt x (/ 1 2.4))) 0.055) (* 12.92 x))))
      (define (g x)
      (max 0 (min 255 (inexact->exact (round (f x))))))]
    (RGB (g (RGB-linear-r in)) (g (RGB-linear-g in)) (g (RGB-linear-b in))))))

(define-provide LAB->RGB (lambda (in)
  (delinearize-RGB (XYZ->RGB-linear (LAB->XYZ in)))))
