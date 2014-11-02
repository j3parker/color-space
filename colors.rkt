(module colors racket
  (struct RGB (r g b) #:transparent)
  (struct RGB-linear (r g b) #:transparent)
  (struct XYZ (x y z) #:transparent)
  (struct LAB (l a b) #:transparent)

  (provide LAB RGB)

  (define (->string c)
    (format "rgb(~a,~a,~a)" (RGB-r c) (RGB-g c) (RGB-b c)))
  (provide ->string)

  (define (add-LAB c1 c2)
    (LAB (+ (LAB-l c1) (LAB-l c2))
         (+ (LAB-a c1) (LAB-a c2))
         (+ (LAB-b c1) (LAB-b c2))))

  (define D65-XYZ (XYZ 95.047 100.0 108.883))

  (define (LAB->XYZ in)
    (local
      [(define whitepoint D65-XYZ)
       (define (f t)
         (if (> (* t t t) 0.008856) (* t t t) (/ (- t 16/116) 7.787)))
       (define Y (/ (+ (LAB-l in) 16) 116))
       (define X (+ (/ (LAB-a in) 500) Y))
       (define Z (- Y (/ (LAB-b in) 200)))]
      (XYZ (* (XYZ-x whitepoint) (f X))
           (* (XYZ-y whitepoint) (f Y))
           (* (XYZ-z whitepoint) (f Z)))))

  (define (XYZ->RGB-linear in)
    (local
      [(define X (/ (XYZ-x in) 100))
       (define Y (/ (XYZ-y in) 100))
       (define Z (/ (XYZ-z in) 100))]
       (RGB-linear (+ (* 3.2406 X) (* -1.5372 Y) (* -0.4986 Z))
                   (+ (* -0.9689 X) (* 1.8758 Y) (* 0.0415 Z))
                   (+ (* 0.0557 X) (* -0.2040 Y) (* 1.0570 Z)))))

  (define (delinearize-RGB in)
    (local
      [(define (f x)
        (* 255 (if (> x 0.0031308) (- (* 1.055 (expt x (/ 1 2.4))) 0.055) (* 12.92 x))))
       (define (g x)
        (max 0 (min 255 (inexact->exact (round (f x))))))]
      (RGB (g (RGB-linear-r in)) (g (RGB-linear-g in)) (g (RGB-linear-b in)))))

  (define (LAB->RGB in)
    (delinearize-RGB (XYZ->RGB-linear (LAB->XYZ in))))
  (provide LAB->RGB)

  (define-syntax (define-color stx)
    (syntax-case stx ()
      [(_ id val) #'(begin
                      (define id (LAB->RGB val))
                      (provide id))]))

  (define colors '())
  (provide colors)

  (define-syntax (define-color-variants stx)
    (syntax-case stx ()
      [(_ id val)
        (with-syntax
          ([id-light (datum->syntax #'id (string->symbol (format "bright-~a" (syntax->datum #'id))))]
           [id-dark (datum->syntax #'id (string->symbol (format "dark-~a" (syntax->datum #'id))))])
          #`(begin
              (define-color id val)
              (define-color id-light (add-LAB (LAB 25 0 0) val))
              (define-color id-dark (add-LAB (LAB -25 0 0) val)))
          )]))

  (define-color base03 (LAB 11 -7 -12))
  (define-color base02 (LAB 20 -7 -12))
  (define-color base01 (LAB 45 -7 -7))
  (define-color base00 (LAB 50 -7 -7))
  (define-color base0 (LAB 60 -6 -3))
  (define-color base1 (LAB 65 -5 -2))
  (define-color base2 (LAB 80 -2 -1))
  (define-color base3 (LAB 97 -1 -1))

  (define-color-variants yellow (LAB 70 10 65))
  (define-color-variants orange (LAB 65 45 300))
  (define-color-variants red (LAB 50 65 45))
  (define-color-variants magenta (LAB 50 65 -05))
  (define-color-variants violet (LAB 50 15 -45))
  (define-color-variants blue (LAB 55 -10 -45))
  (define-color-variants cyan (LAB 60 -35 -05))
  (define-color-variants green (LAB 60 -25 65))

  (define fg 'not-set)
  (define fg-secondary 'not-set)
  (define bg 'not-set)
  (define bg-secondary 'not-set)
  (define muted 'not-set)

  (define (set-mode mode)
    (cond
      [(equal? mode 'light) (set! fg base03)
                            (set! fg-secondary base00)
                            (set! bg base3)
                            (set! bg-secondary base2)
                            (set! muted base2)]

      [(equal? mode 'dark) (set! fg base0)
                           (set! fg-secondary base01)
                           (set! bg base03)
                           (set! bg-secondary base02)
                           (set! muted base02)]))
  (provide fg fg-secondary bg bg-secondary muted set-mode)

  (define (to-css c) (~a "rgb(" (RGB-r c) ", " (RGB-g c) ", " (RGB-b c) ");"))
  (provide to-css)
)
