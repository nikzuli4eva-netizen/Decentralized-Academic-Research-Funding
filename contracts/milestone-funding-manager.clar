;; milestone-funding-manager
;; Milestone-defined grants with logical escrow accounting and verifier-gated release.

;; --------------------
;; Errors
;; --------------------
(define-constant ERR-NOT-OWNER (err u200))
(define-constant ERR-GRANT-NOT-FOUND (err u201))
(define-constant ERR-NOT-GRANT-OWNER (err u202))
(define-constant ERR-BAD-MILESTONE (err u203))
(define-constant ERR-ALREADY-VERIFIED (err u204))
(define-constant ERR-OUT-OF-ORDER (err u205))
(define-constant ERR-NO-AUTH (err u206))

;; --------------------
;; Ownership
;; --------------------
(define-data-var contract-owner principal tx-sender)

(define-private (assert-admin (who principal))
  (if (is-eq who (var-get contract-owner))
      (ok true)
      ERR-NOT-OWNER
  )
)

;; --------------------
(define-data-var next-grant-id uint u1)

;; grants: id -> { owner, title, description, total, current, active, escrow, released }
(define-map grants
  { id: uint }
  { owner: principal,
    title: (string-ascii 100),
    description: (string-utf8 500),
    total: uint,
    current: uint,
    active: bool,
    escrow: uint,
    released: uint })

;; milestones: { id, idx } -> { title, amount, verified }
(define-map milestones
  { id: uint, idx: uint }
  { title: (string-ascii 100), amount: uint, verified: bool })

;; verifiers per grant
(define-map verifiers
  { id: uint, who: principal }
  { allowed: bool })

;; sponsor accounting (logical balances)
(define-map sponsors
  { id: uint, who: principal }
  { amount: uint })

;; --------------------
;; Helpers
;; --------------------
(define-read-only (grant-exists (gid uint))
  (is-some (map-get? grants { id: gid }))
)

(define-read-only (grant-of (gid uint))
  (match (map-get? grants { id: gid }) g (ok g) (err u404))
)

(define-read-only (is-verifier (gid uint) (who principal))
  (match (map-get? verifiers { id: gid, who: who })
    v (get allowed v)
    false)
)

;; --------------------
;; Grant lifecycle
;; --------------------
(define-public (create-grant (title (string-ascii 100)) (description (string-utf8 500)))
  (let ((gid (var-get next-grant-id)))
    (begin
      (var-set next-grant-id (+ gid u1))
      (map-set grants { id: gid }
        { owner: tx-sender, title: title, description: description,
          total: u0, current: u0, active: true, escrow: u0, released: u0 })
      (ok gid)
    )
  )
)

(define-public (add-milestone (gid uint) (title (string-ascii 100)) (amount uint))
  (let ((g (unwrap! (map-get? grants { id: gid }) ERR-GRANT-NOT-FOUND)))
    (begin
      (unwrap-panic (assert-admin (var-get contract-owner)))
      (let ((idx (+ (get total g) u1)))
        (begin
          (map-set milestones { id: gid, idx: idx } { title: title, amount: amount, verified: false })
          (map-set grants { id: gid } (merge g { total: idx }))
          (ok idx)
        )
      )
    )
  )
)

(define-public (add-verifier (gid uint) (who principal))
  (let ((g (unwrap! (map-get? grants { id: gid }) ERR-GRANT-NOT-FOUND)))
    (begin
      (unwrap-panic (if (is-eq tx-sender (get owner g)) (ok true) ERR-NOT-GRANT-OWNER))
      (map-set verifiers { id: gid, who: who } { allowed: true })
      (ok true)
    )
  )
)

(define-public (remove-verifier (gid uint) (who principal))
  (let ((g (unwrap! (map-get? grants { id: gid }) ERR-GRANT-NOT-FOUND)))
    (begin
      (unwrap-panic (if (is-eq tx-sender (get owner g)) (ok true) ERR-NOT-GRANT-OWNER))
      (map-delete verifiers { id: gid, who: who })
      (ok true)
    )
  )
)

;; Logical funding (no STX transfer)
(define-public (fund-grant (gid uint) (amount uint))
  (let ((g (unwrap! (map-get? grants { id: gid }) ERR-GRANT-NOT-FOUND)))
    (begin
(map-set sponsors { id: gid, who: tx-sender }
{ amount: (+ amount (get amount (default-to { amount: u0 } (map-get? sponsors { id: gid, who: tx-sender }))) ) })
      (map-set grants { id: gid } (merge g { escrow: (+ (get escrow g) amount) }))
      (ok (get escrow (unwrap! (map-get? grants { id: gid }) ERR-GRANT-NOT-FOUND)))
    )
  )
)

(define-public (verify-milestone (gid uint) (idx uint))
  (let ((g (unwrap! (map-get? grants { id: gid }) ERR-GRANT-NOT-FOUND)))
    (if (is-verifier gid tx-sender)
        (begin
(let ((expected (+ (get current g) u1)))
            (unwrap! (if (is-eq idx expected) (ok true) (err u205)) (err u205))
          )
          (let ((m (unwrap! (map-get? milestones { id: gid, idx: idx }) ERR-BAD-MILESTONE)))
(unwrap! (if (get verified m) (err u204) (ok true)) (err u204))
            (map-set milestones { id: gid, idx: idx } (merge m { verified: true }))
            (let ((amount (get amount m)) (escrow (get escrow g)))
              (if (>= escrow amount)
                  (begin
                    (map-set grants { id: gid } (merge g { escrow: (- escrow amount), released: (+ (get released g) amount), current: (+ (get current g) u1) }))
                    (ok amount)
                  )
                  (ok u0)
              )
            )
          )
        )
        (err u206))
  )
)

(define-public (finalize-grant (gid uint))
  (let ((g (unwrap! (map-get? grants { id: gid }) ERR-GRANT-NOT-FOUND)))
    (begin
      (unwrap-panic (if (is-eq tx-sender (get owner g)) (ok true) ERR-NOT-GRANT-OWNER))
      (map-set grants { id: gid } (merge g { active: false }))
      (ok true)
    )
  )
)

;; --------------------
;; Views
;; --------------------
(define-read-only (get-grant (gid uint))
  (match (map-get? grants { id: gid }) g (ok g) (err u404))
)

(define-read-only (get-milestone (gid uint) (idx uint))
  (match (map-get? milestones { id: gid, idx: idx }) m (ok m) (err u404))
)

(define-read-only (sponsor-of (gid uint) (who principal))
(ok (default-to { amount: u0 } (map-get? sponsors { id: gid, who: who })))
)
