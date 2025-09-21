;; peer-review-coordinator
;; Coordinated anonymous-leaning peer review with admin assignment and simple reputation.

;; --------------------
;; Errors
;; --------------------
(define-constant ERR-NOT-OWNER (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-REGISTERED (err u102))
(define-constant ERR-INACTIVE-REVIEWER (err u103))
(define-constant ERR-BAD-SUBMISSION (err u104))
(define-constant ERR-ALREADY-ASSIGNED (err u105))
(define-constant ERR-NOT-ASSIGNED (err u106))
(define-constant ERR-ALREADY-SUBMITTED (err u107))
(define-constant ERR-BAD-SCORE (err u108))
(define-constant ERR-FINALIZED (err u109))

;; --------------------
;; Ownership
;; --------------------
(define-data-var contract-owner principal tx-sender)

(define-read-only (get-owner)
  (ok (var-get contract-owner))
)

(define-private (assert-owner (who principal))
  (if (is-eq who (var-get contract-owner))
      (ok true)
      ERR-NOT-OWNER
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (unwrap-panic (assert-owner tx-sender))
    (var-set contract-owner new-owner)
    (ok new-owner)
  )
)

;; --------------------
;; State
;; --------------------
(define-data-var next-submission-id uint u1)

;; reviewer registry: reviewer -> { name, reputation, active }
(define-map reviewers
  { who: principal }
  { name: (string-ascii 50), reputation: int, active: bool }
)

;; submissions: id -> { owner, title, description, max-reviewers, status }
(define-map submissions
  { id: uint }
  { owner: principal,
    title: (string-ascii 100),
    description: (string-utf8 500),
    max: uint,
    status: uint } ;; u0=open, u1=finalized
)

;; reviewer assignments and reviews per submission
(define-map assignments
  { submission: uint, reviewer: principal }
  { assigned: bool,
    submitted: bool,
    score: int,
    comment-hash: (buff 32),
    anon-handle: (buff 32) }
)

;; per-submission stats
(define-map submission-stats
  { id: uint }
  { assigned: uint, submitted: uint, total-score: int }
)

;; --------------------
;; Utilities
;; --------------------
(define-read-only (is-registered (who principal))
  (is-some (map-get? reviewers { who: who }))
)

(define-read-only (is-active (who principal))
  (match (map-get? reviewers { who: who })
    r (get active r)
    false)
)

(define-read-only (get-submission (id uint))
  (match (map-get? submissions { id: id })
    entry (ok entry)
    (err u404)
  )
)

(define-read-only (get-stats (id uint))
  (match (map-get? submission-stats { id: id })
    entry (ok entry)
(ok { assigned: u0, submitted: u0, total-score: 0 })
  )
)

;; --------------------
;; Reviewer management
;; --------------------
(define-public (register-reviewer (name (string-ascii 50)))
  (begin
    (if (is-some (map-get? reviewers { who: tx-sender }))
        (err u101)
        (begin
(map-set reviewers { who: tx-sender } { name: name, reputation: 0, active: true })
          (ok true)
        )
    )
  )
)

(define-public (set-reviewer-reputation (who principal) (rep int))
  (begin
    (unwrap-panic (assert-owner tx-sender))
    (match (map-get? reviewers { who: who })
entry (begin (map-set reviewers { who: who } (merge entry { reputation: rep })) (ok rep))
      (err u102)
    )
  )
)

(define-public (set-reviewer-active (who principal) (active bool))
  (begin
    (unwrap-panic (assert-owner tx-sender))
    (match (map-get? reviewers { who: who })
entry (begin (map-set reviewers { who: who } (merge entry { active: active })) (ok active))
      (err u102)
    )
  )
)

;; --------------------
;; Submission lifecycle
;; --------------------
(define-public (create-submission (title (string-ascii 100)) (description (string-utf8 500)) (max-reviewers uint))
  (let
    (
      (sid (var-get next-submission-id))
    )
    (begin
      (var-set next-submission-id (+ sid u1))
      (map-set submissions { id: sid }
        { owner: tx-sender, title: title, description: description, max: max-reviewers, status: u0 })
(map-set submission-stats { id: sid } { assigned: u0, submitted: u0, total-score: 0 })
      (ok sid)
    )
  )
)

(define-public (assign-reviewer (submission-id uint) (who principal))
  (begin
    (unwrap-panic (assert-owner tx-sender))
    (match (map-get? submissions { id: submission-id })
      submission
      (if (is-eq (get status submission) u1)
(err u109)
(if (not (is-active who))
              (err u103)
              (if (is-some (map-get? assignments { submission: submission-id, reviewer: who }))
(err u105)
                  (begin
                    (map-set assignments { submission: submission-id, reviewer: who }
                      { assigned: true, submitted: false, score: 0,
                        comment-hash: 0x0000000000000000000000000000000000000000000000000000000000000000,
                        anon-handle: 0x0000000000000000000000000000000000000000000000000000000000000000 })
(let ((st (default-to { assigned: u0, submitted: u0, total-score: 0 } (map-get? submission-stats { id: submission-id }))))
                      (map-set submission-stats { id: submission-id } (merge st { assigned: (+ (get assigned st) u1) })))
                    (ok true)
                  )
              )
          )
      )
      (err u104)
    )
  )
)

(define-public (submit-review (submission-id uint) (score int) (comment-hash (buff 32)) (anon-handle (buff 32)))
  (begin
    (match (map-get? submissions { id: submission-id })
      submission
      (if (is-eq (get status submission) u1)
(err u109)
          (begin
(unwrap! (if (or (< score 0) (> score 100)) (err u108) (ok true)) (err u108))
            (match (map-get? assignments { submission: submission-id, reviewer: tx-sender })
              a
              (if (get submitted a)
(err u107)
                  (begin
                    (map-set assignments { submission: submission-id, reviewer: tx-sender }
                      (merge a { submitted: true, score: score, comment-hash: comment-hash, anon-handle: anon-handle }))
(let ((st (default-to { assigned: u0, submitted: u0, total-score: 0 } (map-get? submission-stats { id: submission-id }))))
                      (map-set submission-stats { id: submission-id } { assigned: (get assigned st), submitted: (+ (get submitted st) u1), total-score: (+ (get total-score st) score) })
                    )
                    (ok true)
                  )
              )
(err u106)
            )
          )
      )
(err u104)
    )
  )
)

(define-public (finalize-submission (submission-id uint))
  (begin
    (unwrap-panic (assert-owner tx-sender))
    (match (map-get? submissions { id: submission-id })
      submission
      (begin
        (if (is-eq (get status submission) u1)
(err u109)
            (begin
              (map-set submissions { id: submission-id } (merge submission { status: u1 }))
              (ok true)
            )
        )
      )
      (err u104)
    )
  )
)

;; --------------------
;; Queries
;; --------------------
(define-read-only (get-review (submission-id uint) (who principal))
  (match (map-get? assignments { submission: submission-id, reviewer: who })
    a (ok a)
    (err u404)
  )
)

(define-read-only (average-score (submission-id uint))
  (let ((st (unwrap! (get-stats submission-id) (err u0))))
    (if (is-eq (get submitted st) u0)
(ok 0)
        (ok (/ (get total-score st) (to-int (get submitted st))))
    )
  )
)

(define-read-only (submission-status (submission-id uint))
  (match (map-get? submissions { id: submission-id })
    s (ok (get status s))
    (err u404)
  )
)
