(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-INPUT (err u101))
(define-constant ERR-INSUFFICIENT-FUNDS (err u102))
(define-constant ERR-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))

(define-data-var dao-owner principal tx-sender)
(define-data-var content-fee uint u100)
(define-data-var verification-threshold uint u3)

(define-map Contributors
    principal
    {
        reputation: uint,
        earnings: uint,
    }
)

(define-map LearningContent
    uint
    {
        creator: principal,
        language: (string-ascii 10),
        content-hash: (string-ascii 64),
        verified: bool,
        verifications: uint,
        reward: uint,
    }
)

(define-map Verifications
    {
        content-id: uint,
        verifier: principal,
    }
    bool
)

(define-data-var content-counter uint u0)

(define-public (initialize-dao (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set dao-owner new-owner)
        (ok true)
    )
)

(define-public (submit-content
        (language (string-ascii 10))
        (content-hash (string-ascii 64))
        (reward uint)
    )
    (let ((content-id (+ (var-get content-counter) u1)))
        (asserts! (>= (stx-get-balance tx-sender) (var-get content-fee))
            ERR-INSUFFICIENT-FUNDS
        )
        (try! (stx-transfer? (var-get content-fee) tx-sender (var-get dao-owner)))
        (map-set LearningContent content-id {
            creator: tx-sender,
            language: language,
            content-hash: content-hash,
            verified: false,
            verifications: u0,
            reward: reward,
        })
        (var-set content-counter content-id)
        (ok content-id)
    )
)

(define-public (verify-content (content-id uint))
    (let (
            (content (unwrap! (map-get? LearningContent content-id) ERR-NOT-FOUND))
            (verification-key {
                content-id: content-id,
                verifier: tx-sender,
            })
        )
        (asserts! (not (is-eq (get creator content) tx-sender))
            ERR-NOT-AUTHORIZED
        )
        (asserts!
            (not (default-to false (map-get? Verifications verification-key)))
            ERR-ALREADY-EXISTS
        )
        (map-set Verifications verification-key true)
        (map-set LearningContent content-id
            (merge content {
                verifications: (+ (get verifications content) u1),
                verified: (>= (+ (get verifications content) u1)
                    (var-get verification-threshold)
                ),
            })
        )
        (ok true)
    )
)

(define-public (claim-reward (content-id uint))
    (let ((content (unwrap! (map-get? LearningContent content-id) ERR-NOT-FOUND)))
        (asserts! (is-eq (get creator content) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get verified content) ERR-NOT-AUTHORIZED)
        (try! (stx-transfer? (get reward content) (var-get dao-owner) tx-sender))
        (ok true)
    )
)

(define-public (update-contributor-reputation
        (contributor principal)
        (points uint)
    )
    (let ((current-data (default-to {
            reputation: u0,
            earnings: u0,
        }
            (map-get? Contributors contributor)
        )))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (map-set Contributors contributor
            (merge current-data { reputation: (+ (get reputation current-data) points) })
        )
        (ok true)
    )
)

(define-read-only (get-content-details (content-id uint))
    (ok (unwrap! (map-get? LearningContent content-id) ERR-NOT-FOUND))
)

(define-read-only (get-contributor-stats (contributor principal))
    (ok (default-to {
        reputation: u0,
        earnings: u0,
    }
        (map-get? Contributors contributor)
    ))
)

(define-public (update-dao-settings
        (new-fee uint)
        (new-threshold uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set content-fee new-fee)
        (var-set verification-threshold new-threshold)
        (ok true)
    )
)
