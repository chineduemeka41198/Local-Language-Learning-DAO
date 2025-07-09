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

(define-map LearnerProgress
    {
        learner: principal,
        content-id: uint,
    }
    {
        completed: bool,
        completion-date: uint,
        score: uint,
        time-spent: uint,
    }
)

(define-map LearnerStats
    principal
    {
        total-completed: uint,
        current-streak: uint,
        longest-streak: uint,
        last-activity: uint,
        total-score: uint,
        languages-studied: (list 10 (string-ascii 10)),
    }
)

(define-map Achievements
    {
        learner: principal,
        achievement-type: (string-ascii 20),
    }
    {
        earned-date: uint,
        description: (string-ascii 100),
    }
)

(define-data-var streak-threshold uint u86400)

(define-public (complete-content
        (content-id uint)
        (score uint)
        (time-spent uint)
        (language (string-ascii 10))
    )
    (let (
            (progress-key {
                learner: tx-sender,
                content-id: content-id,
            })
            (current-stats (default-to {
                total-completed: u0,
                current-streak: u0,
                longest-streak: u0,
                last-activity: u0,
                total-score: u0,
                languages-studied: (list),
            }
                (map-get? LearnerStats tx-sender)
            ))
            (current-time stacks-block-height)
            (time-diff (- current-time (get last-activity current-stats)))
            (new-streak (if (<= time-diff (var-get streak-threshold))
                (+ (get current-streak current-stats) u1)
                u1
            ))
            (updated-languages (unwrap-panic (as-max-len? (append (get languages-studied current-stats) language)
                u10
            )))
        )
        (asserts!
            (not (default-to false
                (get completed (map-get? LearnerProgress progress-key))
            ))
            ERR-ALREADY-EXISTS
        )
        (map-set LearnerProgress progress-key {
            completed: true,
            completion-date: current-time,
            score: score,
            time-spent: time-spent,
        })
        (map-set LearnerStats tx-sender {
            total-completed: (+ (get total-completed current-stats) u1),
            current-streak: new-streak,
            longest-streak: (if (> new-streak (get longest-streak current-stats))
                new-streak
                (get longest-streak current-stats)
            ),
            last-activity: current-time,
            total-score: (+ (get total-score current-stats) score),
            languages-studied: updated-languages,
        })
        (unwrap-panic (check-and-award-achievements tx-sender new-streak
            (+ (get total-completed current-stats) u1)
        ))
        (ok true)
    )
)

(define-private (check-and-award-achievements
        (learner principal)
        (streak uint)
        (total-completed uint)
    )
    (begin
        (if (is-eq streak u7)
            (unwrap-panic (award-achievement learner "week-streak" "Completed 7 days in a row"))
            true
        )
        (if (is-eq total-completed u10)
            (unwrap-panic (award-achievement learner "content-master"
                "Completed 10 pieces of content"
            ))
            true
        )
        (if (is-eq total-completed u50)
            (unwrap-panic (award-achievement learner "learning-champion"
                "Completed 50 pieces of content"
            ))
            true
        )
        (ok true)
    )
)

(define-private (award-achievement
        (learner principal)
        (achievement-type (string-ascii 20))
        (description (string-ascii 100))
    )
    (let (
            (achievement-key {
                learner: learner,
                achievement-type: achievement-type,
            })
            (current-time stacks-block-height)
        )
        (if (is-none (map-get? Achievements achievement-key))
            (map-set Achievements achievement-key {
                earned-date: current-time,
                description: description,
            })
            false
        )
        (ok true)
    )
)

(define-read-only (get-learner-progress
        (learner principal)
        (content-id uint)
    )
    (ok (map-get? LearnerProgress {
        learner: learner,
        content-id: content-id,
    }))
)

(define-read-only (get-learner-stats (learner principal))
    (ok (default-to {
        total-completed: u0,
        current-streak: u0,
        longest-streak: u0,
        last-activity: u0,
        total-score: u0,
        languages-studied: (list),
    }
        (map-get? LearnerStats learner)
    ))
)

(define-read-only (get-learner-achievements (learner principal))
    (ok (list
        (map-get? Achievements {
            learner: learner,
            achievement-type: "week-streak",
        })
        (map-get? Achievements {
            learner: learner,
            achievement-type: "content-master",
        })
        (map-get? Achievements {
            learner: learner,
            achievement-type: "learning-champion",
        })
    ))
)

(define-public (update-streak-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set streak-threshold new-threshold)
        (ok true)
    )
)
(define-constant ERR-VOTING-ENDED (err u105))
(define-constant ERR-VOTING-ACTIVE (err u106))

(define-data-var proposal-counter uint u0)
(define-data-var voting-period uint u1008)
(define-data-var min-voting-power uint u1000)
(define-data-var quorum-threshold uint u5000)

(define-map Proposals
    uint
    {
        proposer: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        proposal-type: (string-ascii 20),
        target-value: uint,
        start-block: uint,
        end-block: uint,
        yes-votes: uint,
        no-votes: uint,
        executed: bool,
        passed: bool,
    }
)

(define-map Votes
    {
        proposal-id: uint,
        voter: principal,
    }
    {
        vote: bool,
        voting-power: uint,
        block-height: uint,
    }
)

(define-map VotingPower
    principal
    {
        power: uint,
        last-updated: uint,
    }
)

(define-public (create-proposal
        (title (string-ascii 100))
        (description (string-ascii 500))
        (proposal-type (string-ascii 20))
        (target-value uint)
    )
    (let (
            (proposal-id (+ (var-get proposal-counter) u1))
            (voter-power (get power
                (default-to {
                    power: u0,
                    last-updated: u0,
                }
                    (map-get? VotingPower tx-sender)
                )))
            (start-block stacks-block-height)
            (end-block (+ stacks-block-height (var-get voting-period)))
        )
        (asserts! (>= voter-power (var-get min-voting-power)) ERR-NOT-AUTHORIZED)
        (map-set Proposals proposal-id {
            proposer: tx-sender,
            title: title,
            description: description,
            proposal-type: proposal-type,
            target-value: target-value,
            start-block: start-block,
            end-block: end-block,
            yes-votes: u0,
            no-votes: u0,
            executed: false,
            passed: false,
        })
        (var-set proposal-counter proposal-id)
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal (unwrap! (map-get? Proposals proposal-id) ERR-NOT-FOUND))
            (vote-key {
                proposal-id: proposal-id,
                voter: tx-sender,
            })
            (voter-power (get power
                (default-to {
                    power: u0,
                    last-updated: u0,
                }
                    (map-get? VotingPower tx-sender)
                )))
        )
        (asserts! (< stacks-block-height (get end-block proposal))
            ERR-VOTING-ENDED
        )
        (asserts! (>= stacks-block-height (get start-block proposal))
            ERR-INVALID-INPUT
        )
        (asserts! (> voter-power u0) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? Votes vote-key)) ERR-ALREADY-EXISTS)
        (map-set Votes vote-key {
            vote: vote,
            voting-power: voter-power,
            block-height: stacks-block-height,
        })
        (map-set Proposals proposal-id
            (merge proposal {
                yes-votes: (if vote
                    (+ (get yes-votes proposal) voter-power)
                    (get yes-votes proposal)
                ),
                no-votes: (if vote
                    (get no-votes proposal)
                    (+ (get no-votes proposal) voter-power)
                ),
            })
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? Proposals proposal-id) ERR-NOT-FOUND)))
        (asserts! (>= stacks-block-height (get end-block proposal))
            ERR-VOTING-ACTIVE
        )
        (asserts! (not (get executed proposal)) ERR-ALREADY-EXISTS)
        (let (
                (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
                (passed (and
                    (>= total-votes (var-get quorum-threshold))
                    (> (get yes-votes proposal) (get no-votes proposal))
                ))
            )
            (map-set Proposals proposal-id
                (merge proposal {
                    executed: true,
                    passed: passed,
                })
            )
            (if passed
                (unwrap-panic (execute-proposal-action proposal))
                true
            )
            (ok passed)
        )
    )
)

(define-private (execute-proposal-action (proposal {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: (string-ascii 20),
    target-value: uint,
    start-block: uint,
    end-block: uint,
    yes-votes: uint,
    no-votes: uint,
    executed: bool,
    passed: bool,
}))
    (let ((prop-type (get proposal-type proposal)))
        (if (is-eq prop-type "update-content-fee")
            (begin
                (var-set content-fee (get target-value proposal))
                (ok true)
            )
            (if (is-eq prop-type "update-verification-threshold")
                (begin
                    (var-set verification-threshold (get target-value proposal))
                    (ok true)
                )
                (if (is-eq prop-type "update-quorum")
                    (begin
                        (var-set quorum-threshold (get target-value proposal))
                        (ok true)
                    )
                    (if (is-eq prop-type "update-period")
                        (begin
                            (var-set voting-period (get target-value proposal))
                            (ok true)
                        )
                        (ok true)
                    )
                )
            )
        )
    )
)

(define-public (update-voting-power
        (user principal)
        (new-power uint)
    )
    (begin
        (map-set VotingPower user {
            power: new-power,
            last-updated: stacks-block-height,
        })
        (ok true)
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (ok (unwrap! (map-get? Proposals proposal-id) ERR-NOT-FOUND))
)

(define-read-only (get-vote
        (proposal-id uint)
        (voter principal)
    )
    (ok (map-get? Votes {
        proposal-id: proposal-id,
        voter: voter,
    }))
)

(define-read-only (get-voting-power (user principal))
    (ok (default-to {
        power: u0,
        last-updated: u0,
    }
        (map-get? VotingPower user)
    ))
)

(define-read-only (get-governance-settings)
    (ok {
        voting-period: (var-get voting-period),
        min-voting-power: (var-get min-voting-power),
        quorum-threshold: (var-get quorum-threshold),
        proposal-counter: (var-get proposal-counter),
    })
)

(define-public (update-governance-settings
        (new-voting-period uint)
        (new-min-power uint)
        (new-quorum uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set voting-period new-voting-period)
        (var-set min-voting-power new-min-power)
        (var-set quorum-threshold new-quorum)
        (ok true)
    )
)
