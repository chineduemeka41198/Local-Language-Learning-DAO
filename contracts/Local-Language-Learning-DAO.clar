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
(define-constant ERR-INSUFFICIENT-EXPERIENCE (err u107))
(define-constant ERR-CERTIFICATION-EXISTS (err u108))
(define-constant ERR-CONTENT-NOT-PREMIUM (err u109))
(define-constant ERR-ALREADY-PURCHASED (err u110))

(define-data-var proposal-counter uint u0)
(define-data-var voting-period uint u1008)
(define-data-var min-voting-power uint u1000)
(define-data-var quorum-threshold uint u5000)

(define-data-var certification-counter uint u0)
(define-data-var min-score-for-cert uint u8000)
(define-data-var min-content-for-cert uint u20)

(define-data-var marketplace-fee-rate uint u500)
(define-data-var total-marketplace-volume uint u0)

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

(define-map Certifications
    uint
    {
        holder: principal,
        language: (string-ascii 10),
        level: (string-ascii 20),
        score: uint,
        content-completed: uint,
        issued-date: uint,
        issuer: principal,
        verification-hash: (string-ascii 64),
    }
)

(define-map CertificationHolders
    {
        holder: principal,
        language: (string-ascii 10),
        level: (string-ascii 20),
    }
    uint
)

(define-map ContentPricing
    uint
    {
        price: uint,
        is-premium: bool,
        sales-count: uint,
        total-revenue: uint,
    }
)

(define-map ContentPurchases
    {
        buyer: principal,
        content-id: uint,
    }
    {
        purchase-date: uint,
        price-paid: uint,
    }
)

(define-map CreatorEarnings
    principal
    {
        total-earned: uint,
        total-sales: uint,
        content-sold: uint,
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

(define-public (issue-certification
        (candidate principal)
        (language (string-ascii 10))
        (level (string-ascii 20))
        (verification-hash (string-ascii 64))
    )
    (let (
            (cert-key {
                holder: candidate,
                language: language,
                level: level,
            })
            (learner-stats (unwrap! (map-get? LearnerStats candidate) ERR-NOT-FOUND))
            (cert-id (+ (var-get certification-counter) u1))
            (current-time stacks-block-height)
        )
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts!
            (>= (get total-score learner-stats) (var-get min-score-for-cert))
            ERR-INSUFFICIENT-EXPERIENCE
        )
        (asserts!
            (>= (get total-completed learner-stats)
                (var-get min-content-for-cert)
            )
            ERR-INSUFFICIENT-EXPERIENCE
        )
        (asserts! (is-none (map-get? CertificationHolders cert-key))
            ERR-CERTIFICATION-EXISTS
        )
        (map-set Certifications cert-id {
            holder: candidate,
            language: language,
            level: level,
            score: (get total-score learner-stats),
            content-completed: (get total-completed learner-stats),
            issued-date: current-time,
            issuer: tx-sender,
            verification-hash: verification-hash,
        })
        (map-set CertificationHolders cert-key cert-id)
        (var-set certification-counter cert-id)
        (ok cert-id)
    )
)

(define-public (revoke-certification (cert-id uint))
    (let ((certification (unwrap! (map-get? Certifications cert-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (map-delete Certifications cert-id)
        (map-delete CertificationHolders {
            holder: (get holder certification),
            language: (get language certification),
            level: (get level certification),
        })
        (ok true)
    )
)

(define-public (update-certification-requirements
        (new-min-score uint)
        (new-min-content uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set min-score-for-cert new-min-score)
        (var-set min-content-for-cert new-min-content)
        (ok true)
    )
)

(define-read-only (get-certification (cert-id uint))
    (ok (unwrap! (map-get? Certifications cert-id) ERR-NOT-FOUND))
)

(define-read-only (get-user-certification
        (holder principal)
        (language (string-ascii 10))
        (level (string-ascii 20))
    )
    (let ((cert-key {
            holder: holder,
            language: language,
            level: level,
        }))
        (match (map-get? CertificationHolders cert-key)
            cert-id (map-get? Certifications cert-id)
            none
        )
    )
)

(define-read-only (verify-certification
        (cert-id uint)
        (expected-hash (string-ascii 64))
    )
    (let ((certification (unwrap! (map-get? Certifications cert-id) ERR-NOT-FOUND)))
        (ok (is-eq (get verification-hash certification) expected-hash))
    )
)

(define-read-only (get-certification-requirements)
    (ok {
        min-score: (var-get min-score-for-cert),
        min-content: (var-get min-content-for-cert),
        certification-counter: (var-get certification-counter),
    })
)

(define-read-only (check-certification-eligibility (candidate principal))
    (let ((learner-stats (unwrap! (map-get? LearnerStats candidate) ERR-NOT-FOUND)))
        (ok {
            eligible: (and
                (>= (get total-score learner-stats) (var-get min-score-for-cert))
                (>= (get total-completed learner-stats)
                    (var-get min-content-for-cert)
                )
            ),
            current-score: (get total-score learner-stats),
            current-completed: (get total-completed learner-stats),
            score-needed: (var-get min-score-for-cert),
            content-needed: (var-get min-content-for-cert),
        })
    )
)

;; === LANGUAGE EXCHANGE MATCHMAKING SYSTEM ===
;; New independent feature for connecting language learners

;; Additional error constants for exchange system
(define-constant ERR-PROFILE-NOT-FOUND (err u111))
(define-constant ERR-NO-MATCH-FOUND (err u112))
(define-constant ERR-SESSION-NOT-FOUND (err u113))
(define-constant ERR-SESSION-ALREADY-RATED (err u114))
(define-constant ERR-INVALID-RATING (err u115))
(define-constant ERR-SESSION-NOT-COMPLETED (err u116))
(define-constant ERR-CANNOT-MATCH-SELF (err u117))
(define-constant ERR-TIME-SLOT-TAKEN (err u118))

;; Data variables for exchange system
(define-data-var session-counter uint u0)
(define-data-var min-reputation-for-matching uint u3)
(define-data-var session-duration-blocks uint u144)

;; Language Exchange Profile Map
(define-map LanguageProfiles
    principal
    {
        native-languages: (list 3 (string-ascii 10)),
        learning-languages: (list 5 (string-ascii 10)),
        proficiency-levels: (list 5 uint),
        availability-hours: (list 10 uint),
        preferred-session-length: uint,
        exchange-reputation: uint,
        total-sessions: uint,
        avg-rating: uint,
        bio: (string-ascii 200),
        active: bool,
    }
)

;; Exchange Sessions Map
(define-map ExchangeSessions
    uint
    {
        participant1: principal,
        participant2: principal,
        language1: (string-ascii 10),
        language2: (string-ascii 10),
        scheduled-time: uint,
        duration: uint,
        status: (string-ascii 20),
        completion-time: uint,
        notes1: (string-ascii 300),
        notes2: (string-ascii 300),
    }
)

;; Session Ratings Map
(define-map SessionRatings
    {
        session-id: uint,
        rater: principal,
    }
    {
        rating: uint,
        feedback: (string-ascii 200),
        communication-score: uint,
        helpfulness-score: uint,
        punctuality-score: uint,
        rating-time: uint,
    }
)

;; Exchange Matches Map - tracks successful matches
(define-map ExchangeMatches
    {
        requester: principal,
        matched-partner: principal,
        language-pair: (string-ascii 21),
    }
    {
        match-score: uint,
        created-time: uint,
        sessions-completed: uint,
        avg-mutual-rating: uint,
    }
)

;; Exchange Achievements Map - specific to language exchange
(define-map ExchangeAchievements
    {
        participant: principal,
        achievement-type: (string-ascii 30),
    }
    {
        earned-date: uint,
        description: (string-ascii 100),
        sessions-required: uint,
    }
)

;; === CORE EXCHANGE FUNCTIONS ===

;; Register or update language exchange profile
(define-public (register-language-profile
        (native-langs (list 3 (string-ascii 10)))
        (learning-langs (list 5 (string-ascii 10)))
        (proficiency-levels (list 5 uint))
        (availability-hours (list 10 uint))
        (preferred-length uint)
        (bio (string-ascii 200))
    )
    (let (
            (current-profile (map-get? LanguageProfiles tx-sender))
            (existing-sessions (match current-profile
                some-profile (get total-sessions some-profile)
                u0
            ))
            (existing-reputation (match current-profile
                some-profile (get exchange-reputation some-profile)
                u0
            ))
            (existing-rating (match current-profile
                some-profile (get avg-rating some-profile)
                u0
            ))
        )
        (asserts! (> (len learning-langs) u0) ERR-INVALID-INPUT)
        (asserts! (> (len native-langs) u0) ERR-INVALID-INPUT)
        (asserts! (is-eq (len learning-langs) (len proficiency-levels))
            ERR-INVALID-INPUT
        )
        (asserts! (and (>= preferred-length u30) (<= preferred-length u180))
            ERR-INVALID-INPUT
        )
        (map-set LanguageProfiles tx-sender {
            native-languages: native-langs,
            learning-languages: learning-langs,
            proficiency-levels: proficiency-levels,
            availability-hours: availability-hours,
            preferred-session-length: preferred-length,
            exchange-reputation: existing-reputation,
            total-sessions: existing-sessions,
            avg-rating: existing-rating,
            bio: bio,
            active: true,
        })
        (ok true)
    )
)

;; Find compatible exchange partner
(define-public (find-exchange-match (target-language (string-ascii 10)))
    (let (
            (requester-profile (unwrap! (map-get? LanguageProfiles tx-sender)
                ERR-PROFILE-NOT-FOUND
            ))
            (requester-natives (get native-languages requester-profile))
            (potential-matches (find-compatible-partners tx-sender target-language
                requester-natives
            ))
        )
        (asserts! (get active requester-profile) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (index-of (get learning-languages requester-profile)
            target-language
        )) ERR-INVALID-INPUT)
        (match potential-matches
            some-match (begin
                (let (
                        (match-key {
                            requester: tx-sender,
                            matched-partner: some-match,
                            language-pair: (concat target-language "-exchange"),
                        })
                        (match-score (calculate-match-score tx-sender some-match))
                    )
                    (map-set ExchangeMatches match-key {
                        match-score: match-score,
                        created-time: stacks-block-height,
                        sessions-completed: u0,
                        avg-mutual-rating: u0,
                    })
                    (ok some-match)
                )
            )
            ERR-NO-MATCH-FOUND
        )
    )
)

;; Schedule a language exchange session
(define-public (schedule-exchange-session
        (partner principal)
        (primary-language (string-ascii 10))
        (secondary-language (string-ascii 10))
        (scheduled-time uint)
        (session-notes (string-ascii 300))
    )
    (let (
            (session-id (+ (var-get session-counter) u1))
            (requester-profile (unwrap! (map-get? LanguageProfiles tx-sender)
                ERR-PROFILE-NOT-FOUND
            ))
            (partner-profile (unwrap! (map-get? LanguageProfiles partner)
                ERR-PROFILE-NOT-FOUND
            ))
            (duration (get preferred-session-length requester-profile))
        )
        (asserts! (not (is-eq tx-sender partner)) ERR-CANNOT-MATCH-SELF)
        (asserts! (get active requester-profile) ERR-NOT-AUTHORIZED)
        (asserts! (get active partner-profile) ERR-NOT-AUTHORIZED)
        (asserts! (> scheduled-time stacks-block-height) ERR-INVALID-INPUT)
        (asserts! (check-time-availability partner scheduled-time duration)
            ERR-TIME-SLOT-TAKEN
        )
        (map-set ExchangeSessions session-id {
            participant1: tx-sender,
            participant2: partner,
            language1: primary-language,
            language2: secondary-language,
            scheduled-time: scheduled-time,
            duration: duration,
            status: "scheduled",
            completion-time: u0,
            notes1: session-notes,
            notes2: "",
        })
        (var-set session-counter session-id)
        (ok session-id)
    )
)

;; Complete an exchange session and update stats
(define-public (complete-exchange-session
        (session-id uint)
        (completion-notes (string-ascii 300))
    )
    (let (
            (session (unwrap! (map-get? ExchangeSessions session-id)
                ERR-SESSION-NOT-FOUND
            ))
            (is-participant1 (is-eq tx-sender (get participant1 session)))
            (is-participant2 (is-eq tx-sender (get participant2 session)))
        )
        (asserts! (or is-participant1 is-participant2) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status session) "scheduled") ERR-INVALID-INPUT)
        (asserts! (>= stacks-block-height (get scheduled-time session))
            ERR-INVALID-INPUT
        )
        (map-set ExchangeSessions session-id
            (merge session {
                status: "completed",
                completion-time: stacks-block-height,
                notes2: (if is-participant2 completion-notes (get notes2 session)),
                notes1: (if is-participant1 completion-notes (get notes1 session)),
            })
        )
        (unwrap-panic (update-session-stats tx-sender))
        (unwrap-panic (check-and-award-exchange-achievements tx-sender))
        (ok true)
    )
)

;; Rate a completed exchange session
(define-public (rate-exchange-session
        (session-id uint)
        (overall-rating uint)
        (communication-score uint)
        (helpfulness-score uint)
        (punctuality-score uint)
        (feedback (string-ascii 200))
    )
    (let (
            (session (unwrap! (map-get? ExchangeSessions session-id)
                ERR-SESSION-NOT-FOUND
            ))
            (rating-key {
                session-id: session-id,
                rater: tx-sender,
            })
            (is-participant (or
                (is-eq tx-sender (get participant1 session))
                (is-eq tx-sender (get participant2 session))
            ))
            (partner (if (is-eq tx-sender (get participant1 session))
                (get participant2 session)
                (get participant1 session)
            ))
        )
        (asserts! is-participant ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status session) "completed")
            ERR-SESSION-NOT-COMPLETED
        )
        (asserts! (and (>= overall-rating u1) (<= overall-rating u5))
            ERR-INVALID-RATING
        )
        (asserts! (and (>= communication-score u1) (<= communication-score u5))
            ERR-INVALID-RATING
        )
        (asserts! (and (>= helpfulness-score u1) (<= helpfulness-score u5))
            ERR-INVALID-RATING
        )
        (asserts! (and (>= punctuality-score u1) (<= punctuality-score u5))
            ERR-INVALID-RATING
        )
        (asserts! (is-none (map-get? SessionRatings rating-key))
            ERR-SESSION-ALREADY-RATED
        )
        (map-set SessionRatings rating-key {
            rating: overall-rating,
            feedback: feedback,
            communication-score: communication-score,
            helpfulness-score: helpfulness-score,
            punctuality-score: punctuality-score,
            rating-time: stacks-block-height,
        })
        (unwrap-panic (update-partner-reputation partner overall-rating))
        (ok true)
    )
)

;; === PRIVATE HELPER FUNCTIONS ===

;; Find compatible language exchange partners
(define-private (find-compatible-partners
        (requester principal)
        (target-language (string-ascii 10))
        (requester-natives (list 3 (string-ascii 10)))
    )
    (let (
            ;; In a real implementation, this would iterate through profiles
            ;; For now, we return none as a placeholder - would need iteration logic
            (dummy-partner (as-contract tx-sender))
        )
        ;; Simplified match logic - in production would check all profiles
        ;; for language compatibility and reputation requirements
        (if (>= (get-user-reputation dummy-partner) (var-get min-reputation-for-matching))
            (some dummy-partner)
            none
        )
    )
)

;; Calculate match compatibility score
(define-private (calculate-match-score (user1 principal) (user2 principal))
    (let (
            (profile1 (unwrap-panic (map-get? LanguageProfiles user1)))
            (profile2 (unwrap-panic (map-get? LanguageProfiles user2)))
        )
        ;; Simplified scoring based on reputation and total sessions
        (+ (get exchange-reputation profile1)
           (get exchange-reputation profile2)
           (get total-sessions profile1)
           (get total-sessions profile2)
        )
    )
)

;; Check if time slot is available for partner
(define-private (check-time-availability
        (partner principal)
        (requested-time uint)
        (duration uint)
    )
    ;; Simplified availability check - would check existing sessions in real impl
    (let ((end-time (+ requested-time duration)))
        (< (- end-time requested-time) u288) ;; Max 2 days in blocks
    )
)

;; Update session statistics for participant
(define-private (update-session-stats (participant principal))
    (let (
            (current-profile (unwrap! (map-get? LanguageProfiles participant)
                ERR-PROFILE-NOT-FOUND
            ))
        )
        (map-set LanguageProfiles participant
            (merge current-profile {
                total-sessions: (+ (get total-sessions current-profile) u1),
                exchange-reputation: (+ (get exchange-reputation current-profile) u1),
            })
        )
        (ok true)
    )
)

;; Update partner reputation based on rating
(define-private (update-partner-reputation (partner principal) (rating uint))
    (let (
            (current-profile (unwrap! (map-get? LanguageProfiles partner)
                ERR-PROFILE-NOT-FOUND
            ))
            (current-avg (get avg-rating current-profile))
            (sessions (get total-sessions current-profile))
            ;; Simple average calculation
            (new-avg (if (is-eq sessions u0)
                rating
                (/ (+ (* current-avg sessions) rating) (+ sessions u1))
            ))
        )
        (map-set LanguageProfiles partner
            (merge current-profile {
                avg-rating: new-avg,
            })
        )
        (ok true)
    )
)

;; Get user reputation (helper function)
(define-private (get-user-reputation (user principal))
    (match (map-get? LanguageProfiles user)
        profile (get exchange-reputation profile)
        u0
    )
)

;; Check and award exchange-specific achievements
(define-private (check-and-award-exchange-achievements (participant principal))
    (let (
            (profile (unwrap! (map-get? LanguageProfiles participant)
                ERR-PROFILE-NOT-FOUND
            ))
            (sessions (get total-sessions profile))
        )
        (if (is-eq sessions u5)
            (unwrap-panic (award-exchange-achievement participant "first-exchanges"
                "Completed first 5 language exchange sessions" u5
            ))
            true
        )
        (if (is-eq sessions u10)
            (unwrap-panic (award-exchange-achievement participant "exchange-regular"
                "Completed 10 language exchange sessions" u10
            ))
            true
        )
        (if (is-eq sessions u25)
            (unwrap-panic (award-exchange-achievement participant "exchange-enthusiast"
                "Completed 25 language exchange sessions" u25
            ))
            true
        )
        (if (is-eq sessions u50)
            (unwrap-panic (award-exchange-achievement participant "exchange-master"
                "Completed 50 language exchange sessions" u50
            ))
            true
        )
        (ok true)
    )
)

;; Award exchange-specific achievement
(define-private (award-exchange-achievement
        (participant principal)
        (achievement-type (string-ascii 30))
        (description (string-ascii 100))
        (sessions-required uint)
    )
    (let (
            (achievement-key {
                participant: participant,
                achievement-type: achievement-type,
            })
        )
        (if (is-none (map-get? ExchangeAchievements achievement-key))
            (map-set ExchangeAchievements achievement-key {
                earned-date: stacks-block-height,
                description: description,
                sessions-required: sessions-required,
            })
            false
        )
        (ok true)
    )
)

;; === READ-ONLY FUNCTIONS FOR EXCHANGE SYSTEM ===

;; Get user's language exchange profile
(define-read-only (get-exchange-profile (user principal))
    (ok (map-get? LanguageProfiles user))
)

;; Get exchange session details
(define-read-only (get-exchange-session (session-id uint))
    (ok (unwrap! (map-get? ExchangeSessions session-id) ERR-SESSION-NOT-FOUND))
)

;; Get session rating details
(define-read-only (get-session-rating
        (session-id uint)
        (rater principal)
    )
    (ok (map-get? SessionRatings {
        session-id: session-id,
        rater: rater,
    }))
)

;; Get match information
(define-read-only (get-exchange-match
        (requester principal)
        (partner principal)
        (language-pair (string-ascii 21))
    )
    (ok (map-get? ExchangeMatches {
        requester: requester,
        matched-partner: partner,
        language-pair: language-pair,
    }))
)

;; Get all exchange achievements for a user
(define-read-only (get-exchange-achievements (participant principal))
    (ok (list
        (map-get? ExchangeAchievements {
            participant: participant,
            achievement-type: "first-exchanges",
        })
        (map-get? ExchangeAchievements {
            participant: participant,
            achievement-type: "exchange-regular",
        })
        (map-get? ExchangeAchievements {
            participant: participant,
            achievement-type: "exchange-enthusiast",
        })
        (map-get? ExchangeAchievements {
            participant: participant,
            achievement-type: "exchange-master",
        })
    ))
)

;; Get exchange system settings
(define-read-only (get-exchange-settings)
    (ok {
        session-counter: (var-get session-counter),
        min-reputation: (var-get min-reputation-for-matching),
        session-duration: (var-get session-duration-blocks),
    })
)

;; Check if user can participate in exchanges
(define-read-only (can-participate-in-exchange (user principal))
    (let ((profile (map-get? LanguageProfiles user)))
        (ok (match profile
            some-profile (and
                (get active some-profile)
                (>= (get exchange-reputation some-profile)
                    (var-get min-reputation-for-matching)
                )
            )
            false
        ))
    )
)

;; Get user's exchange statistics
(define-read-only (get-exchange-stats (user principal))
    (let ((profile (map-get? LanguageProfiles user)))
        (ok (match profile
            some-profile {
                total-sessions: (get total-sessions some-profile),
                avg-rating: (get avg-rating some-profile),
                exchange-reputation: (get exchange-reputation some-profile),
                active: (get active some-profile),
            }
            {
                total-sessions: u0,
                avg-rating: u0,
                exchange-reputation: u0,
                active: false,
            }
        ))
    )
)

;; === ADMIN FUNCTIONS FOR EXCHANGE SYSTEM ===

;; Update exchange system settings
(define-public (update-exchange-settings
        (new-min-reputation uint)
        (new-session-duration uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set min-reputation-for-matching new-min-reputation)
        (var-set session-duration-blocks new-session-duration)
        (ok true)
    )
)

;; Deactivate a user's exchange profile (admin only)
(define-public (deactivate-exchange-profile (user principal))
    (let ((profile (unwrap! (map-get? LanguageProfiles user) ERR-PROFILE-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (map-set LanguageProfiles user
            (merge profile { active: false })
        )
        (ok true)
    )
)
