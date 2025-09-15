;; Dynamic Identity Score System Contract
;; Calculates and manages dynamic reputation scores based on user behavior

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u400))
(define-constant err-not-verified (err u401))
(define-constant err-invalid-score (err u402))
(define-constant err-invalid-weight (err u403))
(define-constant err-insufficient-activity (err u404))
(define-constant err-score-locked (err u405))

(define-constant err-invalid-period (err u406))
(define-constant err-unauthorized-reporter (err u407))

;; Score calculation constants
(define-constant base-score u500)
(define-constant max-score u1000)
(define-constant min-score u100)
(define-constant decay-rate u5) ;; Score decay per 1000 blocks
(define-constant activity-bonus u25)
(define-constant community-feedback-weight u10)
(define-constant verification-bonus u100)

;; Data variables
(define-data-var total-participants uint u0)
(define-data-var score-calculation-nonce uint u0)

;; Core identity scores with time-weighted components
(define-map identity-scores 
    principal 
    {
        current-score: uint,
        base-score: uint,
        last-update: uint,
        activity-count: uint,
        positive-feedback: uint,
        negative-feedback: uint,
        verification-level: uint,
        score-locked: bool,
        lock-expiry: uint
    }
)

;; Activity tracking for score calculation
(define-map user-activities 
    {user: principal} 
    {
        activities: (list 20 {
            activity-type: (string-utf8 32),
            timestamp: uint,
            score-impact: int,
            verified: bool
        })
    }
)

;; Community feedback system
(define-map feedback-records
    {reporter: principal, subject: principal}
    {
        feedback-type: (string-utf8 16),
        weight: uint,
        timestamp: uint,
        reason: (string-utf8 128)
    }
)

;; Score calculation metrics
(define-map score-metrics
    {period: uint}
    {
        average-score: uint,
        active-users: uint,
        total-activities: uint,
        calculation-timestamp: uint
    }
)

;; Reputation milestones and achievements
(define-map reputation-milestones
    {milestone-id: uint}
    {
        name: (string-utf8 64),
        required-score: uint,
        duration-blocks: uint,
        reward-multiplier: uint,
        active: bool
    }
)

(define-map user-milestones
    {user: principal}
    {
        achieved: (list 10 uint),
        current-streak: uint,
        best-score: uint,
        total-rewards: uint
    }
)

;; Score decay tracking
(define-map score-decay-history
    {user: principal}
    {
        decay-events: (list 15 {
            block-height: uint,
            old-score: uint,
            new-score: uint,
            decay-amount: uint
        })
    }
)

;; Authorized activity reporters
(define-map authorized-reporters principal bool)

;; Initialize user score with base values
(define-public (initialize-identity-score (user principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? identity-scores user)) err-invalid-score)
        (map-set identity-scores user
            {
                current-score: base-score,
                base-score: base-score,
                last-update: stacks-block-height,
                activity-count: u0,
                positive-feedback: u0,
                negative-feedback: u0,
                verification-level: u1,
                score-locked: false,
                lock-expiry: u0
            }
        )
        (var-set total-participants (+ (var-get total-participants) u1))
        (ok true)))

;; Record user activity that impacts score
(define-public (record-activity (user principal) (activity-type (string-utf8 32)) (score-impact int))
    (let (
        (current-activities (default-to {activities: (list)} (map-get? user-activities {user: user})))
        (current-score-data (unwrap! (map-get? identity-scores user) err-not-verified))
    )
        (asserts! (is-some (map-get? authorized-reporters tx-sender)) err-unauthorized-reporter)
        (asserts! (not (get score-locked current-score-data)) err-score-locked)
        (map-set user-activities 
            {user: user}
            {activities: (unwrap! (as-max-len? 
                (concat (get activities current-activities) 
                (list {
                    activity-type: activity-type,
                    timestamp: stacks-block-height,
                    score-impact: score-impact,
                    verified: true
                })) u20) err-insufficient-activity)}
        )
        (map-set identity-scores user
            (merge current-score-data 
                {
                    activity-count: (+ (get activity-count current-score-data) u1),
                    last-update: stacks-block-height
                }
            )
        )
        (unwrap-panic (calculate-dynamic-score user))
        (ok true)))

;; Calculate dynamic score based on all factors
(define-public (calculate-dynamic-score (user principal))
    (let (
        (score-data (unwrap! (map-get? identity-scores user) err-not-verified))
        (blocks-passed (- stacks-block-height (get last-update score-data)))
        (decay-amount (/ (* blocks-passed decay-rate) u1000))
        (current-base (get current-score score-data))
        (activity-bonus-amount (* (get activity-count score-data) activity-bonus))
        (feedback-ratio (if (> (+ (get positive-feedback score-data) (get negative-feedback score-data)) u0)
            (/ (* (get positive-feedback score-data) u100) 
               (+ (get positive-feedback score-data) (get negative-feedback score-data)))
            u50))
        (feedback-adjustment (/ (* feedback-ratio community-feedback-weight) u100))
        (verification-adjustment (* (get verification-level score-data) verification-bonus))
        (base-after-decay (if (>= current-base decay-amount) (- current-base decay-amount) min-score))
        (new-score (+ base-after-decay activity-bonus-amount feedback-adjustment verification-adjustment))
        (final-score (if (> new-score max-score) max-score (if (< new-score min-score) min-score new-score)))
    )
        (if (> decay-amount u0)
            (try! (record-decay-event user current-base final-score decay-amount))
            true
        )
        (map-set identity-scores user
            (merge score-data 
                {
                    current-score: final-score,
                    last-update: stacks-block-height
                }
            )
        )
        (unwrap-panic (check-milestone-achievements user final-score))
        (ok final-score)))

;; Record score decay event for transparency
(define-private (record-decay-event (user principal) (old-score uint) (new-score uint) (decay-amount uint))
    (let (
        (current-history (default-to {decay-events: (list)} (map-get? score-decay-history {user: user})))
    )
        (map-set score-decay-history
            {user: user}
            {decay-events: (unwrap! (as-max-len?
                (concat (get decay-events current-history)
                (list {
                    block-height: stacks-block-height,
                    old-score: old-score,
                    new-score: new-score,
                    decay-amount: decay-amount
                })) u15) err-insufficient-activity)}
        )
        (ok true)))

;; Submit community feedback about a user
(define-public (submit-feedback (subject principal) (feedback-type (string-utf8 16)) (reason (string-utf8 128)))
    (let (
        (subject-score (unwrap! (map-get? identity-scores subject) err-not-verified))
        (reporter-score (unwrap! (map-get? identity-scores tx-sender) err-not-verified))
        (feedback-weight (/ (get current-score reporter-score) u100))
    )
        (asserts! (not (is-eq tx-sender subject)) err-unauthorized-reporter)
        (asserts! (>= (get current-score reporter-score) u300) err-insufficient-activity)
        (map-set feedback-records
            {reporter: tx-sender, subject: subject}
            {
                feedback-type: feedback-type,
                weight: feedback-weight,
                timestamp: stacks-block-height,
                reason: reason
            }
        )
        (if (is-eq feedback-type u"POSITIVE")
            (map-set identity-scores subject
                (merge subject-score 
                    {positive-feedback: (+ (get positive-feedback subject-score) feedback-weight)}
                )
            )
            (map-set identity-scores subject
                (merge subject-score 
                    {negative-feedback: (+ (get negative-feedback subject-score) feedback-weight)}
                )
            )
        )
        (unwrap-panic (calculate-dynamic-score subject))
        (ok true)))

;; Create reputation milestone
(define-public (create-milestone (milestone-id uint) (name (string-utf8 64)) (required-score uint) (duration uint) (multiplier uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= required-score max-score) err-invalid-score)
        (asserts! (> multiplier u0) err-invalid-weight)
        (map-set reputation-milestones
            {milestone-id: milestone-id}
            {
                name: name,
                required-score: required-score,
                duration-blocks: duration,
                reward-multiplier: multiplier,
                active: true
            }
        )
        (ok true)))

;; Check and award milestone achievements
(define-private (check-milestone-achievements (user principal) (current-score uint))
    (let (
        (user-milestone-data (default-to 
            {achieved: (list), current-streak: u0, best-score: u0, total-rewards: u0} 
            (map-get? user-milestones {user: user})
        ))
        (new-best-score (if (> current-score (get best-score user-milestone-data)) 
            current-score 
            (get best-score user-milestone-data)
        ))
    )
        (map-set user-milestones
            {user: user}
            (merge user-milestone-data 
                {
                    best-score: new-best-score,
                    current-streak: (if (>= current-score u700) 
                        (+ (get current-streak user-milestone-data) u1) 
                        u0
                    )
                }
            )
        )
        (ok true)))

;; Lock/unlock user score for administrative purposes
(define-public (toggle-score-lock (user principal) (lock-duration uint))
    (let (
        (score-data (unwrap! (map-get? identity-scores user) err-not-verified))
    )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set identity-scores user
            (merge score-data 
                {
                    score-locked: (not (get score-locked score-data)),
                    lock-expiry: (+ stacks-block-height lock-duration)
                }
            )
        )
        (ok true)))

;; Authorize activity reporter
(define-public (authorize-reporter (reporter principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-reporters reporter true)
        (ok true)))

;; Calculate period metrics for analysis
(define-public (calculate-period-metrics (period uint))
    (let (
        (calculation-id (+ (var-get score-calculation-nonce) u1))
        (total-users (var-get total-participants))
    )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set score-metrics
            {period: period}
            {
                average-score: u500, ;; Simplified for this implementation
                active-users: total-users,
                total-activities: calculation-id,
                calculation-timestamp: stacks-block-height
            }
        )
        (var-set score-calculation-nonce calculation-id)
        (ok true)))

;; Read-only functions
(define-read-only (get-identity-score (user principal))
    (ok (map-get? identity-scores user)))

(define-read-only (get-user-activities (user principal))
    (ok (map-get? user-activities {user: user})))

(define-read-only (get-feedback-record (reporter principal) (subject principal))
    (ok (map-get? feedback-records {reporter: reporter, subject: subject})))

(define-read-only (get-milestone (milestone-id uint))
    (ok (map-get? reputation-milestones {milestone-id: milestone-id})))

(define-read-only (get-user-milestones (user principal))
    (ok (map-get? user-milestones {user: user})))

(define-read-only (get-decay-history (user principal))
    (ok (map-get? score-decay-history {user: user})))

(define-read-only (get-period-metrics (period uint))
    (ok (map-get? score-metrics {period: period})))

(define-read-only (is-reporter-authorized (reporter principal))
    (ok (default-to false (map-get? authorized-reporters reporter))))

(define-read-only (get-total-participants)
    (ok (var-get total-participants)))

(define-read-only (calculate-score-percentile (user principal))
    (let (
        (user-score-data (unwrap! (map-get? identity-scores user) err-not-verified))
        (user-score (get current-score user-score-data))
        (total-users (var-get total-participants))
    )
        (ok (/ (* user-score u100) max-score))))

