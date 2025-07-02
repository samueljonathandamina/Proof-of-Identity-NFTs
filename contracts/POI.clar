;; POI NFT Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-verified (err u101))
(define-constant err-already-has-poi (err u102))

;; Data vars
(define-data-var last-token-id uint u0)

;; Data maps
(define-map token-uri {token-id: uint} {uri: (string-utf8 256)})
(define-map verified-addresses principal bool)
(define-map owner-token principal uint)

;; SFTs
(define-non-fungible-token poi-nft uint)

;; Verification functions
(define-public (verify-address (address principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verified-addresses address true)
        (ok true)))

;; NFT functions
(define-public (mint)
    (let 
        ((token-id (+ (var-get last-token-id) u1)))
        (asserts! (is-some (map-get? verified-addresses tx-sender)) err-not-verified)
        (asserts! (is-none (map-get? owner-token tx-sender)) err-already-has-poi)
        (try! (nft-mint? poi-nft token-id tx-sender))
        (var-set last-token-id token-id)
        (map-set owner-token tx-sender token-id)
        (ok token-id)))

;; NFT Trait Implementation
(define-read-only (get-last-token-id)
    (ok (var-get last-token-id)))

(define-read-only (get-token-uri (token-id uint))
    (ok (map-get? token-uri {token-id: token-id})))

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? poi-nft token-id)))



;; Add to existing data maps
(define-map token-metadata 
    {token-id: uint} 
    {
        name: (string-utf8 256),
        description: (string-utf8 1024),
        creation-date: uint
    })

;; Add metadata during mint
(define-public (mint-with-metadata (name (string-utf8 256)) (description (string-utf8 1024)))
    (let 
        ((token-id (+ (var-get last-token-id) u1)))
        (asserts! (is-some (map-get? verified-addresses tx-sender)) err-not-verified)
        (asserts! (is-none (map-get? owner-token tx-sender)) err-already-has-poi)
        (try! (nft-mint? poi-nft token-id tx-sender))
        (map-set token-metadata 
            {token-id: token-id}
            {
                name: name,
                description: description,
                creation-date: stacks-block-height
            })
        (var-set last-token-id token-id)
        (map-set owner-token tx-sender token-id)
        (ok token-id)))


;; Add to constants
(define-constant expiration-blocks u52560) ;; Example: 1 year in blocks

;; Add to data maps
(define-map token-expiry {token-id: uint} {expiry: uint})

;; Add expiry check function
(define-read-only (is-token-valid (token-id uint))
    (let ((expiry (unwrap! (map-get? token-expiry {token-id: token-id}) (ok false))))
        (ok (< stacks-block-height (get expiry expiry)))))



;; Add to constants
(define-constant err-token-revoked (err u103))

;; Add to data maps
(define-map revoked-tokens uint bool)

;; Add revocation function
(define-public (revoke-token (token-id uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set revoked-tokens token-id true)
        (ok true)))




;; Add to data maps
(define-map verification-level principal uint)

;; Add verification level function
(define-public (set-verification-level (address principal) (level uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verification-level address level)
        (ok true)))


;; Add to data maps
(define-map update-history 
    {token-id: uint} 
    {updates: (list 10 {block: uint, action: (string-utf8 64)})}
)

;; Add update tracking function
(define-public (record-update (token-id uint) (action (string-utf8 64)))
    (let ((current-history (default-to {updates: (list)} (map-get? update-history {token-id: token-id}))))
        (map-set update-history 
            {token-id: token-id}
            {updates: (unwrap! (as-max-len? (concat (get updates current-history) (list {block: stacks-block-height, action: action})) u10) (err u104))}
        )
        (ok true)))


;; Add to data maps
(define-map recovery-addresses {token-id: uint} {backup: principal})

;; Add recovery address setting
(define-public (set-recovery-address (token-id uint) (backup-address principal))
    (begin
        (asserts! (is-eq (some tx-sender) (nft-get-owner? poi-nft token-id)) err-owner-only)
        (map-set recovery-addresses {token-id: token-id} {backup: backup-address})
        (ok true)))




;; Add to data maps
(define-map endorsements 
    {token-id: uint} 
    {endorsers: (list 5 principal)}
)

;; Add endorsement function
(define-public (endorse-identity (token-id uint))
    (let ((current-endorsements (default-to {endorsers: (list)} (map-get? endorsements {token-id: token-id}))))
        (asserts! (is-some (map-get? verified-addresses tx-sender)) err-not-verified)
        (map-set endorsements 
            {token-id: token-id}
            {endorsers: (unwrap! (as-max-len? (concat (get endorsers current-endorsements) (list tx-sender)) u5) (err u105))}
        )
        (ok true)))


;; Add to constants
(define-constant tier-basic u1)
(define-constant tier-silver u2)
(define-constant tier-gold u3)

;; Add to data maps
(define-map user-tiers principal uint)

;; Add tier management function
(define-public (set-user-tier (address principal) (tier uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set user-tiers address tier)
        (ok true)))

(define-read-only (get-user-tier (address principal))
    (default-to u0 (map-get? user-tiers address)))



;; Add to data maps
(define-map transfer-locks uint {unlock-height: uint, recipient: principal})

;; Add transfer lock function
(define-public (schedule-transfer (token-id uint) (recipient principal) (blocks uint))
    (begin
        (asserts! (is-eq (some tx-sender) (nft-get-owner? poi-nft token-id)) err-owner-only)
        (map-set transfer-locks token-id 
            {
                unlock-height: (+ stacks-block-height blocks),
                recipient: recipient
            })
        (ok true)))


;; Add to data maps
(define-map reputation-scores principal uint)

;; Add reputation management
(define-public (update-reputation (address principal) (score uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set reputation-scores address score)
        (ok true)))

(define-read-only (get-reputation (address principal))
    (default-to u0 (map-get? reputation-scores address)))



;; Add to data maps
(define-map metadata-versions 
    {token-id: uint} 
    {versions: (list 5 {
        version: uint,
        timestamp: uint,
        metadata-hash: (string-utf8 64)
    })})

(define-public (add-metadata-version (token-id uint) (metadata-hash (string-utf8 64)))
    (let ((current-versions (default-to {versions: (list)} (map-get? metadata-versions {token-id: token-id}))))
        (map-set metadata-versions 
            {token-id: token-id}
            {versions: (unwrap! (as-max-len? 
                (concat (get versions current-versions) 
                (list {version: (+ (len (get versions current-versions)) u1), 
                      timestamp: stacks-block-height,
                      metadata-hash: metadata-hash})) u5) (err u106))}
        )
        (ok true)))


;; Add to data maps
(define-map delegations 
    principal 
    {delegate: principal, expiry: uint})

(define-public (delegate-identity (delegate principal) (duration uint))
    (begin
        (asserts! (is-some (map-get? verified-addresses tx-sender)) err-not-verified)
        (map-set delegations tx-sender 
            {
                delegate: delegate,
                expiry: (+ stacks-block-height duration)
            })
        (ok true)))

(define-read-only (check-delegation (owner principal) (delegate principal))
    (match (map-get? delegations owner)
        delegation (ok (and 
            (is-eq (get delegate delegation) delegate)
            (> (get expiry delegation) stacks-block-height)))
        (ok false)))


;; Add to data maps
(define-map staked-tokens principal uint)

(define-public (stake-tokens (amount uint))
    (begin
        (asserts! (is-some (map-get? verified-addresses tx-sender)) err-not-verified)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set staked-tokens tx-sender amount)
        (ok true)))

(define-public (unstake-tokens)
    (let ((staked-amount (default-to u0 (map-get? staked-tokens tx-sender))))
        (asserts! (> staked-amount u0) (err u110))
        (try! (as-contract (stx-transfer? staked-amount tx-sender tx-sender)))
        (map-delete staked-tokens tx-sender)
        (ok true)))



(define-constant err-invalid-lock-time (err u120))
(define-constant err-transfer-locked (err u121))
(define-constant min-lock-period u100)

(define-map time-locked-transfers uint 
    {
        recipient: principal,
        unlock-time: uint,
        status: (string-utf8 10)
    }
)

(define-public (create-timed-transfer (token-id uint) (to principal) (lock-blocks uint))
    (let ((owner (unwrap! (nft-get-owner? poi-nft token-id) err-owner-only)))
        (asserts! (is-eq tx-sender owner) err-owner-only)
        (asserts! (>= lock-blocks min-lock-period) err-invalid-lock-time)
        (map-set time-locked-transfers token-id
            {
                recipient: to,
                unlock-time: (+ stacks-block-height lock-blocks),
                status: u"PENDING"
            })
        (ok true)))
(define-public (execute-timed-transfer (token-id uint))
    (let (
        (transfer-data (unwrap! (map-get? time-locked-transfers token-id) err-transfer-locked))
        (current-time stacks-block-height)
    )
        (asserts! (>= current-time (get unlock-time transfer-data)) err-transfer-locked)
        (try! (nft-transfer? poi-nft token-id tx-sender (get recipient transfer-data)))
        (map-delete time-locked-transfers token-id)
        (ok true)))



(define-constant err-invalid-hash (err u130))
(define-constant err-unauthorized-verifier (err u131))

(define-map metadata-verifiers principal bool)
(define-map verified-metadata uint 
    {
        hash: (buff 32),
        verifier: principal,
        timestamp: uint
    }
)

(define-public (register-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set metadata-verifiers verifier true)
        (ok true)))

(define-public (verify-metadata (token-id uint) (metadata-hash (buff 32)))
    (begin
        (asserts! (is-some (map-get? metadata-verifiers tx-sender)) err-unauthorized-verifier)
        (map-set verified-metadata token-id
            {
                hash: metadata-hash,
                verifier: tx-sender,
                timestamp: stacks-block-height
            })
        (ok true)))

(define-read-only (get-verified-metadata (token-id uint))
    (ok (map-get? verified-metadata token-id)))


(define-constant err-badge-exists (err u140))
(define-constant err-invalid-badge (err u141))

(define-map badges 
    {badge-id: uint} 
    {
        name: (string-utf8 64),
        criteria: (string-utf8 256)
    }
)

(define-map user-badges 
    {user: principal} 
    {earned: (list 10 uint)}
)

(define-public (create-badge (badge-id uint) (name (string-utf8 64)) (criteria (string-utf8 256)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? badges {badge-id: badge-id})) err-badge-exists)
        (map-set badges {badge-id: badge-id}
            {
                name: name,
                criteria: criteria
            })
        (ok true)))

(define-public (award-badge (user principal) (badge-id uint))
    (let ((current-badges (default-to {earned: (list)} (map-get? user-badges {user: user}))))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-some (map-get? badges {badge-id: badge-id})) err-invalid-badge)
        (map-set user-badges 
            {user: user}
            {earned: (unwrap! (as-max-len? (concat (get earned current-badges) (list badge-id)) u10) (err u142))}
        )
        (ok true)))

(define-read-only (get-user-badges (user principal))
    (ok (map-get? user-badges {user: user})))



(define-constant err-proposal-exists (err u150))
(define-constant err-invalid-proposal (err u151))
(define-constant err-already-voted (err u152))

(define-map proposals 
    {proposal-id: uint} 
    {
        title: (string-utf8 256),
        end-block: uint,
        yes-votes: uint,
        no-votes: uint
    }
)

(define-map votes 
    {proposal-id: uint, voter: principal} 
    {vote: bool}
)

(define-public (create-proposal (proposal-id uint) (title (string-utf8 256)) (duration uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? proposals {proposal-id: proposal-id})) err-proposal-exists)
        (map-set proposals {proposal-id: proposal-id}
            {
                title: title,
                end-block: (+ stacks-block-height duration),
                yes-votes: u0,
                no-votes: u0
            })
        (ok true)))

(define-public (cast-vote (proposal-id uint) (vote bool))
    (let (
        (proposal (unwrap! (map-get? proposals {proposal-id: proposal-id}) err-invalid-proposal))
        (voting-power (+ (get-reputation tx-sender) (get-user-tier tx-sender)))
    )
        (asserts! (< stacks-block-height (get end-block proposal)) err-invalid-proposal)
        (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: tx-sender})) err-already-voted)
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} {vote: vote})
        (map-set proposals {proposal-id: proposal-id}
            (merge proposal 
                {yes-votes: (if vote (+ (get yes-votes proposal) voting-power) (get yes-votes proposal)),
                 no-votes: (if (not vote) (+ (get no-votes proposal) voting-power) (get no-votes proposal))}))
        (ok true)))

(define-constant err-invalid-proof (err u200))
(define-constant err-proof-expired (err u201))
(define-constant err-unauthorized-bridge (err u202))

(define-data-var bridge-nonce uint u0)

(define-map authorized-bridges principal bool)
(define-map identity-proofs 
    {proof-id: (buff 32)} 
    {
        token-id: uint,
        target-chain: (string-utf8 32),
        expiry: uint,
        nonce: uint,
        used: bool
    }
)

(define-map bridge-requests
    {request-id: uint}
    {
        requester: principal,
        token-id: uint,
        target-chain: (string-utf8 32),
        timestamp: uint,
        status: (string-utf8 16)
    }
)

(define-public (authorize-bridge (bridge-address principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-bridges bridge-address true)
        (ok true)))

(define-public (generate-identity-proof (token-id uint) (target-chain (string-utf8 32)) (duration uint))
    (let (
        (current-nonce (var-get bridge-nonce))
        (proof-data (concat 
            (concat
                (unwrap-panic (to-consensus-buff? token-id))
                (unwrap-panic (to-consensus-buff? tx-sender))
            )
            (unwrap-panic (to-consensus-buff? current-nonce))
        ))
        (proof-id (sha256 proof-data))
    )
        (asserts! (is-eq (some tx-sender) (nft-get-owner? poi-nft token-id)) err-owner-only)
        (asserts! (is-none (map-get? revoked-tokens token-id)) err-token-revoked)
        (map-set identity-proofs 
            {proof-id: proof-id}
            {
                token-id: token-id,
                target-chain: target-chain,
                expiry: (+ stacks-block-height duration),
                nonce: current-nonce,
                used: false
            }
        )
        (var-set bridge-nonce (+ current-nonce u1))
        (ok proof-id)))

(define-public (verify-identity-proof (proof-id (buff 32)) (token-id uint) (original-owner principal))
    (let (
        (proof (unwrap! (map-get? identity-proofs {proof-id: proof-id}) err-invalid-proof))
    )
        (asserts! (is-some (map-get? authorized-bridges tx-sender)) err-unauthorized-bridge)
        (asserts! (is-eq (get token-id proof) token-id) err-invalid-proof)
        (asserts! (< stacks-block-height (get expiry proof)) err-proof-expired)
        (asserts! (not (get used proof)) err-invalid-proof)
        (map-set identity-proofs 
            {proof-id: proof-id}
            (merge proof {used: true})
        )
        (ok {
            token-id: (get token-id proof),
            target-chain: (get target-chain proof),
            nonce: (get nonce proof)
        })))

(define-public (create-bridge-request (token-id uint) (target-chain (string-utf8 32)))
    (let (
        (request-id (+ (var-get bridge-nonce) u1))
    )
        (asserts! (is-eq (some tx-sender) (nft-get-owner? poi-nft token-id)) err-owner-only)
        (map-set bridge-requests
            {request-id: request-id}
            {
                requester: tx-sender,
                token-id: token-id,
                target-chain: target-chain,
                timestamp: stacks-block-height,
                status: u"PENDING"
            }
        )
        (var-set bridge-nonce request-id)
        (ok request-id)))

(define-public (process-bridge-request (request-id uint) (status (string-utf8 16)))
    (let (
        (request (unwrap! (map-get? bridge-requests {request-id: request-id}) err-invalid-proposal))
    )
        (asserts! (is-some (map-get? authorized-bridges tx-sender)) err-unauthorized-bridge)
        (map-set bridge-requests
            {request-id: request-id}
            (merge request {status: status})
        )
        (ok true)))

(define-read-only (get-identity-proof (proof-id (buff 32)))
    (ok (map-get? identity-proofs {proof-id: proof-id})))

(define-read-only (get-bridge-request (request-id uint))
    (ok (map-get? bridge-requests {request-id: request-id})))

(define-read-only (validate-cross-chain-identity (proof-id (buff 32)) (claimed-token-id uint))
    (match (map-get? identity-proofs {proof-id: proof-id})
        proof (ok (and 
            (is-eq (get token-id proof) claimed-token-id)
            (< stacks-block-height (get expiry proof))
            (not (get used proof))))
        (ok false)))


(define-constant err-invalid-attestation (err u300))
(define-constant err-insufficient-payment (err u301))
(define-constant err-service-not-active (err u302))
(define-constant err-self-attestation (err u303))
(define-constant err-request-not-found (err u304))
(define-constant err-attestation-expired (err u305))
(define-constant err-unauthorized-action (err u306))

(define-data-var attestation-service-counter uint u0)
(define-data-var attestation-request-counter uint u0)

(define-map attestation-services
    {service-id: uint}
    {
        attester: principal,
        price: uint,
        description: (string-utf8 256),
        reputation-score: uint,
        active: bool,
        total-attestations: uint
    }
)

(define-map attestation-requests
    {request-id: uint}
    {
        requester: principal,
        service-id: uint,
        payment-amount: uint,
        status: (string-utf8 16),
        created-at: uint,
        expires-at: uint,
        evidence-hash: (buff 32)
    }
)

(define-map completed-attestations
    {attestation-id: uint}
    {
        attester: principal,
        subject: principal,
        service-id: uint,
        quality-rating: uint,
        timestamp: uint,
        verified: bool
    }
)

(define-map escrow-funds
    {request-id: uint}
    {
        amount: uint,
        locked: bool
    }
)

(define-map attester-earnings principal uint)

(define-public (create-attestation-service (price uint) (description (string-utf8 256)))
    (let (
        (service-id (+ (var-get attestation-service-counter) u1))
    )
        (asserts! (is-some (map-get? verified-addresses tx-sender)) err-not-verified)
        (asserts! (> price u0) err-insufficient-payment)
        (map-set attestation-services
            {service-id: service-id}
            {
                attester: tx-sender,
                price: price,
                description: description,
                reputation-score: u100,
                active: true,
                total-attestations: u0
            }
        )
        (var-set attestation-service-counter service-id)
        (ok service-id)))

(define-public (update-service-status (service-id uint) (active bool))
    (let (
        (service (unwrap! (map-get? attestation-services {service-id: service-id}) err-service-not-active))
    )
        (asserts! (is-eq tx-sender (get attester service)) err-unauthorized-action)
        (map-set attestation-services
            {service-id: service-id}
            (merge service {active: active})
        )
        (ok true)))

(define-public (request-attestation (service-id uint) (evidence-hash (buff 32)) (duration uint))
    (let (
        (service (unwrap! (map-get? attestation-services {service-id: service-id}) err-service-not-active))
        (request-id (+ (var-get attestation-request-counter) u1))
        (payment-amount (get price service))
    )
        (asserts! (get active service) err-service-not-active)
        (asserts! (not (is-eq tx-sender (get attester service))) err-self-attestation)
        (try! (stx-transfer? payment-amount tx-sender (as-contract tx-sender)))
        (map-set attestation-requests
            {request-id: request-id}
            {
                requester: tx-sender,
                service-id: service-id,
                payment-amount: payment-amount,
                status: u"PENDING",
                created-at: stacks-block-height,
                expires-at: (+ stacks-block-height duration),
                evidence-hash: evidence-hash
            }
        )
        (map-set escrow-funds
            {request-id: request-id}
            {
                amount: payment-amount,
                locked: true
            }
        )
        (var-set attestation-request-counter request-id)
        (ok request-id)))

(define-public (fulfill-attestation (request-id uint) (verified bool))
    (let (
        (request (unwrap! (map-get? attestation-requests {request-id: request-id}) err-request-not-found))
        (service (unwrap! (map-get? attestation-services {service-id: (get service-id request)}) err-service-not-active))
        (escrow (unwrap! (map-get? escrow-funds {request-id: request-id}) err-insufficient-payment))
        (attestation-id (+ (var-get attestation-request-counter) u1))
    )
        (asserts! (is-eq tx-sender (get attester service)) err-unauthorized-action)
        (asserts! (is-eq (get status request) u"PENDING") err-invalid-attestation)
        (asserts! (< stacks-block-height (get expires-at request)) err-attestation-expired)
        (asserts! (get locked escrow) err-insufficient-payment)
        (map-set attestation-requests
            {request-id: request-id}
            (merge request {status: (if verified u"APPROVED" u"REJECTED")})
        )
        (map-set completed-attestations
            {attestation-id: attestation-id}
            {
                attester: tx-sender,
                subject: (get requester request),
                service-id: (get service-id request),
                quality-rating: u0,
                timestamp: stacks-block-height,
                verified: verified
            }
        )
        (if verified
            (map-set verified-addresses (get requester request) true)
            true
        )
        (map-set escrow-funds
            {request-id: request-id}
            (merge escrow {locked: false})
        )
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get attester service))))
        (map-set attester-earnings tx-sender 
            (+ (default-to u0 (map-get? attester-earnings tx-sender)) (get amount escrow)))
        (map-set attestation-services
            {service-id: (get service-id request)}
            (merge service {total-attestations: (+ (get total-attestations service) u1)})
        )
        (ok attestation-id)))

(define-public (rate-attestation (attestation-id uint) (rating uint))
    (let (
        (attestation (unwrap! (map-get? completed-attestations {attestation-id: attestation-id}) err-invalid-attestation))
    )
        (asserts! (is-eq tx-sender (get subject attestation)) err-unauthorized-action)
        (asserts! (<= rating u5) err-invalid-attestation)
        (map-set completed-attestations
            {attestation-id: attestation-id}
            (merge attestation {quality-rating: rating})
        )
        (ok true)))

(define-public (claim-expired-escrow (request-id uint))
    (let (
        (request (unwrap! (map-get? attestation-requests {request-id: request-id}) err-request-not-found))
        (escrow (unwrap! (map-get? escrow-funds {request-id: request-id}) err-insufficient-payment))
    )
        (asserts! (is-eq tx-sender (get requester request)) err-unauthorized-action)
        (asserts! (>= stacks-block-height (get expires-at request)) err-attestation-expired)
        (asserts! (is-eq (get status request) u"PENDING") err-invalid-attestation)
        (asserts! (get locked escrow) err-insufficient-payment)
        (map-set escrow-funds
            {request-id: request-id}
            (merge escrow {locked: false})
        )
        (map-set attestation-requests
            {request-id: request-id}
            (merge request {status: u"EXPIRED"})
        )
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender tx-sender)))
        (ok true)))

(define-read-only (get-attestation-service (service-id uint))
    (ok (map-get? attestation-services {service-id: service-id})))

(define-read-only (get-attestation-request (request-id uint))
    (ok (map-get? attestation-requests {request-id: request-id})))

(define-read-only (get-completed-attestation (attestation-id uint))
    (ok (map-get? completed-attestations {attestation-id: attestation-id})))

(define-read-only (get-attester-earnings (attester principal))
    (ok (default-to u0 (map-get? attester-earnings attester))))

(define-read-only (get-active-services)
    (ok (var-get attestation-service-counter)))

(define-read-only (check-attestation-status (subject principal))
    (ok (is-some (map-get? verified-addresses subject))))