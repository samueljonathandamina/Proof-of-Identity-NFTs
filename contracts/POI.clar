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
