;; title: Genealogix
;; version: 1.0.0
;; summary: Decentralized Ancestry Tree - NFT-based family lineage mapping
;; description: A smart contract for creating and managing family trees as NFTs with verifiable lineage relationships

;; traits
(define-trait nft-trait
  (
    (get-last-token-id () (response uint uint))
    (get-token-uri (uint) (response (optional (string-ascii 256)) uint))
    (get-owner (uint) (response (optional principal) uint))
    (transfer (uint principal principal) (response bool uint))
  )
)

;; token definitions
(define-non-fungible-token genealogy-node uint)

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-TOKEN-OWNER (err u101))
(define-constant ERR-TOKEN-NOT-FOUND (err u102))
(define-constant ERR-INVALID-PARENT (err u103))
(define-constant ERR-ALREADY-HAS-PARENTS (err u104))
(define-constant ERR-SELF-REFERENCE (err u105))
(define-constant ERR-INVALID-GENERATION (err u106))
(define-constant ERR-MINT-FAILED (err u107))
(define-constant ERR-TRANSFER-FAILED (err u108))
(define-constant MAX-NAME-LENGTH u64)
(define-constant MAX-BIO-LENGTH u256)

;; data vars
(define-data-var last-token-id uint u0)
(define-data-var contract-uri (string-ascii 256) "https://genealogix.io/metadata/")

;; data maps
(define-map token-metadata
  uint
  {
    name: (string-ascii 64),
    bio: (string-ascii 256),
    birth-year: uint,
    death-year: (optional uint),
    generation: uint,
    created-at: uint
  }
)

(define-map family-relationships
  uint
  {
    father: (optional uint),
    mother: (optional uint),
    children: (list 20 uint),
    spouse: (optional uint)
  }
)

(define-map generation-count uint uint)

(define-map user-nodes principal (list 50 uint))

(define-map node-verification
  uint
  {
    verified: bool,
    verified-by: (optional principal),
    verification-date: (optional uint)
  }
)

;; public functions
(define-public (mint-ancestor (name (string-ascii 64)) (bio (string-ascii 256)) (birth-year uint) (death-year (optional uint)))
  (let
    (
      (token-id (+ (var-get last-token-id) u1))
      (generation u1)
    )
    (asserts! (<= (len name) MAX-NAME-LENGTH) (err u109))
    (asserts! (<= (len bio) MAX-BIO-LENGTH) (err u110))
    (try! (nft-mint? genealogy-node token-id tx-sender))
    (map-set token-metadata token-id {
      name: name,
      bio: bio,
      birth-year: birth-year,
      death-year: death-year,
      generation: generation,
      created-at: stacks-block-height
    })
    (map-set family-relationships token-id {
      father: none,
      mother: none,
      children: (list),
      spouse: none
    })
    (map-set node-verification token-id {
      verified: false,
      verified-by: none,
      verification-date: none
    })
    (var-set last-token-id token-id)
    (map-set generation-count generation (+ (default-to u0 (map-get? generation-count generation)) u1))
    (map-set user-nodes tx-sender 
      (unwrap! (as-max-len? (append (default-to (list) (map-get? user-nodes tx-sender)) token-id) u50) ERR-MINT-FAILED))
    (ok token-id)
  )
)

(define-public (mint-descendant (name (string-ascii 64)) (bio (string-ascii 256)) (birth-year uint) (death-year (optional uint)) (father-id (optional uint)) (mother-id (optional uint)))
  (let
    (
      (token-id (+ (var-get last-token-id) u1))
      (parent-generation (get-parent-generation father-id mother-id))
      (generation (+ parent-generation u1))
    )
    (asserts! (<= (len name) MAX-NAME-LENGTH) (err u109))
    (asserts! (<= (len bio) MAX-BIO-LENGTH) (err u110))
    (asserts! (validate-parents father-id mother-id) ERR-INVALID-PARENT)
    (try! (nft-mint? genealogy-node token-id tx-sender))
    (map-set token-metadata token-id {
      name: name,
      bio: bio,
      birth-year: birth-year,
      death-year: death-year,
      generation: generation,
      created-at: stacks-block-height
    })
    (map-set family-relationships token-id {
      father: father-id,
      mother: mother-id,
      children: (list),
      spouse: none
    })
    (map-set node-verification token-id {
      verified: false,
      verified-by: none,
      verification-date: none
    })
    (try! (add-child-to-parents father-id mother-id token-id))
    (var-set last-token-id token-id)
    (map-set generation-count generation (+ (default-to u0 (map-get? generation-count generation)) u1))
    (map-set user-nodes tx-sender 
      (unwrap! (as-max-len? (append (default-to (list) (map-get? user-nodes tx-sender)) token-id) u50) ERR-MINT-FAILED))
    (ok token-id)
  )
)

(define-public (set-spouse (node-id uint) (spouse-id uint))
  (let
    (
      (node-owner (unwrap! (nft-get-owner? genealogy-node node-id) ERR-TOKEN-NOT-FOUND))
      (spouse-owner (unwrap! (nft-get-owner? genealogy-node spouse-id) ERR-TOKEN-NOT-FOUND))
      (current-relationship (unwrap! (map-get? family-relationships node-id) ERR-TOKEN-NOT-FOUND))
      (spouse-relationship (unwrap! (map-get? family-relationships spouse-id) ERR-TOKEN-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender node-owner) ERR-NOT-TOKEN-OWNER)
    (asserts! (not (is-eq node-id spouse-id)) ERR-SELF-REFERENCE)
    (map-set family-relationships node-id (merge current-relationship { spouse: (some spouse-id) }))
    (map-set family-relationships spouse-id (merge spouse-relationship { spouse: (some node-id) }))
    (ok true)
  )
)

(define-public (verify-node (node-id uint))
  (let
    (
      (current-verification (unwrap! (map-get? node-verification node-id) ERR-TOKEN-NOT-FOUND))
    )
    (map-set node-verification node-id {
      verified: true,
      verified-by: (some tx-sender),
      verification-date: (some stacks-block-height)
    })
    (ok true)
  )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-TOKEN-OWNER)
    (try! (nft-transfer? genealogy-node token-id sender recipient))
    (ok true)
  )
)

;; read only functions
(define-read-only (get-last-token-id)
  (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
  (ok (some (concat (var-get contract-uri) (uint-to-ascii token-id))))
)

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? genealogy-node token-id))
)

(define-read-only (get-node-metadata (token-id uint))
  (map-get? token-metadata token-id)
)

(define-read-only (get-family-relationships (token-id uint))
  (map-get? family-relationships token-id)
)

(define-read-only (get-children (token-id uint))
  (match (map-get? family-relationships token-id)
    relationship (get children relationship)
    (list)
  )
)

(define-read-only (get-parents (token-id uint))
  (match (map-get? family-relationships token-id)
    relationship {
      father: (get father relationship),
      mother: (get mother relationship)
    }
    { father: none, mother: none }
  )
)

(define-read-only (get-generation-count (generation uint))
  (default-to u0 (map-get? generation-count generation))
)

(define-read-only (get-user-nodes (user principal))
  (default-to (list) (map-get? user-nodes user))
)

(define-read-only (get-node-verification (token-id uint))
  (map-get? node-verification token-id)
)

(define-read-only (is-verified (token-id uint))
  (match (map-get? node-verification token-id)
    verification (get verified verification)
    false
  )
)

(define-read-only (get-lineage-path (token-id uint))
  (let
    (
      (parents (get-parents token-id))
      (father-id (get father parents))
      (mother-id (get mother parents))
    )
    {
      current: token-id,
      father: father-id,
      mother: mother-id,
      paternal-grandfather: (match father-id some-father (get father (get-parents some-father)) none),
      paternal-grandmother: (match father-id some-father (get mother (get-parents some-father)) none),
      maternal-grandfather: (match mother-id some-mother (get father (get-parents some-mother)) none),
      maternal-grandmother: (match mother-id some-mother (get mother (get-parents some-mother)) none)
    }
  )
)

;; private functions
(define-private (validate-parents (father-id (optional uint)) (mother-id (optional uint)))
  (match father-id
    some-father (is-some (nft-get-owner? genealogy-node some-father))
    (match mother-id
      some-mother (is-some (nft-get-owner? genealogy-node some-mother))
      false
    )
  )
)

(define-private (get-parent-generation (father-id (optional uint)) (mother-id (optional uint)))
  (let
    (
      (father-gen (match father-id
        some-father (match (map-get? token-metadata some-father)
          metadata (get generation metadata)
          u0
        )
        u0
      ))
      (mother-gen (match mother-id
        some-mother (match (map-get? token-metadata some-mother)
          metadata (get generation metadata)
          u0
        )
        u0
      ))
    )
    (if (> father-gen mother-gen) father-gen mother-gen)
  )
)

(define-private (add-child-to-parents (father-id (optional uint)) (mother-id (optional uint)) (child-id uint))
  (begin
    (match father-id
      some-father (let
        (
          (father-relationship (unwrap! (map-get? family-relationships some-father) ERR-TOKEN-NOT-FOUND))
          (current-children (get children father-relationship))
        )
        (map-set family-relationships some-father 
          (merge father-relationship { 
            children: (unwrap! (as-max-len? (append current-children child-id) u20) ERR-MINT-FAILED)
          })
        )
      )
      true
    )
    (match mother-id
      some-mother (let
        (
          (mother-relationship (unwrap! (map-get? family-relationships some-mother) ERR-TOKEN-NOT-FOUND))
          (current-children (get children mother-relationship))
        )
        (map-set family-relationships some-mother 
          (merge mother-relationship { 
            children: (unwrap! (as-max-len? (append current-children child-id) u20) ERR-MINT-FAILED)
          })
        )
      )
      true
    )
    (ok true)
  )
)

(define-private (uint-to-ascii (value uint))
  (if (is-eq value u0) "0"
    (if (< value u10) (unwrap-panic (element-at "0123456789" value))
      (get r (fold uint-to-ascii-inner 
        0x000000000000000000000000000000000000000000000000000000000000000000000000 
        { v: value, r: "" }))))
)

(define-private (uint-to-ascii-inner (i (buff 1)) (d { v: uint, r: (string-ascii 39) }))
  (if (> (get v d) u0)
    { 
      v: (/ (get v d) u10), 
      r: (unwrap-panic (as-max-len? (concat (unwrap-panic (element-at "0123456789" (mod (get v d) u10))) (get r d)) u39))
    }
    d
  )
)