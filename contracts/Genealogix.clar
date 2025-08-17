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
(define-constant ERR-DNA-PROFILE-EXISTS (err u111))
(define-constant ERR-DNA-PROFILE-NOT-FOUND (err u112))
(define-constant ERR-INVALID-DNA-PERCENTAGE (err u113))
(define-constant ERR-INVALID-SEGMENTS (err u114))
(define-constant ERR-MATCH-ALREADY-EXISTS (err u115))
(define-constant ERR-MATCH-NOT-FOUND (err u116))
(define-constant ERR-INSUFFICIENT-MATCH-PERCENTAGE (err u117))
(define-constant ERR-INVALID-RELATIONSHIP-TYPE (err u118))
(define-constant ERR-ASSET-NOT-FOUND (err u119))
(define-constant ERR-NOT-ASSET-OWNER (err u120))
(define-constant ERR-INVALID-ASSET-TYPE (err u121))
(define-constant ERR-ASSET-ALREADY-EXISTS (err u122))
(define-constant ERR-NOT-FAMILY-MEMBER (err u123))
(define-constant ERR-ASSET-LOCKED (err u124))
(define-constant ERR-INVALID-TRANSFER (err u125))
(define-constant ERR-INSUFFICIENT-PERMISSIONS (err u126))
(define-constant MAX-NAME-LENGTH u64)
(define-constant MAX-BIO-LENGTH u256)
(define-constant MIN-SIBLING-MATCH u2500)
(define-constant MIN-PARENT-CHILD-MATCH u4500)
(define-constant MIN-GRANDPARENT-MATCH u1250)
(define-constant MIN-COUSIN-MATCH u200)

;; data vars
(define-data-var last-token-id uint u0)
(define-data-var last-asset-id uint u0)
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

(define-map dna-profiles
  uint
  {
    profile-hash: (string-ascii 128),
    test-provider: (string-ascii 32),
    upload-date: uint,
    total-segments: uint,
    total-cm: uint,
    profile-owner: principal
  }
)

(define-map dna-matches
  { node1: uint, node2: uint }
  {
    shared-cm: uint,
    shared-segments: uint,
    match-percentage: uint,
    relationship-type: (string-ascii 32),
    confirmed: bool,
    match-date: uint,
    submitted-by: principal
  }
)

(define-map node-dna-validation
  uint
  {
    has-dna: bool,
    match-count: uint,
    validated-relationships: (list 10 uint),
    confidence-score: uint
  }
)

(define-map relationship-requirements
  (string-ascii 32)
  {
    min-cm: uint,
    min-percentage: uint,
    max-cm: uint,
    max-percentage: uint
  }
)

(define-map heritage-assets
  uint
  {
    name: (string-ascii 128),
    description: (string-ascii 512),
    asset-type: (string-ascii 32),
    owner-node-id: uint,
    custodian: principal,
    creation-date: uint,
    acquisition-date: uint,
    estimated-value: uint,
    currency: (string-ascii 8),
    insurance-value: uint,
    is-locked: bool,
    access-level: (string-ascii 16)
  }
)

(define-map asset-provenance
  uint
  {
    original-owner: uint,
    previous-owners: (list 20 uint),
    transfer-history: (list 20 { from: uint, to: uint, date: uint }),
    origin-story: (string-ascii 512),
    historical-significance: (string-ascii 256)
  }
)

(define-map asset-access-permissions
  { asset-id: uint, node-id: uint }
  {
    can-view: bool,
    can-edit: bool,
    can-transfer: bool,
    granted-by: uint,
    granted-date: uint
  }
)

(define-map node-heritage-assets
  uint
  (list 50 uint)
)

(define-map family-asset-vault
  uint
  {
    total-assets: uint,
    total-value: uint,
    vault-keeper: uint,
    last-updated: uint
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
    (map-set node-dna-validation token-id {
      has-dna: false,
      match-count: u0,
      validated-relationships: (list),
      confidence-score: u0
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
    (map-set node-dna-validation token-id {
      has-dna: false,
      match-count: u0,
      validated-relationships: (list),
      confidence-score: u0
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

(define-public (submit-dna-profile (node-id uint) (profile-hash (string-ascii 128)) (test-provider (string-ascii 32)) (total-segments uint) (total-cm uint))
  (let
    (
      (node-owner (unwrap! (nft-get-owner? genealogy-node node-id) ERR-TOKEN-NOT-FOUND))
      (current-validation (unwrap! (map-get? node-dna-validation node-id) ERR-TOKEN-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender node-owner) ERR-NOT-TOKEN-OWNER)
    (asserts! (not (is-some (map-get? dna-profiles node-id))) ERR-DNA-PROFILE-EXISTS)
    (asserts! (> total-cm u0) ERR-INVALID-DNA-PERCENTAGE)
    (asserts! (> total-segments u0) ERR-INVALID-SEGMENTS)
    (map-set dna-profiles node-id {
      profile-hash: profile-hash,
      test-provider: test-provider,
      upload-date: stacks-block-height,
      total-segments: total-segments,
      total-cm: total-cm,
      profile-owner: tx-sender
    })
    (map-set node-dna-validation node-id (merge current-validation { has-dna: true }))
    (ok true)
  )
)

(define-public (submit-dna-match (node1-id uint) (node2-id uint) (shared-cm uint) (shared-segments uint) (match-percentage uint) (relationship-type (string-ascii 32)))
  (let
    (
      (node1-owner (unwrap! (nft-get-owner? genealogy-node node1-id) ERR-TOKEN-NOT-FOUND))
      (node2-owner (unwrap! (nft-get-owner? genealogy-node node2-id) ERR-TOKEN-NOT-FOUND))
      (match-key { node1: node1-id, node2: node2-id })
      (reverse-match-key { node1: node2-id, node2: node1-id })
      (min-requirement (get-relationship-requirement relationship-type))
    )
    (asserts! (not (is-eq node1-id node2-id)) ERR-SELF-REFERENCE)
    (asserts! (or (is-eq tx-sender node1-owner) (is-eq tx-sender node2-owner)) ERR-NOT-TOKEN-OWNER)
    (asserts! (is-some (map-get? dna-profiles node1-id)) ERR-DNA-PROFILE-NOT-FOUND)
    (asserts! (is-some (map-get? dna-profiles node2-id)) ERR-DNA-PROFILE-NOT-FOUND)
    (asserts! (and (not (is-some (map-get? dna-matches match-key))) (not (is-some (map-get? dna-matches reverse-match-key)))) ERR-MATCH-ALREADY-EXISTS)
    (asserts! (>= match-percentage (get min-percentage min-requirement)) ERR-INSUFFICIENT-MATCH-PERCENTAGE)
    (asserts! (>= shared-cm (get min-cm min-requirement)) ERR-INSUFFICIENT-MATCH-PERCENTAGE)
    (map-set dna-matches match-key {
      shared-cm: shared-cm,
      shared-segments: shared-segments,
      match-percentage: match-percentage,
      relationship-type: relationship-type,
      confirmed: false,
      match-date: stacks-block-height,
      submitted-by: tx-sender
    })
    (try! (update-dna-validation-counts node1-id node2-id))
    (ok true)
  )
)

(define-public (confirm-dna-match (node1-id uint) (node2-id uint))
  (let
    (
      (match-key { node1: node1-id, node2: node2-id })
      (reverse-match-key { node1: node2-id, node2: node1-id })
      (existing-match (map-get? dna-matches match-key))
      (reverse-match (map-get? dna-matches reverse-match-key))
      (node1-owner (unwrap! (nft-get-owner? genealogy-node node1-id) ERR-TOKEN-NOT-FOUND))
      (node2-owner (unwrap! (nft-get-owner? genealogy-node node2-id) ERR-TOKEN-NOT-FOUND))
    )
    (asserts! (or (is-eq tx-sender node1-owner) (is-eq tx-sender node2-owner)) ERR-NOT-TOKEN-OWNER)
    (match existing-match
      some-match (begin
        (map-set dna-matches match-key (merge some-match { confirmed: true }))
        (try! (validate-genetic-relationship node1-id node2-id some-match))
        (ok true)
      )
      (match reverse-match
        some-reverse (begin
          (map-set dna-matches reverse-match-key (merge some-reverse { confirmed: true }))
          (try! (validate-genetic-relationship node2-id node1-id some-reverse))
          (ok true)
        )
        ERR-MATCH-NOT-FOUND
      )
    )
  )
)

(define-public (calculate-confidence-score (node-id uint))
  (let
    (
      (current-validation (unwrap! (map-get? node-dna-validation node-id) ERR-TOKEN-NOT-FOUND))
      (match-count (get match-count current-validation))
      (validated-count (len (get validated-relationships current-validation)))
      (base-score (if (get has-dna current-validation) u25 u0))
      (match-score (* match-count u10))
      (validation-score (* validated-count u15))
      (total-score (+ base-score (+ match-score validation-score)))
    )
    (map-set node-dna-validation node-id (merge current-validation { confidence-score: (if (> total-score u100) u100 total-score) }))
    (ok total-score)
  )
)

(define-public (register-heritage-asset (owner-node-id uint) (name (string-ascii 128)) (description (string-ascii 512)) (asset-type (string-ascii 32)) (estimated-value uint) (currency (string-ascii 8)) (insurance-value uint) (origin-story (string-ascii 512)))
  (let
    (
      (asset-id (+ (var-get last-asset-id) u1))
      (node-owner (unwrap! (nft-get-owner? genealogy-node owner-node-id) ERR-TOKEN-NOT-FOUND))
      (current-assets (default-to (list) (map-get? node-heritage-assets owner-node-id)))
    )
    (asserts! (is-eq tx-sender node-owner) ERR-NOT-TOKEN-OWNER)
    (asserts! (> (len name) u0) ERR-INVALID-ASSET-TYPE)
    (asserts! (or (is-eq asset-type "document") (or (is-eq asset-type "photo") (or (is-eq asset-type "heirloom") (or (is-eq asset-type "jewelry") (is-eq asset-type "artifact"))))) ERR-INVALID-ASSET-TYPE)
    (map-set heritage-assets asset-id {
      name: name,
      description: description,
      asset-type: asset-type,
      owner-node-id: owner-node-id,
      custodian: tx-sender,
      creation-date: stacks-block-height,
      acquisition-date: stacks-block-height,
      estimated-value: estimated-value,
      currency: currency,
      insurance-value: insurance-value,
      is-locked: false,
      access-level: "family"
    })
    (map-set asset-provenance asset-id {
      original-owner: owner-node-id,
      previous-owners: (list),
      transfer-history: (list),
      origin-story: origin-story,
      historical-significance: ""
    })
    (map-set node-heritage-assets owner-node-id 
      (unwrap! (as-max-len? (append current-assets asset-id) u50) ERR-MINT-FAILED))
    (unwrap-panic (update-family-vault owner-node-id estimated-value))
    (var-set last-asset-id asset-id)
    (ok asset-id)
  )
)

(define-public (transfer-asset-custody (asset-id uint) (new-custodian-node-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? heritage-assets asset-id) ERR-ASSET-NOT-FOUND))
      (current-custodian (get custodian asset-data))
      (current-owner-node (get owner-node-id asset-data))
      (new-custodian (unwrap! (nft-get-owner? genealogy-node new-custodian-node-id) ERR-TOKEN-NOT-FOUND))
      (provenance-data (unwrap! (map-get? asset-provenance asset-id) ERR-ASSET-NOT-FOUND))
      (current-transfer-history (get transfer-history provenance-data))
    )
    (asserts! (is-eq tx-sender current-custodian) ERR-NOT-ASSET-OWNER)
    (asserts! (not (get is-locked asset-data)) ERR-ASSET-LOCKED)
    (asserts! (is-family-member current-owner-node new-custodian-node-id) ERR-NOT-FAMILY-MEMBER)
    (map-set heritage-assets asset-id 
      (merge asset-data { 
        custodian: new-custodian,
        acquisition-date: stacks-block-height
      }))
    (map-set asset-provenance asset-id
      (merge provenance-data {
        transfer-history: (unwrap! (as-max-len? 
          (append current-transfer-history { from: current-owner-node, to: new-custodian-node-id, date: stacks-block-height }) 
          u20) ERR-MINT-FAILED)
      }))
    (unwrap-panic (add-asset-to-node new-custodian-node-id asset-id))
    (ok true)
  )
)

(define-public (grant-asset-access (asset-id uint) (target-node-id uint) (can-view bool) (can-edit bool) (can-transfer bool))
  (let
    (
      (asset-data (unwrap! (map-get? heritage-assets asset-id) ERR-ASSET-NOT-FOUND))
      (asset-owner-node (get owner-node-id asset-data))
      (asset-custodian (get custodian asset-data))
      (permission-key { asset-id: asset-id, node-id: target-node-id })
    )
    (asserts! (is-eq tx-sender asset-custodian) ERR-NOT-ASSET-OWNER)
    (asserts! (is-family-member asset-owner-node target-node-id) ERR-NOT-FAMILY-MEMBER)
    (map-set asset-access-permissions permission-key {
      can-view: can-view,
      can-edit: can-edit,
      can-transfer: can-transfer,
      granted-by: asset-owner-node,
      granted-date: stacks-block-height
    })
    (ok true)
  )
)

(define-public (update-asset-valuation (asset-id uint) (new-value uint) (new-insurance-value uint))
  (let
    (
      (asset-data (unwrap! (map-get? heritage-assets asset-id) ERR-ASSET-NOT-FOUND))
      (asset-custodian (get custodian asset-data))
      (owner-node-id (get owner-node-id asset-data))
    )
    (asserts! (is-eq tx-sender asset-custodian) ERR-NOT-ASSET-OWNER)
    (map-set heritage-assets asset-id 
      (merge asset-data { 
        estimated-value: new-value,
        insurance-value: new-insurance-value
      }))
    (unwrap-panic (recalculate-family-vault owner-node-id))
    (ok true)
  )
)

(define-public (lock-asset (asset-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? heritage-assets asset-id) ERR-ASSET-NOT-FOUND))
      (asset-custodian (get custodian asset-data))
    )
    (asserts! (is-eq tx-sender asset-custodian) ERR-NOT-ASSET-OWNER)
    (map-set heritage-assets asset-id (merge asset-data { is-locked: true }))
    (ok true)
  )
)

(define-public (unlock-asset (asset-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? heritage-assets asset-id) ERR-ASSET-NOT-FOUND))
      (asset-custodian (get custodian asset-data))
    )
    (asserts! (is-eq tx-sender asset-custodian) ERR-NOT-ASSET-OWNER)
    (map-set heritage-assets asset-id (merge asset-data { is-locked: false }))
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

(define-read-only (get-dna-profile (node-id uint))
  (map-get? dna-profiles node-id)
)

(define-read-only (get-dna-match (node1-id uint) (node2-id uint))
  (let
    (
      (match-key { node1: node1-id, node2: node2-id })
      (reverse-match-key { node1: node2-id, node2: node1-id })
    )
    (match (map-get? dna-matches match-key)
      some-match (some some-match)
      (map-get? dna-matches reverse-match-key)
    )
  )
)

(define-read-only (get-node-dna-validation (node-id uint))
  (map-get? node-dna-validation node-id)
)

(define-read-only (get-confirmed-matches (node-id uint))
  (get validated-relationships (default-to { has-dna: false, match-count: u0, validated-relationships: (list), confidence-score: u0 } (map-get? node-dna-validation node-id)))
)

(define-read-only (get-dna-confidence-score (node-id uint))
  (match (map-get? node-dna-validation node-id)
    validation (get confidence-score validation)
    u0
  )
)

(define-read-only (has-dna-profile (node-id uint))
  (is-some (map-get? dna-profiles node-id))
)

(define-read-only (get-relationship-strength (node1-id uint) (node2-id uint))
  (match (get-dna-match node1-id node2-id)
    match-data {
      shared-cm: (get shared-cm match-data),
      match-percentage: (get match-percentage match-data),
      relationship-type: (get relationship-type match-data),
      confirmed: (get confirmed match-data)
    }
    {
      shared-cm: u0,
      match-percentage: u0,
      relationship-type: "none",
      confirmed: false
    }
  )
)

(define-read-only (get-heritage-asset (asset-id uint))
  (map-get? heritage-assets asset-id)
)

(define-read-only (get-asset-provenance (asset-id uint))
  (map-get? asset-provenance asset-id)
)

(define-read-only (get-node-assets (node-id uint))
  (default-to (list) (map-get? node-heritage-assets node-id))
)

(define-read-only (get-asset-permissions (asset-id uint) (node-id uint))
  (map-get? asset-access-permissions { asset-id: asset-id, node-id: node-id })
)

(define-read-only (get-family-vault-info (node-id uint))
  (map-get? family-asset-vault node-id)
)

(define-read-only (can-access-asset (asset-id uint) (accessor-node-id uint))
  (let
    (
      (asset-data (map-get? heritage-assets asset-id))
      (permissions (map-get? asset-access-permissions { asset-id: asset-id, node-id: accessor-node-id }))
    )
    (match asset-data
      some-asset (match permissions
        some-perms (get can-view some-perms)
        false
      )
      false
    )
  )
)

(define-read-only (get-asset-custody-chain (asset-id uint))
  (match (map-get? asset-provenance asset-id)
    provenance-data (get transfer-history provenance-data)
    (list)
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

(define-private (get-relationship-requirement (relationship-type (string-ascii 32)))
  (match (map-get? relationship-requirements relationship-type)
    requirement requirement
    { min-cm: u0, min-percentage: u0, max-cm: u10000, max-percentage: u10000 }
  )
)

(define-private (update-dna-validation-counts (node1-id uint) (node2-id uint))
  (let
    (
      (validation1 (unwrap! (map-get? node-dna-validation node1-id) ERR-TOKEN-NOT-FOUND))
      (validation2 (unwrap! (map-get? node-dna-validation node2-id) ERR-TOKEN-NOT-FOUND))
      (new-count1 (+ (get match-count validation1) u1))
      (new-count2 (+ (get match-count validation2) u1))
    )
    (map-set node-dna-validation node1-id (merge validation1 { match-count: new-count1 }))
    (map-set node-dna-validation node2-id (merge validation2 { match-count: new-count2 }))
    (ok true)
  )
)

(define-private (validate-genetic-relationship (node1-id uint) (node2-id uint) (match-data { shared-cm: uint, shared-segments: uint, match-percentage: uint, relationship-type: (string-ascii 32), confirmed: bool, match-date: uint, submitted-by: principal }))
  (let
    (
      (validation1 (unwrap! (map-get? node-dna-validation node1-id) ERR-TOKEN-NOT-FOUND))
      (validation2 (unwrap! (map-get? node-dna-validation node2-id) ERR-TOKEN-NOT-FOUND))
      (current-relationships1 (get validated-relationships validation1))
      (current-relationships2 (get validated-relationships validation2))
    )
    (map-set node-dna-validation node1-id 
      (merge validation1 { 
        validated-relationships: (unwrap! (as-max-len? (append current-relationships1 node2-id) u10) ERR-MINT-FAILED)
      })
    )
    (map-set node-dna-validation node2-id 
      (merge validation2 { 
        validated-relationships: (unwrap! (as-max-len? (append current-relationships2 node1-id) u10) ERR-MINT-FAILED)
      })
    )
    (ok true)
  )
)



(define-private (initialize-relationship-requirements)
  (begin
    (map-set relationship-requirements "parent-child" { min-cm: MIN-PARENT-CHILD-MATCH, min-percentage: u3500, max-cm: u3700, max-percentage: u5500 })
    (map-set relationship-requirements "sibling" { min-cm: MIN-SIBLING-MATCH, min-percentage: u2300, max-cm: u3700, max-percentage: u6100 })
    (map-set relationship-requirements "grandparent" { min-cm: MIN-GRANDPARENT-MATCH, min-percentage: u1200, max-cm: u2000, max-percentage: u2900 })
    (map-set relationship-requirements "cousin" { min-cm: MIN-COUSIN-MATCH, min-percentage: u150, max-cm: u1300, max-percentage: u2300 })
    (ok true)
  )
)

(define-private (is-family-member (node1-id uint) (node2-id uint))
  (let
    (
      (node1-relationships (map-get? family-relationships node1-id))
      (node2-relationships (map-get? family-relationships node2-id))
    )
    (match node1-relationships
      some-rel1 (match node2-relationships
        some-rel2 (or
          (is-eq node1-id node2-id)
          (or
            (is-eq (some node2-id) (get father some-rel1))
            (or
              (is-eq (some node2-id) (get mother some-rel1))
              (or
                (is-some (index-of (get children some-rel1) node2-id))
                (or
                  (is-eq (some node1-id) (get father some-rel2))
                  (or
                    (is-eq (some node1-id) (get mother some-rel2))
                    (is-some (index-of (get children some-rel2) node1-id))
                  )
                )
              )
            )
          )
        )
        false
      )
      false
    )
  )
)

(define-private (update-family-vault (node-id uint) (asset-value uint))
  (let
    (
      (current-vault (default-to { total-assets: u0, total-value: u0, vault-keeper: node-id, last-updated: u0 } (map-get? family-asset-vault node-id)))
      (new-total-assets (+ (get total-assets current-vault) u1))
      (new-total-value (+ (get total-value current-vault) asset-value))
    )
    (map-set family-asset-vault node-id {
      total-assets: new-total-assets,
      total-value: new-total-value,
      vault-keeper: node-id,
      last-updated: stacks-block-height
    })
    (ok true)
  )
)

(define-private (recalculate-family-vault (node-id uint))
  (let
    (
      (node-assets (get-node-assets node-id))
      (total-value (fold calculate-asset-value node-assets u0))
    )
    (map-set family-asset-vault node-id {
      total-assets: (len node-assets),
      total-value: total-value,
      vault-keeper: node-id,
      last-updated: stacks-block-height
    })
    (ok true)
  )
)

(define-private (calculate-asset-value (asset-id uint) (current-total uint))
  (match (map-get? heritage-assets asset-id)
    asset-data (+ current-total (get estimated-value asset-data))
    current-total
  )
)

(define-private (add-asset-to-node (node-id uint) (asset-id uint))
  (let
    (
      (current-assets (default-to (list) (map-get? node-heritage-assets node-id)))
    )
    (map-set node-heritage-assets node-id 
      (unwrap! (as-max-len? (append current-assets asset-id) u50) ERR-MINT-FAILED))
    (ok true)
  )
)

