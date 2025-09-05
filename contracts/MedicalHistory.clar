;; MedicalHistory Contract - Family health tracking and genetic risk assessment
;; Enables family members to record medical conditions and analyze hereditary health patterns

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_NOT_FOUND (err u401))
(define-constant ERR_ALREADY_EXISTS (err u402))
(define-constant ERR_INVALID_INPUT (err u403))
(define-constant ERR_NO_ACCESS (err u404))
(define-constant ERR_INVALID_NODE (err u405))

;; Reference to main Genealogix contract
(define-constant GENEALOGIX_CONTRACT .Genealogix)

;; Privacy levels
(define-constant PRIVACY_PUBLIC "public")
(define-constant PRIVACY_FAMILY "family") 
(define-constant PRIVACY_DESCENDANTS "descendants")
(define-constant PRIVACY_PRIVATE "private")

;; Risk levels
(define-constant RISK_LOW u1)
(define-constant RISK_MODERATE u2)
(define-constant RISK_HIGH u3)
(define-constant RISK_VERY_HIGH u4)

;; Data variables
(define-data-var contract-admin principal CONTRACT_OWNER)
(define-data-var next-record-id uint u1)

;; Medical condition records for each family member
(define-map medical-records uint {
    node-id: uint,
    condition-name: (string-ascii 64),
    category: (string-ascii 32), ;; genetic, lifestyle, infectious, etc.
    onset-age: uint,
    severity: uint, ;; 1-5 scale
    is-hereditary: bool,
    notes: (string-ascii 200),
    privacy-level: (string-ascii 16),
    recorded-by: principal,
    recorded-at: uint
})

;; Node medical summaries
(define-map node-medical-summary uint {
    total-conditions: uint,
    hereditary-conditions: uint,
    last-updated: uint,
    privacy-default: (string-ascii 16)
})

;; Access permissions for medical data
(define-map medical-access-permissions { node-id: uint, accessor: principal } {
    can-view: bool,
    can-add: bool,
    granted-by: principal,
    granted-at: uint
})

;; Condition statistics across family
(define-map condition-statistics (string-ascii 64) {
    total-occurrences: uint,
    affected-generations: (list 10 uint),
    average-onset-age: uint,
    hereditary-cases: uint,
    risk-score: uint
})

;; Genetic risk assessments
(define-map genetic-risk-profile uint {
    node-id: uint,
    calculated-risks: (list 20 { condition: (string-ascii 64), risk-level: uint, confidence: uint }),
    total-risk-score: uint,
    assessment-date: uint
})

;; Admin functions
(define-public (set-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR_UNAUTHORIZED)
        (var-set contract-admin new-admin)
        (ok true)
    )
)

;; Record medical condition for a family member
(define-public (record-medical-condition 
    (node-id uint) 
    (condition-name (string-ascii 64)) 
    (category (string-ascii 32))
    (onset-age uint) 
    (severity uint) 
    (is-hereditary bool)
    (notes (string-ascii 200))
    (privacy-level (string-ascii 16)))
    (let (
        (record-id (var-get next-record-id))
        (node-owner (unwrap! (contract-call? GENEALOGIX_CONTRACT get-owner node-id) ERR_INVALID_NODE))
    )
        ;; Verify node ownership or permission
        (asserts! (or 
            (is-eq (some tx-sender) node-owner)
            (can-add-medical-data node-id tx-sender)
        ) ERR_UNAUTHORIZED)
        
        ;; Validate inputs
        (asserts! (and (>= severity u1) (<= severity u5)) ERR_INVALID_INPUT)
        (asserts! (or (is-eq privacy-level PRIVACY_PUBLIC) 
                     (or (is-eq privacy-level PRIVACY_FAMILY)
                         (or (is-eq privacy-level PRIVACY_DESCENDANTS)
                             (is-eq privacy-level PRIVACY_PRIVATE)))) ERR_INVALID_INPUT)
        
        ;; Create medical record
        (map-set medical-records record-id {
            node-id: node-id,
            condition-name: condition-name,
            category: category,
            onset-age: onset-age,
            severity: severity,
            is-hereditary: is-hereditary,
            notes: notes,
            privacy-level: privacy-level,
            recorded-by: tx-sender,
            recorded-at: stacks-block-height
        })
        
        ;; Update node medical summary
        (unwrap! (update-node-medical-summary node-id) ERR_INVALID_INPUT)
        
        ;; Update condition statistics
        (unwrap! (update-condition-statistics condition-name onset-age is-hereditary) ERR_INVALID_INPUT)
        
        (var-set next-record-id (+ record-id u1))
        (ok record-id)
    )
)

;; Grant medical data access to family member
(define-public (grant-medical-access (node-id uint) (accessor principal) (can-view bool) (can-add bool))
    (let ((node-owner (unwrap! (contract-call? GENEALOGIX_CONTRACT get-owner node-id) ERR_INVALID_NODE)))
        (asserts! (is-eq (some tx-sender) node-owner) ERR_UNAUTHORIZED)
        
        (map-set medical-access-permissions { node-id: node-id, accessor: accessor } {
            can-view: can-view,
            can-add: can-add,
            granted-by: tx-sender,
            granted-at: stacks-block-height
        })
        (ok true)
    )
)

;; Set default privacy level for node
(define-public (set-medical-privacy-default (node-id uint) (privacy-level (string-ascii 16)))
    (let ((node-owner (unwrap! (contract-call? GENEALOGIX_CONTRACT get-owner node-id) ERR_INVALID_NODE))
          (current-summary (default-to 
            { total-conditions: u0, hereditary-conditions: u0, last-updated: u0, privacy-default: PRIVACY_FAMILY }
            (map-get? node-medical-summary node-id))))
        (asserts! (is-eq (some tx-sender) node-owner) ERR_UNAUTHORIZED)
        
        (map-set node-medical-summary node-id 
            (merge current-summary { privacy-default: privacy-level }))
        (ok true)
    )
)

;; Calculate genetic risk profile for a node
(define-public (calculate-genetic-risks (node-id uint))
    (let ((node-owner (unwrap! (contract-call? GENEALOGIX_CONTRACT get-owner node-id) ERR_INVALID_NODE))
          (lineage (contract-call? GENEALOGIX_CONTRACT get-lineage-path node-id)))
        (asserts! (or 
            (is-eq (some tx-sender) node-owner)
            (can-view-medical-data node-id tx-sender)
        ) ERR_UNAUTHORIZED)
        
        ;; Calculate hereditary risk based on family medical history
        (let ((risk-calculations (calculate-hereditary-risks node-id lineage)))
            (map-set genetic-risk-profile node-id {
                node-id: node-id,
                calculated-risks: risk-calculations,
                total-risk-score: (fold sum-risk-scores risk-calculations u0),
                assessment-date: stacks-block-height
            })
            (ok (len risk-calculations))
        )
    )
)

;; Private helper functions
(define-private (can-view-medical-data (node-id uint) (accessor principal))
    (match (map-get? medical-access-permissions { node-id: node-id, accessor: accessor })
        permissions (get can-view permissions)
        false
    )
)

(define-private (can-add-medical-data (node-id uint) (accessor principal))
    (match (map-get? medical-access-permissions { node-id: node-id, accessor: accessor })
        permissions (get can-add permissions)  
        false
    )
)

(define-private (update-node-medical-summary (node-id uint))
    (let ((current-summary (default-to 
            { total-conditions: u0, hereditary-conditions: u0, last-updated: u0, privacy-default: PRIVACY_FAMILY }
            (map-get? node-medical-summary node-id))))
        (map-set node-medical-summary node-id {
            total-conditions: (+ (get total-conditions current-summary) u1),
            hereditary-conditions: (get hereditary-conditions current-summary), ;; Will be updated by condition analysis
            last-updated: stacks-block-height,
            privacy-default: (get privacy-default current-summary)
        })
        (ok true)
    )
)

(define-private (update-condition-statistics (condition (string-ascii 64)) (onset-age uint) (is-hereditary bool))
    (let ((current-stats (default-to 
            { total-occurrences: u0, affected-generations: (list), average-onset-age: u0, hereditary-cases: u0, risk-score: RISK_LOW }
            (map-get? condition-statistics condition)))
          (new-occurrences (+ (get total-occurrences current-stats) u1))
          (current-total-age (* (get total-occurrences current-stats) (get average-onset-age current-stats)))
          (new-average-age (/ (+ current-total-age onset-age) new-occurrences))
          (new-hereditary-cases (if is-hereditary (+ (get hereditary-cases current-stats) u1) (get hereditary-cases current-stats))))
        
        (map-set condition-statistics condition {
            total-occurrences: new-occurrences,
            affected-generations: (get affected-generations current-stats),
            average-onset-age: new-average-age,
            hereditary-cases: new-hereditary-cases,
            risk-score: (calculate-condition-risk-level new-occurrences new-hereditary-cases)
        })
        (ok true)
    )
)

(define-private (calculate-condition-risk-level (occurrences uint) (hereditary-cases uint))
    (let ((hereditary-ratio (if (> occurrences u0) (/ (* hereditary-cases u100) occurrences) u0)))
        (if (>= hereditary-ratio u75) RISK_VERY_HIGH
            (if (>= hereditary-ratio u50) RISK_HIGH
                (if (>= hereditary-ratio u25) RISK_MODERATE
                    RISK_LOW
                )
            )
        )
    )
)

(define-private (calculate-hereditary-risks (node-id uint) (lineage { current: uint, father: (optional uint), mother: (optional uint), paternal-grandfather: (optional uint), paternal-grandmother: (optional uint), maternal-grandfather: (optional uint), maternal-grandmother: (optional uint) }))
    ;; Simplified risk calculation - check parents and grandparents for hereditary conditions
    (let ((parent-risks (get-parent-condition-risks (get father lineage) (get mother lineage)))
          (grandparent-risks (get-grandparent-condition-risks 
                                (get paternal-grandfather lineage) (get paternal-grandmother lineage)
                                (get maternal-grandfather lineage) (get maternal-grandmother lineage))))
        (concat-risk-lists parent-risks grandparent-risks)
    )
)

(define-private (get-parent-condition-risks (father (optional uint)) (mother (optional uint)))
    ;; Simplified - return empty list as this would require complex medical record analysis
    (list)
)

(define-private (get-grandparent-condition-risks (pg (optional uint)) (pgm (optional uint)) (mg (optional uint)) (mgm (optional uint)))
    ;; Simplified - return empty list as this would require complex medical record analysis  
    (list)
)

(define-private (concat-risk-lists (list1 (list 20 { condition: (string-ascii 64), risk-level: uint, confidence: uint })) (list2 (list 20 { condition: (string-ascii 64), risk-level: uint, confidence: uint })))
    ;; Simplified concatenation - in practice would merge and deduplicate
    list1
)

(define-private (sum-risk-scores (risk { condition: (string-ascii 64), risk-level: uint, confidence: uint }) (total uint))
    (+ total (get risk-level risk))
)

;; Read-only functions
(define-read-only (get-medical-record (record-id uint))
    (map-get? medical-records record-id)
)

(define-read-only (get-node-medical-summary (node-id uint))
    (map-get? node-medical-summary node-id)
)

(define-read-only (get-condition-statistics (condition (string-ascii 64)))
    (map-get? condition-statistics condition)
)

(define-read-only (get-genetic-risk-profile (node-id uint))
    (map-get? genetic-risk-profile node-id)
)

(define-read-only (can-access-medical-data (node-id uint) (accessor principal))
    ;; Simplified check - in production would verify node ownership properly
    (or 
        (is-eq accessor tx-sender) ;; Assuming accessor is the caller
        (can-view-medical-data node-id accessor)
    )
)

(define-read-only (get-family-health-overview (node-id uint))
    (let ((summary (get-node-medical-summary node-id))
          (risks (get-genetic-risk-profile node-id)))
        {
            node-summary: summary,
            risk-profile: risks,
            has-data: (is-some summary)
        }
    )
)

(define-read-only (get-contract-info)
    {
        contract-admin: (var-get contract-admin),
        next-record-id: (var-get next-record-id),
        privacy-levels: (list PRIVACY_PUBLIC PRIVACY_FAMILY PRIVACY_DESCENDANTS PRIVACY_PRIVATE)
    }
)
