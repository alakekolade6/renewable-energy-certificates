;; Renewable Energy Certificates Smart Contract
;; Issue renewable energy certificates, track green energy production, and facilitate trading

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_CERTIFICATE_RETIRED (err u105))
(define-constant ERR_INVALID_FACILITY (err u106))
(define-constant ERR_PRODUCTION_NOT_VERIFIED (err u107))

;; Data Variables
(define-data-var next-certificate-id uint u1)
(define-data-var next-facility-id uint u1)
(define-data-var contract-admin principal tx-sender)

;; Data Maps

;; Renewable Energy Facilities
(define-map facilities
  { facility-id: uint }
  {
    owner: principal,
    name: (string-ascii 100),
    energy-type: (string-ascii 50),
    capacity-mw: uint,
    location: (string-ascii 100),
    verified: bool,
    registered-at: uint
  }
)

;; Energy Production Records
(define-map production-records
  { facility-id: uint, period: uint }
  {
    energy-produced-mwh: uint,
    verified: bool,
    verifier: (optional principal),
    timestamp: uint,
    certificates-issued: uint
  }
)

;; Renewable Energy Certificates
(define-map certificates
  { certificate-id: uint }
  {
    facility-id: uint,
    owner: principal,
    energy-amount-mwh: uint,
    issue-date: uint,
    production-period: uint,
    retired: bool,
    retirement-date: (optional uint),
    retirement-reason: (optional (string-ascii 200))
  }
)

;; Certificate Trading Marketplace
(define-map marketplace-listings
  { certificate-id: uint }
  {
    seller: principal,
    price-ustx: uint,
    listed-at: uint,
    active: bool
  }
)

;; Authorized Verifiers
(define-map authorized-verifiers
  { verifier: principal }
  { authorized: bool, authorized-at: uint }
)

;; Certificate Ownership Balance
(define-map certificate-balances
  { owner: principal, facility-id: uint }
  { balance-mwh: uint }
)

;; Private Functions

;; Check if caller is contract admin
(define-private (is-admin (caller principal))
  (is-eq caller (var-get contract-admin))
)

;; Check if verifier is authorized
(define-private (is-authorized-verifier (verifier principal))
  (default-to false (get authorized (map-get? authorized-verifiers { verifier: verifier })))
)

;; Get next certificate ID and increment
(define-private (get-next-certificate-id)
  (let ((current-id (var-get next-certificate-id)))
    (var-set next-certificate-id (+ current-id u1))
    current-id
  )
)

;; Get next facility ID and increment
(define-private (get-next-facility-id)
  (let ((current-id (var-get next-facility-id)))
    (var-set next-facility-id (+ current-id u1))
    current-id
  )
)

;; Validate energy amount
(define-private (is-valid-amount (amount uint))
  (> amount u0)
)

;; Public Functions

;; Register a new renewable energy facility
(define-public (register-facility 
  (name (string-ascii 100))
  (energy-type (string-ascii 50))
  (capacity-mw uint)
  (location (string-ascii 100))
)
  (let ((facility-id (get-next-facility-id)))
    (asserts! (is-valid-amount capacity-mw) ERR_INVALID_AMOUNT)
    (map-set facilities
      { facility-id: facility-id }
      {
        owner: tx-sender,
        name: name,
        energy-type: energy-type,
        capacity-mw: capacity-mw,
        location: location,
        verified: false,
        registered-at: stacks-block-height
      }
    )
    (ok facility-id)
  )
)

;; Verify a renewable energy facility (admin only)
(define-public (verify-facility (facility-id uint))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? facilities { facility-id: facility-id })) ERR_NOT_FOUND)
    (map-set facilities
      { facility-id: facility-id }
      (merge 
        (unwrap-panic (map-get? facilities { facility-id: facility-id }))
        { verified: true }
      )
    )
    (ok true)
  )
)

;; Add authorized verifier (admin only)
(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-admin tx-sender) ERR_UNAUTHORIZED)
    (map-set authorized-verifiers
      { verifier: verifier }
      { authorized: true, authorized-at: stacks-block-height }
    )
    (ok true)
  )
)

;; Record energy production for a facility
(define-public (record-production 
  (facility-id uint)
  (period uint)
  (energy-produced-mwh uint)
)
  (let ((facility (unwrap! (map-get? facilities { facility-id: facility-id }) ERR_NOT_FOUND)))
    (asserts! (is-eq (get owner facility) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (get verified facility) ERR_INVALID_FACILITY)
    (asserts! (is-valid-amount energy-produced-mwh) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? production-records { facility-id: facility-id, period: period })) ERR_ALREADY_EXISTS)
    
    (map-set production-records
      { facility-id: facility-id, period: period }
      {
        energy-produced-mwh: energy-produced-mwh,
        verified: false,
        verifier: none,
        timestamp: stacks-block-height,
        certificates-issued: u0
      }
    )
    (ok true)
  )
)

;; Verify energy production (authorized verifiers only)
(define-public (verify-production 
  (facility-id uint)
  (period uint)
  (verified-amount-mwh uint)
)
  (let ((production (unwrap! (map-get? production-records { facility-id: facility-id, period: period }) ERR_NOT_FOUND)))
    (asserts! (is-authorized-verifier tx-sender) ERR_UNAUTHORIZED)
    (asserts! (<= verified-amount-mwh (get energy-produced-mwh production)) ERR_INVALID_AMOUNT)
    
    (map-set production-records
      { facility-id: facility-id, period: period }
      (merge production {
        verified: true,
        verifier: (some tx-sender),
        energy-produced-mwh: verified-amount-mwh
      })
    )
    (ok true)
  )
)

;; Issue renewable energy certificates for verified production
(define-public (issue-certificate 
  (facility-id uint)
  (period uint)
  (energy-amount-mwh uint)
)
  (let 
    (
      (production (unwrap! (map-get? production-records { facility-id: facility-id, period: period }) ERR_NOT_FOUND))
      (facility (unwrap! (map-get? facilities { facility-id: facility-id }) ERR_NOT_FOUND))
      (certificate-id (get-next-certificate-id))
      (remaining-energy (- (get energy-produced-mwh production) (get certificates-issued production)))
    )
    (asserts! (is-eq (get owner facility) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (get verified production) ERR_PRODUCTION_NOT_VERIFIED)
    (asserts! (is-valid-amount energy-amount-mwh) ERR_INVALID_AMOUNT)
    (asserts! (<= energy-amount-mwh remaining-energy) ERR_INSUFFICIENT_BALANCE)
    
    ;; Create certificate
    (map-set certificates
      { certificate-id: certificate-id }
      {
        facility-id: facility-id,
        owner: tx-sender,
        energy-amount-mwh: energy-amount-mwh,
        issue-date: stacks-block-height,
        production-period: period,
        retired: false,
        retirement-date: none,
        retirement-reason: none
      }
    )
    
    ;; Update production record
    (map-set production-records
      { facility-id: facility-id, period: period }
      (merge production {
        certificates-issued: (+ (get certificates-issued production) energy-amount-mwh)
      })
    )
    
    ;; Update certificate balance
    (let ((current-balance (default-to u0 (get balance-mwh (map-get? certificate-balances { owner: tx-sender, facility-id: facility-id })))))
      (map-set certificate-balances
        { owner: tx-sender, facility-id: facility-id }
        { balance-mwh: (+ current-balance energy-amount-mwh) }
      )
    )
    
    (ok certificate-id)
  )
)

;; Transfer certificate ownership
(define-public (transfer-certificate (certificate-id uint) (recipient principal))
  (let ((certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_NOT_FOUND)))
    (asserts! (is-eq (get owner certificate) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (get retired certificate)) ERR_CERTIFICATE_RETIRED)
    
    ;; Update certificate ownership
    (map-set certificates
      { certificate-id: certificate-id }
      (merge certificate { owner: recipient })
    )
    
    ;; Update balances
    (let 
      (
        (energy-amount (get energy-amount-mwh certificate))
        (facility-id (get facility-id certificate))
        (sender-balance (default-to u0 (get balance-mwh (map-get? certificate-balances { owner: tx-sender, facility-id: facility-id }))))
        (recipient-balance (default-to u0 (get balance-mwh (map-get? certificate-balances { owner: recipient, facility-id: facility-id }))))
      )
      (map-set certificate-balances
        { owner: tx-sender, facility-id: facility-id }
        { balance-mwh: (- sender-balance energy-amount) }
      )
      (map-set certificate-balances
        { owner: recipient, facility-id: facility-id }
        { balance-mwh: (+ recipient-balance energy-amount) }
      )
    )
    
    (ok true)
  )
)

;; Retire certificate
(define-public (retire-certificate (certificate-id uint) (reason (string-ascii 200)))
  (let ((certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_NOT_FOUND)))
    (asserts! (is-eq (get owner certificate) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (get retired certificate)) ERR_CERTIFICATE_RETIRED)
    
    (map-set certificates
      { certificate-id: certificate-id }
      (merge certificate {
        retired: true,
        retirement-date: (some stacks-block-height),
        retirement-reason: (some reason)
      })
    )
    (ok true)
  )
)

;; List certificate for sale
(define-public (list-certificate-for-sale (certificate-id uint) (price-ustx uint))
  (let ((certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_NOT_FOUND)))
    (asserts! (is-eq (get owner certificate) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (get retired certificate)) ERR_CERTIFICATE_RETIRED)
    (asserts! (> price-ustx u0) ERR_INVALID_AMOUNT)
    
    (map-set marketplace-listings
      { certificate-id: certificate-id }
      {
        seller: tx-sender,
        price-ustx: price-ustx,
        listed-at: stacks-block-height,
        active: true
      }
    )
    (ok true)
  )
)

;; Purchase certificate from marketplace
(define-public (purchase-certificate (certificate-id uint))
  (let 
    (
      (listing (unwrap! (map-get? marketplace-listings { certificate-id: certificate-id }) ERR_NOT_FOUND))
      (certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_NOT_FOUND))
    )
    (asserts! (get active listing) ERR_NOT_FOUND)
    (asserts! (not (is-eq (get seller listing) tx-sender)) ERR_UNAUTHORIZED)
    
    ;; Transfer payment
    (try! (stx-transfer? (get price-ustx listing) tx-sender (get seller listing)))
    
    ;; Transfer certificate ownership
    (try! (transfer-certificate certificate-id tx-sender))
    
    ;; Deactivate listing
    (map-set marketplace-listings
      { certificate-id: certificate-id }
      (merge listing { active: false })
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get facility information
(define-read-only (get-facility (facility-id uint))
  (map-get? facilities { facility-id: facility-id })
)

;; Get production record
(define-read-only (get-production-record (facility-id uint) (period uint))
  (map-get? production-records { facility-id: facility-id, period: period })
)

;; Get certificate information
(define-read-only (get-certificate (certificate-id uint))
  (map-get? certificates { certificate-id: certificate-id })
)

;; Get marketplace listing
(define-read-only (get-marketplace-listing (certificate-id uint))
  (map-get? marketplace-listings { certificate-id: certificate-id })
)

;; Get certificate balance for owner and facility
(define-read-only (get-certificate-balance (owner principal) (facility-id uint))
  (default-to u0 (get balance-mwh (map-get? certificate-balances { owner: owner, facility-id: facility-id })))
)

;; Check if verifier is authorized
(define-read-only (is-verifier-authorized (verifier principal))
  (is-authorized-verifier verifier)
)

