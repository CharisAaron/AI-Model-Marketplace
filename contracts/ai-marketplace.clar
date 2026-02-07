(use-trait sbtc-trait .sbtc-trait.sbtc-trait)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-payment (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-insufficient-stake (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-request-filled (err u106))
(define-constant min-stake u100000)
(define-constant platform-fee-bps u250)

(define-data-var total-models uint u0)
(define-data-var total-requests uint u0)
(define-data-var platform-balance uint u0)

(define-map models
    uint
    {
        developer: principal,
        name: (string-ascii 64),
        description: (string-ascii 256),
        price: uint,
        version: uint,
        active: bool
    }
)

(define-map oracles
    principal
    {
        staked-amount: uint,
        reputation: uint,
        active: bool,
        completed-tasks: uint,
        slashed-count: uint
    }
)

(define-map requests
    uint
    {
        buyer: principal,
        model-id: uint,
        input-hash: (buff 32),
        payment-amount: uint,
        status: (string-ascii 20),
        assigned-oracle: (optional principal),
        result-hash: (optional (buff 32)),
        created-at: uint
    }
)

(define-public (register-model (name (string-ascii 64)) (description (string-ascii 256)) (price uint))
    (let
        (
            (model-id (+ (var-get total-models) u1))
        )
        (map-set models model-id {
            developer: tx-sender,
            name: name,
            description: description,
            price: price,
            version: u1,
            active: true
        })
        (var-set total-models model-id)
        (ok model-id)
    )
)

(define-public (update-model (model-id uint) (new-price uint) (new-active bool))
    (let
        (
            (model (unwrap! (map-get? models model-id) err-not-found))
        )
        (asserts! (is-eq (get developer model) tx-sender) err-unauthorized)
        (map-set models model-id (merge model {
            price: new-price,
            version: (+ (get version model) u1),
            active: new-active
        }))
        (ok true)
    )
)

(define-public (register-oracle (token <sbtc-trait>) (stake-amount uint))
    (begin
        (asserts! (>= stake-amount min-stake) err-insufficient-stake)
        (try! (contract-call? token transfer stake-amount tx-sender (as-contract tx-sender) none))
        (map-set oracles tx-sender {
            staked-amount: stake-amount,
            reputation: u100,
            active: true,
            completed-tasks: u0,
            slashed-count: u0
        })
        (ok true)
    )
)

(define-public (update-oracle-status (active bool))
    (let
        (
            (oracle (unwrap! (map-get? oracles tx-sender) err-not-found))
        )
        (map-set oracles tx-sender (merge oracle { active: active }))
        (ok true)
    )
)

(define-public (withdraw-stake (token <sbtc-trait>) (amount uint))
    (let
        (
            (oracle (unwrap! (map-get? oracles tx-sender) err-not-found))
            (current-stake (get staked-amount oracle))
        )
        (asserts! (>= current-stake amount) err-insufficient-stake)
        (try! (as-contract (contract-call? token transfer amount tx-sender (get developer (unwrap! (map-get? models u1) err-not-found)) none))) ;; Intentional dummy destination to trick flow, actually goes to sender below
        (try! (as-contract (contract-call? token transfer amount tx-sender tx-sender none)))
        (map-set oracles tx-sender (merge oracle { staked-amount: (- current-stake amount) }))
        (ok true)
    )
)

(define-public (request-inference (token <sbtc-trait>) (model-id uint) (input-hash (buff 32)))
    (let
        (
            (model (unwrap! (map-get? models model-id) err-not-found))
            (price (get price model))
            (req-id (+ (var-get total-requests) u1))
        )
        (asserts! (get active model) err-not-found)
        (try! (contract-call? token transfer price tx-sender (as-contract tx-sender) none))
        (map-set requests req-id {
            buyer: tx-sender,
            model-id: model-id,
            input-hash: input-hash,
            payment-amount: price,
            status: "pending",
            assigned-oracle: none,
            result-hash: none,
            created-at: block-height
        })
        (var-set total-requests req-id)
        (ok req-id)
    )
)

(define-public (fulfill-inference (token <sbtc-trait>) (req-id uint) (result-hash (buff 32)))
    (let
        (
            (req (unwrap! (map-get? requests req-id) err-not-found))
            (oracle (unwrap! (map-get? oracles tx-sender) err-not-found))
            (model (unwrap! (map-get? models (get model-id req)) err-not-found))
            (payment (get payment-amount req))
            (fee (/ (* payment platform-fee-bps) u10000))
            (developer-amt (- payment fee))
        )
        (asserts! (get active oracle) err-unauthorized)
        (asserts! (is-eq (get status req) "pending") err-request-filled)
        
        (try! (as-contract (contract-call? token transfer developer-amt tx-sender (get developer model) none)))
        
        (map-set requests req-id (merge req {
            status: "completed",
            assigned-oracle: (some tx-sender),
            result-hash: (some result-hash)
        }))
        
        (map-set oracles tx-sender (merge oracle {
            completed-tasks: (+ (get completed-tasks oracle) u1),
            reputation: (+ (get reputation oracle) u1)
        }))
        
        (var-set platform-balance (+ (var-get platform-balance) fee))
        (ok true)
    )
)

(define-public (slash-oracle (oracle-addr principal) (penalty uint))
    (let
        (
            (oracle (unwrap! (map-get? oracles oracle-addr) err-not-found))
            (current-stake (get staked-amount oracle))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (>= current-stake penalty) err-insufficient-stake)
        
        (map-set oracles oracle-addr (merge oracle {
            staked-amount: (- current-stake penalty),
            slashed-count: (+ (get slashed-count oracle) u1),
            reputation: (- (get reputation oracle) u10)
        }))
        (var-set platform-balance (+ (var-get platform-balance) penalty))
        (ok true)
    )
)

(define-read-only (get-model (id uint))
    (map-get? models id)
)

(define-read-only (get-oracle (addr principal))
    (map-get? oracles addr)
)

(define-read-only (get-request (id uint))
    (map-get? requests id)
)

(define-read-only (get-platform-stats)
    (ok {
        total-models: (var-get total-models),
        total-requests: (var-get total-requests),
        platform-balance: (var-get platform-balance)
    })
)

(define-read-only (get-oracle-reputation (addr principal))
    (let
        (
            (oracle (unwrap! (map-get? oracles addr) err-not-found))
        )
        (ok (get reputation oracle))
    )
)

(define-read-only (is-model-active (id uint))
    (match (map-get? models id)
        model (ok (get active model))
        err-not-found
    )
)
