
(use-trait sbtc-trait .sbtc-trait.sbtc-trait)
(define-data-var contract-owner principal tx-sender)
(define-data-var pending-owner (optional principal) none)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-payment (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-insufficient-stake (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-request-filled (err u106))

(define-constant err-invalid-rating (err u107))
(define-constant err-already-rated (err u108))
(define-constant err-request-not-completed (err u109))
(define-constant err-invalid-amount (err u110))
(define-constant err-request-not-expired (err u111))
(define-constant err-oracle-not-active (err u112))
(define-constant err-unstake-not-requested (err u113))
(define-constant err-cooldown-active (err u114))
(define-constant err-already-disputed (err u115))
(define-constant err-not-disputed (err u116))
(define-constant err-invalid-governance-param (err u117))
(define-constant err-low-reputation (err u118))

(define-data-var min-stake uint u100000)
(define-data-var platform-fee-bps uint u250)
(define-data-var request-expiry-blocks uint u144)
(define-data-var unstake-cooldown-blocks uint u144)

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
        active: bool,
        rating-sum: uint,
        rating-count: uint,
        min-reputation: uint,
    }
)

(define-map oracles
    principal
    {
        staked-amount: uint,
        reputation: uint,
        active: bool,
        completed-tasks: uint,
        slashed-count: uint,
        unstake-requested-at: (optional uint),
    }
)

(define-map model-ratings
    uint ;; request-id
    uint ;; rating
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
        created-at: uint,
    }
)

(define-map disputes
    uint ;; request-id
    {
        flagged-by: principal,
        flagged-at: uint,
        resolved: bool,
    }
)

(define-public (register-model
        (name (string-ascii 64))
        (description (string-ascii 256))
        (price uint)
        (min-reputation uint)
    )
    (let ((model-id (+ (var-get total-models) u1)))
        (map-set models model-id {
            developer: tx-sender,
            name: name,
            description: description,
            price: price,
            version: u1,
            active: true,
            rating-sum: u0,
            rating-count: u0,
            min-reputation: min-reputation,
        })
        (var-set total-models model-id)
        (ok model-id)
    )
)

(define-public (update-model
        (model-id uint)
        (new-price uint)
        (new-active bool)
        (new-min-reputation uint)
    )
    (let ((model (unwrap! (map-get? models model-id) err-not-found)))
        (asserts! (is-eq (get developer model) tx-sender) err-unauthorized)
        (map-set models model-id
            (merge model {
                price: new-price,
                version: (+ (get version model) u1),
                active: new-active,
                rating-sum: (get rating-sum model),
                rating-count: (get rating-count model),
                min-reputation: new-min-reputation,
            })
        )
        (ok true)
    )
)

(define-public (register-oracle
        (token <sbtc-trait>)
        (stake-amount uint)
    )
    (begin
        (asserts! (>= stake-amount (var-get min-stake)) err-insufficient-stake)
        (try! (contract-call? token transfer stake-amount tx-sender
            (as-contract tx-sender) none
        ))
        (map-set oracles tx-sender {
            staked-amount: stake-amount,
            reputation: u100,
            active: true,
            completed-tasks: u0,
            slashed-count: u0,
            unstake-requested-at: none,
        })
        (ok true)
    )
)

(define-public (update-oracle-status (active bool))
    (let ((oracle (unwrap! (map-get? oracles tx-sender) err-not-found)))
        (map-set oracles tx-sender (merge oracle { active: active }))
        (ok true)
    )
)

(define-public (request-unstake)
    (let ((oracle (unwrap! (map-get? oracles tx-sender) err-not-found)))
        (map-set oracles tx-sender
            (merge oracle { unstake-requested-at: (some block-height) })
        )
        (ok true)
    )
)

(define-public (withdraw-stake
        (token <sbtc-trait>)
        (amount uint)
    )
    (let (
            (oracle (unwrap! (map-get? oracles tx-sender) err-not-found))
            (current-stake (get staked-amount oracle))
            (requested-at (unwrap! (get unstake-requested-at oracle) err-unstake-not-requested))
        )
        (asserts! (>= current-stake amount) err-insufficient-stake)
        (asserts!
            (> block-height (+ requested-at (var-get unstake-cooldown-blocks)))
            err-cooldown-active
        )

        (try! (as-contract (contract-call? token transfer amount tx-sender tx-sender none)))

        (map-set oracles tx-sender
            (merge oracle {
                staked-amount: (- current-stake amount),
                unstake-requested-at: none,
            })
        )
        (ok true)
    )
)

(define-public (request-inference
        (token <sbtc-trait>)
        (model-id uint)
        (input-hash (buff 32))
    )
    (let (
            (model (unwrap! (map-get? models model-id) err-not-found))
            (price (get price model))
            (req-id (+ (var-get total-requests) u1))
        )
        (asserts! (get active model) err-not-found)
        (try! (contract-call? token transfer price tx-sender (as-contract tx-sender)
            none
        ))
        (map-set requests req-id {
            buyer: tx-sender,
            model-id: model-id,
            input-hash: input-hash,
            payment-amount: price,
            status: "pending",
            assigned-oracle: none,
            result-hash: none,
            created-at: block-height,
        })
        (var-set total-requests req-id)
        (ok req-id)
    )
)

(define-public (fulfill-inference
        (token <sbtc-trait>)
        (req-id uint)
        (result-hash (buff 32))
    )
    (let (
            (req (unwrap! (map-get? requests req-id) err-not-found))
            (oracle (unwrap! (map-get? oracles tx-sender) err-not-found))
            (model (unwrap! (map-get? models (get model-id req)) err-not-found))
            (payment (get payment-amount req))
            (fee (/ (* payment (var-get platform-fee-bps)) u10000))
            (developer-amt (- payment fee))
        )
        (asserts! (get active oracle) err-unauthorized)
        (asserts! (>= (get reputation oracle) (get min-reputation model))
            err-low-reputation
        )
        (asserts! (is-eq (get status req) "pending") err-request-filled)

        (try! (as-contract (contract-call? token transfer developer-amt tx-sender
            (get developer model) none
        )))

        (map-set requests req-id
            (merge req {
                status: "completed",
                assigned-oracle: (some tx-sender),
                result-hash: (some result-hash),
            })
        )

        (map-set oracles tx-sender
            (merge oracle {
                completed-tasks: (+ (get completed-tasks oracle) u1),
                reputation: (+ (get reputation oracle) u1),
            })
        )

        (var-set platform-balance (+ (var-get platform-balance) fee))
        (ok true)
    )
)

(define-public (slash-oracle
        (oracle-addr principal)
        (penalty uint)
    )
    (let (
            (oracle (unwrap! (map-get? oracles oracle-addr) err-not-found))
            (current-stake (get staked-amount oracle))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (asserts! (>= current-stake penalty) err-insufficient-stake)

        (map-set oracles oracle-addr
            (merge oracle {
                staked-amount: (- current-stake penalty),
                slashed-count: (+ (get slashed-count oracle) u1),
                reputation: (- (get reputation oracle) u10),
            })
        )
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
        platform-balance: (var-get platform-balance),
    })
)

(define-read-only (get-oracle-reputation (addr principal))
    (let ((oracle (unwrap! (map-get? oracles addr) err-not-found)))
        (ok (get reputation oracle))
    )
)

(define-read-only (is-model-active (id uint))
    (match (map-get? models id)
        model (ok (get active model))
        err-not-found
    )
)

(define-public (rate-model
        (req-id uint)
        (rating uint)
    )
    (let (
            (req (unwrap! (map-get? requests req-id) err-not-found))
            (model-id (get model-id req))
            (model (unwrap! (map-get? models model-id) err-not-found))
        )
        (asserts! (is-eq (get buyer req) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status req) "completed") err-request-not-completed)
        (asserts! (is-none (map-get? model-ratings req-id)) err-already-rated)
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)

        (map-set models model-id
            (merge model {
                rating-sum: (+ (get rating-sum model) rating),
                rating-count: (+ (get rating-count model) u1),
            })
        )
        (map-set model-ratings req-id rating)
        (ok true)
    )
)

(define-read-only (get-model-rating (id uint))
    (match (map-get? models id)
        model (ok {
            rating-sum: (get rating-sum model),
            rating-count: (get rating-count model),
        })
        err-not-found
    )
)

(define-public (withdraw-platform-fees
        (token <sbtc-trait>)
        (amount uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (asserts! (<= amount (var-get platform-balance)) err-invalid-amount)
        (try! (as-contract (contract-call? token transfer amount tx-sender (var-get contract-owner)
            none
        )))
        (var-set platform-balance (- (var-get platform-balance) amount))
        (ok true)
    )
)

(define-public (refund-stale-request
        (token <sbtc-trait>)
        (req-id uint)
    )
    (let (
            (req (unwrap! (map-get? requests req-id) err-not-found))
            (payment (get payment-amount req))
        )
        (asserts! (is-eq (get buyer req) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status req) "pending") err-request-filled)
        (asserts!
            (> block-height
                (+ (get created-at req) (var-get request-expiry-blocks))
            )
            err-request-not-expired
        )

        (try! (as-contract (contract-call? token transfer payment tx-sender (get buyer req) none)))

        (map-set requests req-id (merge req { status: "expired" }))
        (ok true)
    )
)

(define-public (update-model-metadata
        (model-id uint)
        (new-name (string-ascii 64))
        (new-description (string-ascii 256))
    )
    (let ((model (unwrap! (map-get? models model-id) err-not-found)))
        (asserts! (is-eq (get developer model) tx-sender) err-unauthorized)
        (map-set models model-id
            (merge model {
                name: new-name,
                description: new-description,
                version: (+ (get version model) u1),
            })
        )
        (ok true)
    )
)

(define-public (request-inference-from-oracle
        (token <sbtc-trait>)
        (model-id uint)
        (input-hash (buff 32))
        (target-oracle principal)
    )
    (let (
            (model (unwrap! (map-get? models model-id) err-not-found))
            (oracle (unwrap! (map-get? oracles target-oracle) err-not-found))
            (price (get price model))
            (req-id (+ (var-get total-requests) u1))
        )
        (asserts! (get active model) err-not-found)
        (asserts! (get active oracle) err-oracle-not-active)
        (asserts! (>= (get reputation oracle) (get min-reputation model))
            err-low-reputation
        )

        (try! (contract-call? token transfer price tx-sender (as-contract tx-sender)
            none
        ))

        (map-set requests req-id {
            buyer: tx-sender,
            model-id: model-id,
            input-hash: input-hash,
            payment-amount: price,
            status: "pending",
            assigned-oracle: (some target-oracle),
            result-hash: none,
            created-at: block-height,
        })
        (var-set total-requests req-id)
        (ok req-id)
    )
)

(define-public (flag-dispute (req-id uint))
    (let ((req (unwrap! (map-get? requests req-id) err-not-found)))
        (asserts! (is-eq (get buyer req) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status req) "completed") err-request-not-completed)
        (asserts! (is-none (map-get? disputes req-id)) err-already-disputed)

        (map-set disputes req-id {
            flagged-by: tx-sender,
            flagged-at: block-height,
            resolved: false,
        })

        (map-set requests req-id (merge req { status: "disputed" }))
        (ok true)
    )
)

(define-public (resolve-dispute
        (req-id uint)
        (in-favor-of-buyer bool)
    )
    (let (
            (req (unwrap! (map-get? requests req-id) err-not-found))
            (dispute (unwrap! (map-get? disputes req-id) err-not-disputed))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (asserts! (is-eq (get status req) "disputed") err-not-disputed)
        (asserts! (is-eq (get resolved dispute) false) err-already-disputed)

        (map-set disputes req-id (merge dispute { resolved: true }))

        (if in-favor-of-buyer
            (map-set requests req-id (merge req { status: "refunded" }))
            (map-set requests req-id (merge req { status: "completed" }))
        )
        (ok true)
    )
)

(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (var-set pending-owner (some new-owner))
        (ok true)
    )
)

(define-public (claim-ownership)
    (let ((pending (unwrap! (var-get pending-owner) err-unauthorized)))
        (asserts! (is-eq tx-sender pending) err-unauthorized)
        (var-set contract-owner pending)
        (var-set pending-owner none)
        (ok true)
    )
)

(define-private (request-inference-iter
        (input-hash (buff 32))
        (ctx {
            id: uint,
            model-id: uint,
            price: uint,
            buyer: principal,
        })
    )
    (let ((next-id (+ (get id ctx) u1)))
        (map-set requests next-id {
            buyer: (get buyer ctx),
            model-id: (get model-id ctx),
            input-hash: input-hash,
            payment-amount: (get price ctx),
            status: "pending",
            assigned-oracle: none,
            result-hash: none,
            created-at: block-height,
        })
        (merge ctx { id: next-id })
    )
)

(define-public (request-inference-bulk
        (token <sbtc-trait>)
        (model-id uint)
        (input-hashes (list 20 (buff 32)))
    )
    (let (
            (model (unwrap! (map-get? models model-id) err-not-found))
            (price (get price model))
            (count (len input-hashes))
            (total-cost (* price count))
            (start-id (var-get total-requests))
            (ctx {
                id: start-id,
                model-id: model-id,
                price: price,
                buyer: tx-sender,
            })
        )
        (asserts! (get active model) err-not-found)
        (try! (contract-call? token transfer total-cost tx-sender
            (as-contract tx-sender) none
        ))

        (let ((final-ctx (fold request-inference-iter input-hashes ctx)))
            (var-set total-requests (get id final-ctx))
            (ok (get id final-ctx))
        )
    )
)
