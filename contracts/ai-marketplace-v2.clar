(use-trait sbtc-trait .sbtc-trait.sbtc-trait)

(define-constant contract-owner tx-sender)
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
(define-constant err-model-deprecated (err u112))
(define-constant err-version-not-found (err u113))
(define-constant err-invalid-split (err u114))
(define-constant err-oracle-still-active (err u115))
(define-constant err-not-whitelisted (err u116))
(define-constant min-stake u100000)
(define-constant platform-fee-bps u250)
(define-constant request-expiry-blocks u144)
(define-constant oracle-idle-blocks u2016)
(define-constant liveness-bounty-bps u100)

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
        deprecated: bool,
        private: bool,
        rating-sum: uint,
        rating-count: uint,
    }
)

(define-map model-versions
    { model-id: uint, version: uint }
    {
        price: uint,
        description: (string-ascii 256),
        deprecated: bool,
        deprecated-at: (optional uint),
    }
)

(define-map model-splits
    uint
    {
        collab-a: (optional principal),
        share-a: uint,
        collab-b: (optional principal),
        share-b: uint,
        collab-c: (optional principal),
        share-c: uint,
    }
)

(define-map model-whitelist
    { model-id: uint, buyer: principal }
    bool
)

(define-map oracles
    principal
    {
        staked-amount: uint,
        reputation: uint,
        active: bool,
        completed-tasks: uint,
        slashed-count: uint,
        last-active-block: uint,
    }
)

(define-map model-ratings
    uint
    uint
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
        pinned-version: (optional uint),
    }
)

(define-public (register-model
        (name (string-ascii 64))
        (description (string-ascii 256))
        (price uint)
        (private bool)
    )
    (let ((model-id (+ (var-get total-models) u1)))
        (map-set models model-id {
            developer: tx-sender,
            name: name,
            description: description,
            price: price,
            version: u1,
            active: true,
            deprecated: false,
            private: private,
            rating-sum: u0,
            rating-count: u0,
        })
        (map-set model-versions { model-id: model-id, version: u1 } {
            price: price,
            description: description,
            deprecated: false,
            deprecated-at: none,
        })
        (var-set total-models model-id)
        (ok model-id)
    )
)

(define-public (register-model-with-splits
        (name (string-ascii 64))
        (description (string-ascii 256))
        (price uint)
        (private bool)
        (collab-a (optional principal))
        (share-a uint)
        (collab-b (optional principal))
        (share-b uint)
        (collab-c (optional principal))
        (share-c uint)
    )
    (let (
            (model-id (+ (var-get total-models) u1))
            (total-shares (+ (+ share-a share-b) share-c))
        )
        (asserts! (< total-shares u10000) err-invalid-split)
        (map-set models model-id {
            developer: tx-sender,
            name: name,
            description: description,
            price: price,
            version: u1,
            active: true,
            deprecated: false,
            private: private,
            rating-sum: u0,
            rating-count: u0,
        })
        (map-set model-versions { model-id: model-id, version: u1 } {
            price: price,
            description: description,
            deprecated: false,
            deprecated-at: none,
        })
        (map-set model-splits model-id {
            collab-a: collab-a,
            share-a: share-a,
            collab-b: collab-b,
            share-b: share-b,
            collab-c: collab-c,
            share-c: share-c,
        })
        (var-set total-models model-id)
        (ok model-id)
    )
)

(define-public (set-model-privacy (model-id uint) (private bool))
    (let ((model (unwrap! (map-get? models model-id) err-not-found)))
        (asserts! (is-eq (get developer model) tx-sender) err-unauthorized)
        (map-set models model-id (merge model { private: private }))
        (ok true)
    )
)

(define-public (set-whitelist-entry
        (model-id uint)
        (buyer principal)
        (approved bool)
    )
    (let ((model (unwrap! (map-get? models model-id) err-not-found)))
        (asserts! (is-eq (get developer model) tx-sender) err-unauthorized)
        (map-set model-whitelist { model-id: model-id, buyer: buyer } approved)
        (ok true)
    )
)

(define-public (update-model
        (model-id uint)
        (new-price uint)
        (new-active bool)
    )
    (let (
            (model (unwrap! (map-get? models model-id) err-not-found))
            (next-version (+ (get version model) u1))
        )
        (asserts! (is-eq (get developer model) tx-sender) err-unauthorized)
        (asserts! (not (get deprecated model)) err-model-deprecated)
        (map-set models model-id
            (merge model {
                price: new-price,
                version: next-version,
                active: new-active,
            })
        )
        (map-set model-versions { model-id: model-id, version: next-version } {
            price: new-price,
            description: (get description model),
            deprecated: false,
            deprecated-at: none,
        })
        (ok next-version)
    )
)

(define-public (deprecate-model-version
        (model-id uint)
        (version uint)
    )
    (let (
            (model (unwrap! (map-get? models model-id) err-not-found))
            (ver (unwrap! (map-get? model-versions { model-id: model-id, version: version }) err-version-not-found))
        )
        (asserts! (is-eq (get developer model) tx-sender) err-unauthorized)
        (asserts! (not (get deprecated ver)) err-already-exists)
        (map-set model-versions { model-id: model-id, version: version }
            (merge ver {
                deprecated: true,
                deprecated-at: (some block-height),
            })
        )
        (if (is-eq version (get version model))
            (map-set models model-id (merge model { deprecated: true, active: false }))
            true
        )
        (ok true)
    )
)

(define-public (register-oracle
        (token <sbtc-trait>)
        (stake-amount uint)
    )
    (begin
        (asserts! (>= stake-amount min-stake) err-insufficient-stake)
        (try! (contract-call? token transfer stake-amount tx-sender
            (as-contract tx-sender) none
        ))
        (map-set oracles tx-sender {
            staked-amount: stake-amount,
            reputation: u100,
            active: true,
            completed-tasks: u0,
            slashed-count: u0,
            last-active-block: block-height,
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

(define-public (withdraw-stake
        (token <sbtc-trait>)
        (amount uint)
    )
    (let (
            (oracle (unwrap! (map-get? oracles tx-sender) err-not-found))
            (current-stake (get staked-amount oracle))
        )
        (asserts! (>= current-stake amount) err-insufficient-stake)
        (try! (as-contract (contract-call? token transfer amount tx-sender tx-sender none)))
        (map-set oracles tx-sender
            (merge oracle { staked-amount: (- current-stake amount) })
        )
        (ok true)
    )
)

(define-public (flag-idle-oracle
        (token <sbtc-trait>)
        (oracle-addr principal)
    )
    (let (
            (oracle (unwrap! (map-get? oracles oracle-addr) err-not-found))
            (idle-since (get last-active-block oracle))
            (current-stake (get staked-amount oracle))
            (bounty (/ (* current-stake liveness-bounty-bps) u10000))
        )
        (asserts! (> block-height (+ idle-since oracle-idle-blocks)) err-oracle-still-active)
        (asserts! (get active oracle) err-oracle-still-active)
        (asserts! (>= current-stake bounty) err-insufficient-stake)
        (try! (as-contract (contract-call? token transfer bounty tx-sender tx-sender none)))
        (map-set oracles oracle-addr
            (merge oracle {
                active: false,
                staked-amount: (- current-stake bounty),
                slashed-count: (+ (get slashed-count oracle) u1),
            })
        )
        (ok bounty)
    )
)

(define-public (request-inference
        (token <sbtc-trait>)
        (model-id uint)
        (input-hash (buff 32))
        (pin-version (optional uint))
    )
    (let (
            (model (unwrap! (map-get? models model-id) err-not-found))
            (price (get price model))
            (req-id (+ (var-get total-requests) u1))
        )
        (asserts! (get active model) err-not-found)
        (asserts! (not (get deprecated model)) err-model-deprecated)
        (if (get private model)
            (asserts! (default-to false (map-get? model-whitelist { model-id: model-id, buyer: tx-sender })) err-not-whitelisted)
            true
        )
        (match pin-version
            v (let ((ver (unwrap! (map-get? model-versions { model-id: model-id, version: v }) err-version-not-found)))
                (asserts! (not (get deprecated ver)) err-model-deprecated)
                true
              )
            true
        )
        (try! (contract-call? token transfer price tx-sender (as-contract tx-sender) none))
        (map-set requests req-id {
            buyer: tx-sender,
            model-id: model-id,
            input-hash: input-hash,
            payment-amount: price,
            status: "pending",
            assigned-oracle: none,
            result-hash: none,
            created-at: block-height,
            pinned-version: pin-version,
        })
        (var-set total-requests req-id)
        (ok req-id)
    )
)

(define-private (disburse-if-some
        (token <sbtc-trait>)
        (recipient (optional principal))
        (amount uint)
    )
    (match recipient
        addr (as-contract (contract-call? token transfer amount tx-sender addr none))
        (ok true)
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
            (fee (/ (* payment platform-fee-bps) u10000))
            (developer-pool (- payment fee))
        )
        (asserts! (get active oracle) err-unauthorized)
        (asserts! (is-eq (get status req) "pending") err-request-filled)
        (match (map-get? model-splits (get model-id req))
            s (let (
                    (amt-a (/ (* developer-pool (get share-a s)) u10000))
                    (amt-b (/ (* developer-pool (get share-b s)) u10000))
                    (amt-c (/ (* developer-pool (get share-c s)) u10000))
                    (dev-amt (- (- (- developer-pool amt-a) amt-b) amt-c))
                )
                (try! (as-contract (contract-call? token transfer dev-amt tx-sender (get developer model) none)))
                (try! (disburse-if-some token (get collab-a s) amt-a))
                (try! (disburse-if-some token (get collab-b s) amt-b))
                (try! (disburse-if-some token (get collab-c s) amt-c))
              )
            (try! (as-contract (contract-call? token transfer developer-pool tx-sender (get developer model) none)))
        )
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
                last-active-block: block-height,
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
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
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

(define-public (withdraw-platform-fees
        (token <sbtc-trait>)
        (amount uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= amount (var-get platform-balance)) err-invalid-amount)
        (try! (as-contract (contract-call? token transfer amount tx-sender contract-owner none)))
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
        (asserts! (> block-height (+ (get created-at req) request-expiry-blocks))
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
        (asserts! (not (get deprecated model)) err-model-deprecated)
        (map-set models model-id
            (merge model {
                name: new-name,
                description: new-description,
            })
        )
        (ok true)
    )
)

(define-read-only (get-model (id uint))
    (map-get? models id)
)

(define-read-only (get-model-version (model-id uint) (version uint))
    (map-get? model-versions { model-id: model-id, version: version })
)

(define-read-only (get-model-splits (model-id uint))
    (map-get? model-splits model-id)
)

(define-read-only (is-buyer-whitelisted (model-id uint) (buyer principal))
    (ok (default-to false (map-get? model-whitelist { model-id: model-id, buyer: buyer })))
)

(define-read-only (get-oracle (addr principal))
    (map-get? oracles addr)
)

(define-read-only (is-oracle-idle (addr principal))
    (match (map-get? oracles addr)
        oracle (ok (> block-height (+ (get last-active-block oracle) oracle-idle-blocks)))
        err-not-found
    )
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

(define-read-only (is-version-deprecated (model-id uint) (version uint))
    (match (map-get? model-versions { model-id: model-id, version: version })
        ver (ok (get deprecated ver))
        err-version-not-found
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