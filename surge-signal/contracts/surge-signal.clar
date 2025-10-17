;; Surge Signal - Dynamic Liquid Delegation Governance
;; A DAO governance platform with expertise-weighted voting

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-voted (err u102))
(define-constant err-invalid-proposal (err u103))
(define-constant err-insufficient-power (err u104))
(define-constant err-invalid-delegation (err u105))

;; Data Variables
(define-data-var proposal-nonce uint u0)

;; Proposal categories
(define-constant category-treasury u1)
(define-constant category-technical u2)
(define-constant category-governance u3)
(define-constant category-general u4)

;; Data Maps
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 256),
    category: uint,
    votes-for: uint,
    votes-against: uint,
    end-block: uint,
    executed: bool,
    active: bool
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, power: uint }
)

(define-map delegations
  { delegator: principal, category: uint }
  { delegate: principal, percentage: uint }
)

(define-map token-balance
  { owner: principal }
  { balance: uint }
)

(define-map reputation-scores
  { user: principal, category: uint }
  { score: uint, decisions: uint }
)

(define-map user-expertise
  { user: principal }
  { total-score: uint }
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-delegation (delegator principal) (category uint))
  (map-get? delegations { delegator: delegator, category: category })
)

(define-read-only (get-token-balance (owner principal))
  (default-to { balance: u0 }
    (map-get? token-balance { owner: owner }))
)

(define-read-only (get-reputation (user principal) (category uint))
  (default-to { score: u100, decisions: u0 }
    (map-get? reputation-scores { user: user, category: category }))
)

(define-read-only (get-voting-power (voter principal) (category uint))
  (let (
    (balance (get balance (get-token-balance voter)))
    (reputation (get score (get-reputation voter category)))
  )
    ;; Voting power = balance * (reputation / 100)
    (/ (* balance reputation) u100)
  )
)

;; Public functions

;; Initialize token balance (simplified for demo)
(define-public (mint-tokens (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let (
      (current-balance (get balance (get-token-balance recipient)))
    )
      (ok (map-set token-balance
        { owner: recipient }
        { balance: (+ current-balance amount) }
      ))
    )
  )
)

;; Create a new proposal
(define-public (create-proposal (title (string-ascii 256)) (category uint) (duration uint))
  (let (
    (proposal-id (var-get proposal-nonce))
  )
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        category: category,
        votes-for: u0,
        votes-against: u0,
        end-block: (+ block-height duration),
        executed: false,
        active: true
      }
    )
    (var-set proposal-nonce (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Delegate voting power for a specific category
(define-public (delegate-votes (delegate principal) (category uint) (percentage uint))
  (begin
    (asserts! (<= percentage u100) err-invalid-delegation)
    (ok (map-set delegations
      { delegator: tx-sender, category: category }
      { delegate: delegate, percentage: percentage }
    ))
  )
)

;; Remove delegation
(define-public (remove-delegation (category uint))
  (ok (map-delete delegations { delegator: tx-sender, category: category }))
)

;; Cast a vote on a proposal
(define-public (cast-vote (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-not-found))
    (voter-balance (get balance (get-token-balance tx-sender)))
    (category (get category proposal))
    (voting-power (get-voting-power tx-sender category))
  )
    (asserts! (get active proposal) err-invalid-proposal)
    (asserts! (< block-height (get end-block proposal)) err-invalid-proposal)
    (asserts! (is-none (get-vote proposal-id tx-sender)) err-already-voted)
    (asserts! (> voting-power u0) err-insufficient-power)
    
    ;; Record the vote
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-for, power: voting-power }
    )
    
    ;; Update proposal vote counts
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal {
        votes-for: (if vote-for 
          (+ (get votes-for proposal) voting-power)
          (get votes-for proposal)),
        votes-against: (if vote-for
          (get votes-against proposal)
          (+ (get votes-against proposal) voting-power))
      })
    )
    
    (ok true)
  )
)

;; Cast vote as delegate
(define-public (cast-delegated-vote (proposal-id uint) (vote-for bool) (delegator principal))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-not-found))
    (category (get category proposal))
    (delegation-info (unwrap! (get-delegation delegator category) err-not-found))
    (delegator-balance (get balance (get-token-balance delegator)))
    (reputation (get score (get-reputation tx-sender category)))
    (delegation-percentage (get percentage delegation-info))
  )
    (asserts! (is-eq tx-sender (get delegate delegation-info)) err-owner-only)
    (asserts! (get active proposal) err-invalid-proposal)
    (asserts! (< block-height (get end-block proposal)) err-invalid-proposal)
    (asserts! (is-none (get-vote proposal-id delegator)) err-already-voted)
    
    (let (
      (delegated-power (/ (* delegator-balance delegation-percentage) u100))
      (weighted-power (/ (* delegated-power reputation) u100))
    )
      ;; Record the vote
      (map-set votes
        { proposal-id: proposal-id, voter: delegator }
        { vote: vote-for, power: weighted-power }
      )
      
      ;; Update proposal vote counts
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal {
          votes-for: (if vote-for 
            (+ (get votes-for proposal) weighted-power)
            (get votes-for proposal)),
          votes-against: (if vote-for
            (get votes-against proposal)
            (+ (get votes-against proposal) weighted-power))
        })
      )
      
      (ok true)
    )
  )
)

;; Execute proposal (simplified)
(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) err-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= block-height (get end-block proposal)) err-invalid-proposal)
    (asserts! (not (get executed proposal)) err-invalid-proposal)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) err-invalid-proposal)
    
    (ok (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true, active: false })
    ))
  )
)

;; Update reputation score
(define-public (update-reputation (user principal) (category uint) (score-delta int))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let (
      (current-rep (get-reputation user category))
      (current-score (get score current-rep))
      (new-score (if (>= score-delta 0)
        (+ current-score (to-uint score-delta))
        (if (> current-score (to-uint (* score-delta -1)))
          (- current-score (to-uint (* score-delta -1)))
          u0)))
    )
      (ok (map-set reputation-scores
        { user: user, category: category }
        { 
          score: new-score,
          decisions: (+ (get decisions current-rep) u1)
        }
      ))
    )
  )
)

;; Initialize contract
(begin
  (var-set proposal-nonce u0)
)