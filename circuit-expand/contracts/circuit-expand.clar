;; Circuit Expand - Tournament Management Smart Contract
;; A blockchain gaming ecosystem for competitive tournaments with circuit expansion

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-stake (err u103))
(define-constant err-tournament-full (err u104))
(define-constant err-not-participant (err u105))
(define-constant err-tournament-active (err u106))
(define-constant err-tournament-ended (err u107))
(define-constant err-invalid-winner (err u108))

;; Data Variables
(define-data-var tournament-nonce uint u0)
(define-data-var min-stake-amount uint u1000000) ;; 1 STX in microstacks

;; Data Maps
(define-map tournaments
    uint
    {
        creator: principal,
        name: (string-ascii 50),
        stake-amount: uint,
        prize-pool: uint,
        max-participants: uint,
        current-participants: uint,
        circuit-level: uint,
        is-active: bool,
        winner: (optional principal)
    }
)

(define-map tournament-participants
    {tournament-id: uint, player: principal}
    {
        joined-at: uint,
        skill-rating: uint,
        is-active: bool
    }
)

(define-map player-stats
    principal
    {
        total-tournaments: uint,
        wins: uint,
        total-earnings: uint,
        skill-rating: uint,
        circuit-level: uint
    }
)

(define-map circuit-unlocks
    {player: principal, circuit-level: uint}
    {
        unlocked-at: uint,
        tournaments-completed: uint
    }
)

;; Read-only functions
(define-read-only (get-tournament (tournament-id uint))
    (map-get? tournaments tournament-id)
)

(define-read-only (get-player-stats (player principal))
    (default-to
        {
            total-tournaments: u0,
            wins: u0,
            total-earnings: u0,
            skill-rating: u1000,
            circuit-level: u1
        }
        (map-get? player-stats player)
    )
)

(define-read-only (get-participant-info (tournament-id uint) (player principal))
    (map-get? tournament-participants {tournament-id: tournament-id, player: player})
)

(define-read-only (is-circuit-unlocked (player principal) (circuit-level uint))
    (is-some (map-get? circuit-unlocks {player: player, circuit-level: circuit-level}))
)

(define-read-only (get-min-stake)
    (var-get min-stake-amount)
)

;; Public functions

;; Create a new tournament
(define-public (create-tournament (name (string-ascii 50)) (stake-amount uint) (max-participants uint) (circuit-level uint))
    (let
        (
            (tournament-id (+ (var-get tournament-nonce) u1))
            (creator-stats (get-player-stats tx-sender))
        )
        ;; Verify creator has unlocked this circuit level
        (asserts! (>= (get circuit-level creator-stats) circuit-level) err-not-found)
        (asserts! (>= stake-amount (var-get min-stake-amount)) err-insufficient-stake)
        
        ;; Create tournament
        (map-set tournaments tournament-id
            {
                creator: tx-sender,
                name: name,
                stake-amount: stake-amount,
                prize-pool: u0,
                max-participants: max-participants,
                current-participants: u0,
                circuit-level: circuit-level,
                is-active: true,
                winner: none
            }
        )
        
        (var-set tournament-nonce tournament-id)
        (ok tournament-id)
    )
)

;; Join a tournament
(define-public (join-tournament (tournament-id uint))
    (let
        (
            (tournament (unwrap! (get-tournament tournament-id) err-not-found))
            (stake-amount (get stake-amount tournament))
            (player-stats-data (get-player-stats tx-sender))
        )
        ;; Verify tournament is active and not full
        (asserts! (get is-active tournament) err-tournament-ended)
        (asserts! (< (get current-participants tournament) (get max-participants tournament)) err-tournament-full)
        (asserts! (is-none (get-participant-info tournament-id tx-sender)) err-already-exists)
        (asserts! (>= (get circuit-level player-stats-data) (get circuit-level tournament)) err-not-found)
        
        ;; Transfer stake to contract
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        
        ;; Add participant
        (map-set tournament-participants
            {tournament-id: tournament-id, player: tx-sender}
            {
                joined-at: block-height,
                skill-rating: (get skill-rating player-stats-data),
                is-active: true
            }
        )
        
        ;; Update tournament
        (map-set tournaments tournament-id
            (merge tournament
                {
                    current-participants: (+ (get current-participants tournament) u1),
                    prize-pool: (+ (get prize-pool tournament) stake-amount)
                }
            )
        )
        
        (ok true)
    )
)

;; Declare tournament winner and distribute prizes
(define-public (declare-winner (tournament-id uint) (winner principal))
    (let
        (
            (tournament (unwrap! (get-tournament tournament-id) err-not-found))
            (participant (unwrap! (get-participant-info tournament-id winner) err-invalid-winner))
            (prize-amount (get prize-pool tournament))
            (winner-stats (get-player-stats winner))
        )
        ;; Only tournament creator can declare winner
        (asserts! (is-eq tx-sender (get creator tournament)) err-owner-only)
        (asserts! (get is-active tournament) err-tournament-ended)
        (asserts! (get is-active participant) err-not-participant)
        
        ;; Transfer prize to winner
        (try! (as-contract (stx-transfer? prize-amount tx-sender winner)))
        
        ;; Update tournament
        (map-set tournaments tournament-id
            (merge tournament
                {
                    is-active: false,
                    winner: (some winner)
                }
            )
        )
        
        ;; Update winner stats
        (map-set player-stats winner
            {
                total-tournaments: (+ (get total-tournaments winner-stats) u1),
                wins: (+ (get wins winner-stats) u1),
                total-earnings: (+ (get total-earnings winner-stats) prize-amount),
                skill-rating: (+ (get skill-rating winner-stats) u50),
                circuit-level: (get circuit-level winner-stats)
            }
        )
        
        ;; Check if winner should unlock new circuit
        (check-circuit-unlock winner)
        
        (ok true)
    )
)

;; Internal function to check and unlock new circuits
(define-private (check-circuit-unlock (player principal))
    (let
        (
            (stats (get-player-stats player))
            (current-level (get circuit-level stats))
            (wins (get wins stats))
        )
        ;; Unlock next circuit level after 3 wins in current level
        (if (and (>= wins (* current-level u3)) (is-none (map-get? circuit-unlocks {player: player, circuit-level: (+ current-level u1)})))
            (begin
                (map-set circuit-unlocks
                    {player: player, circuit-level: (+ current-level u1)}
                    {
                        unlocked-at: block-height,
                        tournaments-completed: (get total-tournaments stats)
                    }
                )
                (map-set player-stats player
                    (merge stats {circuit-level: (+ current-level u1)})
                )
                true
            )
            false
        )
    )
)

;; Admin function to update minimum stake
(define-public (set-min-stake (new-amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set min-stake-amount new-amount)
        (ok true)
    )
)

;; Initialize player stats (optional, for new players)
(define-public (initialize-player)
    (begin
        (asserts! (is-none (map-get? player-stats tx-sender)) err-already-exists)
        (map-set player-stats tx-sender
            {
                total-tournaments: u0,
                wins: u0,
                total-earnings: u0,
                skill-rating: u1000,
                circuit-level: u1
            }
        )
        (map-set circuit-unlocks
            {player: tx-sender, circuit-level: u1}
            {
                unlocked-at: block-height,
                tournaments-completed: u0
            }
        )
        (ok true)
    )
)
