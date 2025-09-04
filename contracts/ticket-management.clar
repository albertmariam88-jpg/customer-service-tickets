;; Customer Service Ticket Management Smart Contract
;; Provides issue tracking, response time monitoring, and satisfaction measurement

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_TICKET_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INVALID_PRIORITY (err u103))
(define-constant ERR_INVALID_RATING (err u104))
(define-constant ERR_TICKET_CLOSED (err u105))

;; Data structures
(define-map tickets 
  { ticket-id: uint }
  {
    customer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    status: uint, ;; 0: open, 1: in-progress, 2: resolved, 3: closed
    priority: uint, ;; 1: low, 2: medium, 3: high, 4: critical
    category: (string-ascii 50),
    created-at: uint,
    updated-at: uint,
    assigned-agent: (optional principal),
    response-time: (optional uint),
    resolution-time: (optional uint),
    satisfaction-rating: (optional uint) ;; 1-5 scale
  }
)

(define-map customer-tickets
  { customer: principal }
  { ticket-count: uint, total-tickets: uint }
)

(define-map agent-stats
  { agent: principal }
  {
    assigned-tickets: uint,
    resolved-tickets: uint,
    avg-response-time: uint,
    avg-resolution-time: uint,
    satisfaction-score: uint
  }
)

;; Data variables
(define-data-var next-ticket-id uint u1)
(define-data-var total-tickets-created uint u0)
(define-data-var total-tickets-resolved uint u0)

;; Private functions
(define-private (is-valid-status (status uint))
  (and (<= status u3) (>= status u0))
)

(define-private (is-valid-priority (priority uint))
  (and (<= priority u4) (>= priority u1))
)

(define-private (is-valid-rating (rating uint))
  (and (<= rating u5) (>= rating u1))
)

(define-private (update-agent-stats (agent principal) (response-time uint) (resolution-time uint) (rating uint))
  (let (
    (current-stats (default-to 
      { assigned-tickets: u0, resolved-tickets: u0, avg-response-time: u0, avg-resolution-time: u0, satisfaction-score: u0 }
      (map-get? agent-stats { agent: agent })
    ))
    (new-resolved (+ (get resolved-tickets current-stats) u1))
  )
    (map-set agent-stats { agent: agent }
      {
        assigned-tickets: (get assigned-tickets current-stats),
        resolved-tickets: new-resolved,
        avg-response-time: (/ (+ (* (get avg-response-time current-stats) (get resolved-tickets current-stats)) response-time) new-resolved),
        avg-resolution-time: (/ (+ (* (get avg-resolution-time current-stats) (get resolved-tickets current-stats)) resolution-time) new-resolved),
        satisfaction-score: (/ (+ (* (get satisfaction-score current-stats) (get resolved-tickets current-stats)) rating) new-resolved)
      }
    )
  )
)

;; Public functions
(define-public (create-ticket (title (string-ascii 100)) (description (string-ascii 500)) (priority uint) (category (string-ascii 50)))
  (begin
    (asserts! (is-valid-priority priority) ERR_INVALID_PRIORITY)
    (let (
      (ticket-id (var-get next-ticket-id))
      (current-time stacks-block-height)
    )
      (map-set tickets { ticket-id: ticket-id }
        {
          customer: tx-sender,
          title: title,
          description: description,
          status: u0, ;; open
          priority: priority,
          category: category,
          created-at: current-time,
          updated-at: current-time,
          assigned-agent: none,
          response-time: none,
          resolution-time: none,
          satisfaction-rating: none
        }
      )
      ;; Update customer ticket count
      (let (
        (customer-data (default-to { ticket-count: u0, total-tickets: u0 } 
                                   (map-get? customer-tickets { customer: tx-sender })))
      )
        (map-set customer-tickets { customer: tx-sender }
          {
            ticket-count: (+ (get ticket-count customer-data) u1),
            total-tickets: (+ (get total-tickets customer-data) u1)
          }
        )
      )
      (var-set next-ticket-id (+ ticket-id u1))
      (var-set total-tickets-created (+ (var-get total-tickets-created) u1))
      (ok ticket-id)
    )
  )
)

(define-public (assign-ticket (ticket-id uint) (agent principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (match (map-get? tickets { ticket-id: ticket-id })
      ticket-data
      (begin
        (asserts! (< (get status ticket-data) u2) ERR_TICKET_CLOSED)
        (map-set tickets { ticket-id: ticket-id }
          (merge ticket-data {
            assigned-agent: (some agent),
            status: u1, ;; in-progress
            updated-at: stacks-block-height,
            response-time: (if (is-none (get response-time ticket-data))
                             (some (- stacks-block-height (get created-at ticket-data)))
                             (get response-time ticket-data))
          })
        )
        ;; Update agent stats
        (let (
          (agent-data (default-to 
            { assigned-tickets: u0, resolved-tickets: u0, avg-response-time: u0, avg-resolution-time: u0, satisfaction-score: u0 }
            (map-get? agent-stats { agent: agent })
          ))
        )
          (map-set agent-stats { agent: agent }
            (merge agent-data { assigned-tickets: (+ (get assigned-tickets agent-data) u1) })
          )
        )
        (ok true)
      )
      ERR_TICKET_NOT_FOUND
    )
  )
)

(define-public (update-ticket-status (ticket-id uint) (new-status uint))
  (begin
    (asserts! (is-valid-status new-status) ERR_INVALID_STATUS)
    (match (map-get? tickets { ticket-id: ticket-id })
      ticket-data
      (begin
        (asserts! (or (is-eq tx-sender CONTRACT_OWNER) 
                     (is-eq (some tx-sender) (get assigned-agent ticket-data))) ERR_UNAUTHORIZED)
        (map-set tickets { ticket-id: ticket-id }
          (merge ticket-data {
            status: new-status,
            updated-at: stacks-block-height,
            resolution-time: (if (and (is-eq new-status u2) (is-none (get resolution-time ticket-data)))
                               (some (- stacks-block-height (get created-at ticket-data)))
                               (get resolution-time ticket-data))
          })
        )
        (if (is-eq new-status u2)
          (begin
            (var-set total-tickets-resolved (+ (var-get total-tickets-resolved) u1))
            ;; Update customer ticket count
            (let (
              (customer-data (unwrap-panic (map-get? customer-tickets { customer: (get customer ticket-data) })))
            )
              (map-set customer-tickets { customer: (get customer ticket-data) }
                (merge customer-data { ticket-count: (- (get ticket-count customer-data) u1) })
              )
            )
          )
          true
        )
        (ok true)
      )
      ERR_TICKET_NOT_FOUND
    )
  )
)

(define-public (rate-ticket (ticket-id uint) (rating uint))
  (begin
    (asserts! (is-valid-rating rating) ERR_INVALID_RATING)
    (match (map-get? tickets { ticket-id: ticket-id })
      ticket-data
      (begin
        (asserts! (is-eq tx-sender (get customer ticket-data)) ERR_UNAUTHORIZED)
        (asserts! (>= (get status ticket-data) u2) ERR_INVALID_STATUS) ;; Must be resolved or closed
        (map-set tickets { ticket-id: ticket-id }
          (merge ticket-data {
            satisfaction-rating: (some rating),
            updated-at: stacks-block-height
          })
        )
        ;; Update agent satisfaction score if agent is assigned
        (match (get assigned-agent ticket-data)
          agent
          (update-agent-stats agent 
                             (unwrap-panic (get response-time ticket-data))
                             (unwrap-panic (get resolution-time ticket-data))
                             rating)
          true
        )
        (ok true)
      )
      ERR_TICKET_NOT_FOUND
    )
  )
)

;; Read-only functions
(define-read-only (get-ticket (ticket-id uint))
  (map-get? tickets { ticket-id: ticket-id })
)

(define-read-only (get-customer-stats (customer principal))
  (map-get? customer-tickets { customer: customer })
)

(define-read-only (get-agent-stats (agent principal))
  (map-get? agent-stats { agent: agent })
)

(define-read-only (get-total-tickets)
  (var-get total-tickets-created)
)

(define-read-only (get-resolved-tickets)
  (var-get total-tickets-resolved)
)

(define-read-only (get-next-ticket-id)
  (var-get next-ticket-id)
)

(define-read-only (get-resolution-rate)
  (if (> (var-get total-tickets-created) u0)
    (/ (* (var-get total-tickets-resolved) u100) (var-get total-tickets-created))
    u0
  )
)


;; title: ticket-management
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

