(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))

(define-map equipment-inventory
  { equipment-id: uint }
  {
    make: (string-ascii 50),
    model: (string-ascii 50),
    category: (string-ascii 30),
    price: uint,
    dealer-id: principal,
    available: bool
  }
)

(define-map dealer-profiles
  { dealer-id: principal }
  {
    name: (string-ascii 100),
    location: (string-ascii 100),
    total-sales: uint,
    active: bool
  }
)

(define-map service-requests
  { request-id: uint }
  {
    equipment-id: uint,
    customer: principal,
    service-type: (string-ascii 50),
    status: (string-ascii 20),
    dealer-id: principal
  }
)

(define-data-var next-equipment-id uint u1)
(define-data-var next-request-id uint u1)

(define-public (register-dealer (name (string-ascii 100)) (location (string-ascii 100)))
  (if (is-eq tx-sender contract-owner)
    (ok (map-set dealer-profiles
      { dealer-id: tx-sender }
      {
        name: name,
        location: location,
        total-sales: u0,
        active: true
      }
    ))
    err-owner-only
  )
)

(define-public (add-equipment (make (string-ascii 50)) (model (string-ascii 50)) (category (string-ascii 30)) (price uint))
  (let ((equipment-id (var-get next-equipment-id)))
    (begin
      (map-set equipment-inventory
        { equipment-id: equipment-id }
        {
          make: make,
          model: model,
          category: category,
          price: price,
          dealer-id: tx-sender,
          available: true
        }
      )
      (var-set next-equipment-id (+ equipment-id u1))
      (ok equipment-id)
    )
  )
)

(define-public (purchase-equipment (equipment-id uint))
  (let ((equipment (map-get? equipment-inventory { equipment-id: equipment-id })))
    (match equipment
      equipment-data
      (if (get available equipment-data)
        (begin
          (map-set equipment-inventory
            { equipment-id: equipment-id }
            (merge equipment-data { available: false })
          )
          (ok true)
        )
        (err u104)
      )
      err-not-found
    )
  )
)

(define-public (schedule-service (equipment-id uint) (service-type (string-ascii 50)))
  (let ((request-id (var-get next-request-id)))
    (begin
      (map-set service-requests
        { request-id: request-id }
        {
          equipment-id: equipment-id,
          customer: tx-sender,
          service-type: service-type,
          status: "scheduled",
          dealer-id: contract-owner
        }
      )
      (var-set next-request-id (+ request-id u1))
      (ok request-id)
    )
  )
)

(define-read-only (get-equipment (equipment-id uint))
  (map-get? equipment-inventory { equipment-id: equipment-id })
)

(define-read-only (get-dealer (dealer-id principal))
  (map-get? dealer-profiles { dealer-id: dealer-id })
)

(define-read-only (get-service-request (request-id uint))
  (map-get? service-requests { request-id: request-id })
)

