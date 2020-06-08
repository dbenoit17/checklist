checklist
======
An extensible checklist language.


### Installation
On Fedora:
```
sudo dnf install racket
raco pkg install https://github.com/dbenoit17/checklist
```

### Basic Usage
```
#!/usr/bin/racket
#lang checklist

;; This is a comment.
;; checklists contain zero or more checklist-items.
;; checklist-items have three components:
;;    - status field: `[<x>]`
;;    - name field:   `<string>\n`
;;    - description field (optional): `<string>\n ...`

;; The next entry is denoted by the presence of the status field.

;; List of rooms to clean
[x] living room
[x] kitchen
    don't forget to take out the trash
[ ] dining room

```
Use the `@-form` to call functions.

```
#!/usr/bin/racket
#lang checklist

;; List of rooms to clean
[x] living room
[x] kitchen
[ ] dining room

@print-checklist-incomplete

;; prints:
;; [ ] dining room
```

Use key-value `%-tags` in the description field to annotate checklist items.
```
#!/usr/bin/racket
#lang checklist

;; %-tags are arbitrary key-value pairs which can be used
;; to annotate item descriptions.  Users can define
;; functions to operate on checklist items based on
;; %-tags.  %due{<date>} is an example tag used by the 
;; checklist/date module.

;; @require{} is a special form in Checklist that
;; imports modules.

@require{checklist/date}

[ ] task 1
    %due{2020-Mar-12}
    the first task

[ ] task 2
    %due{2020-Mar-13}
    the second task

[ ] task 3
    %due{2020-Mar-10}
    the third task

[ ] task 4
    %due{2020-Feb-12}
    the fourth task

;; Inline racket code can be written with Checklist's @racket{} form

@racket{
  (define sorted-checklist (checklist-sort-due))
}

;; Sort checklist items using %tags.

@print-checklist-items{sorted-checklist}

```

### Embedding Racket Subroutines

Users can define Racket functions to accept any arbitrary key-value `%-tags`.

```
#lang checklist

@require{checklist/date}

;; Grocery list

[x] Tomatoes
    %bought{2020-May-20} %shelf-days{7}
[x] Potatos
    %bought{2020-May-20} %shelf-days{21}
[x] Lettuce
    %bought{2020-May-20} %shelf-days{8}
[x] Onions
    %bought{2020-May-20} %shelf-days{30}

@racket{
  (define today "2020-Jun-8")

  (define (expired? item)
    (define day-bought (car (checklist-item-key-ref item 'bought)))
    (define shelf-days (car (checklist-item-key-ref item 'shelf-days)))
    (define expiration (date-plus-days day-bought shelf-days))
    (date< expiration today))

  (define (print-expired-items)
    (for ([item (current-checklist)])
      (when (expired? item)
        (print-checklist-item item))))
}

@print-expired-items

;;  prints:
;; 
;; [x] Tomatoes
;;     %bought{2020-May-20} %shelf-days{7} 
;; 
;; [x] Lettuce
;;     %bought{2020-May-20} %shelf-days{8} 
```

