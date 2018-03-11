(require 'smie)
(defvar tuml-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '(
      (id)
      ;;(ids (ids id) (id))
      (id_list (id_list "," id) (id))
      ;;(stereotype "[" idlist "]")
      (package ("model" id namespace_contents "end" "."))
      (namespace_contents (namespace_contents ";" top_level_element)
                          (top_level_element))
      (top_level_element ("class" class_def)
                        ;; ("association" association_def)
                         ("enumeration" enumeration_def)
                         ("exception" exception_def)
                         ("primitive" primitive_def)) ;; lots missing
      (class_def (id ";" feature_decl_list "end"))
      (feature_decl_list (feature_decl_list ";" feature_decl)
                         (feature_decl))
      (feature_decl (feature_type))
      (feature_type (operation_decl)
                    (attribute_decl))
      (operation_decl ("operation" id "(" param_decl_list ")" ":" id)
                      ("operation" id "("  ")" ":" id)
                      ("operation" id "(" param_decl_list ")")
                      ("operation" id "("  ")"))
      (signature ("(" param_decl_list ")" id)
                 ("(" param_decl_list ")"))
      (param_decl_list (param_decl_list "," param_decl)
                       (param_decl))
      (param_decl (id ":" id ":=" id)
                  (id ":" id))
      (enumeration_def ("id" id_list "end"))
      (exception_def (id))
      (primitive_def (id)))
    '((assoc ";") (assoc ":"))
   )))

(defun tuml-indentation-rule (method arg) nil)

;; (defvar tuml-constants
;;   '("reservedword1"
;;     "reservedword2"))

(defvar tuml-keywords
  '(
    "abstract"
    "access"
    "actor"
    "aggregation"
    "alias"
    "allow"
    "all"
    "and"
    "any"
    "anyone"
    "apply"
    "association"
    "association_class"
    "as"
    "attribute"
    "begin"
    "broadcast"
    "by"
    "call"
    "catch"
    "class"
    "component"
    "composition"
    "connector"
    "create"
    "datatype"
    "delete"
    "deny"
    "dependency"
    "derived"
    "destroy"
    "do"
    "else"
    "elseif"
    "end"
    "entry"
    "enumeration"
    "exception"
    "exit"
    "extends"
    "extent"
    "external"
    "false"
    "final"
    "finally"
    "function"
    "id"
    "if"
    "implements"
    "import"
    "in"
    "initial"
    "inout"
    "interface"
    "invariant"
    "is"
    "link"
    "literal"
    "load"
    "model"
    "navigable"
    "new"
    "none"
    "nonunique"
    "not"
    "null"
    "on"
    "operation"
    "opposite"
    "or"
    "ordered"
    "out"
    "package"
    "port"
    "postcondition"
    "precondition"
    "primitive"
    "private"
    "profile"
    "property"
    "protected"
    "provided"
    "public"
    "query"
    "raise"
    "raises"
    "read"
    "readonly"
    "reception"
    "reference"
    "repeat"
    "required"
    "return"
    "role"
    "self"
    "send"
    "signal"
    "specializes"
    "state"
    "statemachine"
    "static"
    "stereotype"
    "subsets"
    "terminate"
    "then"
    "to"
    "transition"
    "true"
    "try"
    "type"
    "unique"
    "unlink"
    "unordered"
    "until"
    "update"
    "var"
    "when"
    "where"
    "while"
    ))

(defvar tuml-tab-width 2 "Width of a tab for TUML mode")

(defvar tuml-font-lock-defaults
  `((
     ;; stuff between double quotes
     ("\"\\.\\*\\?" . font-lock-string-face)
     ("(\\*[\0-\377[:nonascii:]]*?\\*)" . font-lock-doc-face)
     ("/\\*[\0-\377[:nonascii:]]*?\\*/" . font-lock-comment-face)
     ;; ; : , ; { } =>  @ $ = are all special elements
     ;; (":\\|,\\|;\\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
     ( ,(regexp-opt tuml-keywords 'words) . font-lock-keyword-face)
     ;; ( ,(regexp-opt tuml-constants 'words) . font-lock-constant-face)
     )))

(define-derived-mode tuml-mode fundamental-mode "TUML script"
  "TUML mode is a major mode for editing TUML files"

  ;; (smie-setup tuml-smie-grammar 'tuml-indentation-rule)

  (setq font-lock-defaults tuml-font-lock-defaults)

  ;; buffer-local
  (setq indent-tabs-mode t)

  ;; when there's an override, use it
  ;; otherwise it gets the default value
  (when tuml-tab-width
    (setq tab-width tuml-tab-width))

  ;; This is the lazy solution.
  ;; buffer-local
  (setq font-lock-multiline t)

  ;; for comments
  ;; they're made buffer local when you set them
  (setq comment-start "/*")
  (setq comment-end "*/")

  ;; (modify-syntax-entry ?# "< b" tuml-mode-syntax-table)
  ;; (modify-syntax-entry ?\n "> b" tuml-mode-syntax-table)

  ;; Note that there's no need to manually call `tuml-mode-hook'; `define-derived-mode'
  ;; will define `tuml-mode' to call it properly right before it exits
  )

(provide 'tuml-mode)
