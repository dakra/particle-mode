;;; particle.el --- Particle API Client          -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Daniel Kraus

;; Author: Daniel Kraus <daniel@kraus.my>
;; Version: 0.1
;; Package-Requires: ((request "0.3.0") (emacs "24.4"))
;; Keywords: languages tools particle spark wiring ino request api
;; URL: https://github.com/dakra/particle-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode and utils for working with particle (/spark).
;; A lot of the particle major-mode is copied from `arduino-mode'
;; (https://github.com/stardiviner/arduino-mode)

;; TODO: Use particle rest api

;;; Code:


(require 'request)

(eval-when-compile
  (require 'cl-lib)
  (require 'cc-langs)
  (require 'cc-fonts)
  (require 'cc-menus))

(eval-and-compile
  ;; fall back on c-mode
  (c-add-language 'particle-mode 'c-mode))

(defgroup particle nil
  "Particle"
  :prefix "particle-"
  :group 'languages)

(defcustom particle-auth-token nil
  "Your particle api token.
When nil read email from authinfo."
  :type 'string
  :safe #'stringp
  :group 'particle)

(defcustom particle-font-lock-extra-types nil
  "List of extra types (aside from type keywords) to recognize in particle mode.
Each list item should be a regexp matching a single identifier." :group 'particle)

(defcustom particle-executable "particle"
  "The particle executable."
  :group 'particle
  :type 'string)


(defun particle-api-url (model &rest slug)
  "Return particle api url for MODEL with optional SLUG."
  (let ((proto (format "http%s" (if (= particle-api-port 443) "s" "")))
        (domain particle-api-domain)
        (port (if (not (or (= particle-api-port 80) (= particle-api-port 443)))
                  (format ":%s" particle-api-port)
                ""))
        (version particle-api-version)
        (slug (format "%s%s" model
                      (if slug
                          (concat "/" (mapconcat (lambda (s) (format "%s" s)) slug "/"))
                        ""))))
    (format "%s://%s%s/%s/%s" proto domain port version slug)))

;;;###autoload
(defun particle-login (&optional callback api)
  "Login to particle API and call CALLBACK with auth-token."
  (interactive)
  (let* ((particle-api-domain (or api particle-api-domain))
         (auth (auth-source-user-and-password particle-api-domain))
         (email (or particle-email (car auth)))
         (password (or particle-password (cadr auth))))
    (if (and email password)
        (request
         (particle-api-url 'login)
         :type "POST"
         :data (json-encode `(("email" . ,email)
                              ("password" . ,password)))
         :headers '(("User-Agent" . "Particle Emacs Client")
                    ("Accept" . "application/json")
                    ("Content-Type" . "application/json;charset=utf-8"))
         :parser 'json-read
         :success (cl-function
                   (lambda (&key data &allow-other-keys)
                     (let ((_user (cdr (assoc 'user data)))
                           (particle-message (cdr (assoc 'message data)))
                           (auth-token (cdr (assoc 'auth_token data))))
                       (setq particle--auth-token auth-token)
                       (message particle-message)
                       (when callback
                         (funcall callback auth-token)))))
         :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                               (message "Got error %S while getting token" error-thrown))))
      (error "You have to set particle api email and password"))))

(defun particle-logout ()
  "Forget particle auth-token."
  (interactive)
  (setq particle--auth-token nil))


;;;###autoload
(defun particle-get (model &optional success &rest slug)
  "Get particle MODEL with optional SLUG attributes and SUCCESS callback."
  (interactive "sModel to get: ")
  (when (and success (not (functionp success)))
    (setq slug (cons success slug))
    (setq success nil))
  (request
   (apply #'particle-api-url model slug)
   :type "GET"
   :headers `(("User-Agent" . "Particle Emacs Client")
              ("Accept" . "application/json")
              ("Content-Type" . "application/json;charset=utf-8")
              ("Authorization" . ,(format "Bearer %s" particle--auth-token)))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (let* ((resource (cdr (assoc 'resource data)))
                      (model (cdr (assoc-string resource data))))
                 (if success
                     (funcall success model)
                   (message "%s: %S" resource model)))))
   :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "Got error %S while getting model" error-thrown)))))


(c-lang-defconst c-primitive-type-kwds
  particle (append '("boolean" "byte")
                   (c-lang-const c-primitive-type-kwds)))

(c-lang-defconst c-constant-kwds
  particle (append
            '("HIGH" "LOW"
              "INPUT" "OUTPUT" "INPUT_PULLUP" "INPUT_PULLDOWN"
              ;; RGB Colors
              "RGB_COLOR_BLUE"
              "RGB_COLOR_GREEN"
              "RGB_COLOR_CYAN"
              "RGB_COLOR_RED"
              "RGB_COLOR_MAGENTA"
              "RGB_COLOR_YELLOW"
              "RGB_COLOR_WHITE"
              "RGB_COLOR_GRAY"
              "RGB_COLOR_ORANGE"
              ;; Booleans
              "true" "false")
            (c-lang-const c-constant-kwds)))

(c-lang-defconst c-simple-stmt-kwds
  particle (append
            '(;; Input/Outut
              "pinMode"
              "getPinMode"
              "digitalWrite"
              "digitalRead"
              "analogWrite"
              "analogWriteResolution"
              "analogWriteMaxFrequency"
              "analogRead"
              "setADCSampleTime"
              ;; Low Level Input/Output
              "pinSetFast"
              "pinResetFast"
              "digitalWriteFast"
              "pinReadFast"
              ;; Advanced I/O
              "tone"
              "noTone"
              "shiftOut"
              "shiftIn"
              "pulseIn"
              ;; IPAddress
              "IPAddress"
              "IPfromInt"
              "IPfromBytes"
              ;; TCP/UDP
              "TCPServer"
              "TCPClient"
              "UDP"
              ;; Servo
              "Servo"
              ;; LED
              "LEDStatus"
              "LEDPriority"
              "LEDPattern"
              ;; Time
              "millis"
              "micros"
              "delay"
              "delayMicroseconds"
              ;; Interrupts
              "attachInterrupt"
              "detachInterrupt"
              "interrupts"
              "noInterrupts"
              ;; Software Timers
              "Timer"
              ;; Math
              "min"
              "max"
              "abs"
              "constrain"
              "map"
              "pow"
              "sqrt"
              ;; Trigonometry
              "sin"
              "cos"
              "tan"
              ;; Random Numbers
              "random"
              "randomSeed")
            (c-lang-const c-simple-stmt-kwds)))

(c-lang-defconst c-primary-expr-kwds
  particle (append
            '("Particle"
              "WiFi"
              "Serial"
              "SPI"
              "Wire"
              "RGB"
              "Time"
              )
            (c-lang-const c-primary-expr-kwds)))


(defconst particle-font-lock-keywords-1 (c-lang-const c-matchers-1 particle)
  "Minimal highlighting for Particle mode.")

(defconst particle-font-lock-keywords-2 (c-lang-const c-matchers-2 particle)
  "Fast normal highlighting for Particle mode.")

(defconst particle-font-lock-keywords-3 (c-lang-const c-matchers-3 particle)
  "Accurate normal highlighting for Particle mode.")

(defvar particle-font-lock-keywords particle-font-lock-keywords-3
  "Default expressions to highlight in PARTICLE mode.")

(defvar particle-mode-syntax-table nil
  "Syntax table used in particle-mode buffers.")
(or particle-mode-syntax-table
    (setq particle-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table particle))))

(defvar particle-mode-abbrev-table nil
  "Abbreviation table used in particle-mode buffers.")

(c-define-abbrev-table 'particle-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trigger
  ;; reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar particle-mode-map
  (let ((map (c-make-inherited-keymap)))
    ;; Add bindings which are only useful for Particle
    map)
  "Keymap used in particle-mode buffers.")

(easy-menu-define particle-menu particle-mode-map "Particle Mode Commands"
  (cons "Particle" (c-lang-const c-mode-menu particle)))


(define-derived-mode particle-mode prog-mode "Particle"
  "Major mode for editing particle firmware (.ino files)."
  ;; For `cc-mode' initialize.
  (c-initialize-cc-mode t)
  (setq abbrev-mode t)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars particle-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'particle-mode)
  (easy-menu-add particle-menu))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ino\\'" . particle-mode))

(provide 'particle)
;;; particle.el ends here
